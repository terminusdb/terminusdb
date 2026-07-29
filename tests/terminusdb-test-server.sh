#!/bin/bash

# terminusdb-test-server.sh
# Manages a local TerminusDB server for testing with a clean temporary storage

set -e

export TERMINUSDB_LOG_LEVEL=${TERMINUSDB_LOG_LEVEL:-DEBUG}
export TERMINUSDB_LOG_FORMAT=${TERMINUSDB_LOG_FORMAT:-text}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Use port-specific PID/storage/log files when a non-default port is set,
# so multiple TerminusDB instances (e.g. 6363 and 7373) don't conflict.
_TDB_PORT="${TERMINUSDB_SERVER_PORT:-6363}"
if [ "$_TDB_PORT" = "6363" ]; then
    PID_FILE="$SCRIPT_DIR/.terminusdb-test.pid"
    STORAGE_DIR="$SCRIPT_DIR/.terminusdb-test-storage"
    LOG_FILE="$SCRIPT_DIR/.terminusdb-test.log"
else
    PID_FILE="$SCRIPT_DIR/.terminusdb-test-${_TDB_PORT}.pid"
    STORAGE_DIR="$SCRIPT_DIR/.terminusdb-test-storage-${_TDB_PORT}"
    LOG_FILE="$SCRIPT_DIR/.terminusdb-test-${_TDB_PORT}.log"
fi

# Default admin password (terminusdb default)
ADMIN_PASS="${TERMINUSDB_ADMIN_PASS:-root}"

function start_server() {
    local clean_storage=false
    while [ "$1" != "" ]; do
        case "$1" in
            --clean) clean_storage=true ;;
        esac
        shift
    done

    # Check if our managed server is already running
    if [ -f "$PID_FILE" ]; then
        local pid=$(cat "$PID_FILE")
        if ps -p "$pid" > /dev/null 2>&1; then
            echo "TerminusDB test server is already running (PID: $pid)"
            return 0
        else
            echo "Stale PID file found. Cleaning up..."
            rm -f "$PID_FILE"
        fi
    fi

    # The server now uses a single port. The backend is selected by
    # TERMINUSDB_SERVER_BACKEND (default: rust).
    local SERVER_PORT=${TERMINUSDB_SERVER_PORT:-6363}
    local SERVER_BACKEND=${TERMINUSDB_SERVER_BACKEND:-rust}
    if lsof -Pi :$SERVER_PORT -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "ERROR: Port $SERVER_PORT is already in use by another process:"
        lsof -Pi :$SERVER_PORT -sTCP:LISTEN
        echo ""
        echo "Stop the conflicting process, or run this script with stop/clean."
        return 1
    fi

    echo "Starting TerminusDB test server..."
    
    # Clean storage only if explicitly requested
    if [ "$clean_storage" = true ]; then
        if [ -d "$STORAGE_DIR" ]; then
            echo "Cleaning up previous test storage..."
            rm -rf "$STORAGE_DIR"
        fi
        mkdir -p "$STORAGE_DIR"
        
        # Initialize the storage
        echo "Initializing database storage..."
        cd "$PROJECT_ROOT"
        export TERMINUSDB_SERVER_DB_PATH="$STORAGE_DIR"
        ./terminusdb store init --key root
        if [ $? -ne 0 ]; then
            echo "ERROR: Failed to initialize database storage"
            return 1
        fi
    else
        # Create storage directory if it doesn't exist
        if [ ! -d "$STORAGE_DIR" ]; then
            mkdir -p "$STORAGE_DIR"
            echo "Initializing database storage..."
            cd "$PROJECT_ROOT"
            export TERMINUSDB_SERVER_DB_PATH="$STORAGE_DIR"
            ./terminusdb store init --key root
            if [ $? -ne 0 ]; then
                echo "ERROR: Failed to initialize database storage"
                return 1
            fi
        fi
    fi
    
    # Build if binary doesn't exist or rust sources changed
    if [ ! -f "$PROJECT_ROOT/terminusdb" ] || [ "$PROJECT_ROOT/src/rust" -nt "$PROJECT_ROOT/terminusdb" ]; then
        echo "Building TerminusDB binary..."
        cd "$PROJECT_ROOT"
        make rust && make dev
    fi
    
    # Start server in background
    cd "$PROJECT_ROOT"
    export TERMINUSDB_SERVER_NAME=127.0.0.1
    export TERMINUSDB_ADMIN_PASS="$ADMIN_PASS"
    export TERMINUSDB_SERVER_DB_PATH="$STORAGE_DIR"
    # Load the appserver plugin (starts the Rust webserver) and the example
    # webserver plugins. By default, auto-optimize is NOT loaded because it
    # runs optimization probabilistically after commits and can break
    # data-version tests. Set TERMINUSDB_ENABLE_OPTIMIZER=1 to enable it.
    export TERMINUSDB_ADDON_PATH="$PROJECT_ROOT"
    export TERMINUSDB_PLUGINS_PATH="$PROJECT_ROOT/plugins"
    if [ "${TERMINUSDB_ENABLE_OPTIMIZER:-0}" = "1" ]; then
        echo "Optimizer plugin enabled (TERMINUSDB_ENABLE_OPTIMIZER=1)"
        cp "$PROJECT_ROOT/docker/plugins/auto-optimize.pl" "$PROJECT_ROOT/plugins/auto-optimize.pl"
    else
        rm -f "$PROJECT_ROOT/plugins/auto-optimize.pl"
    fi
    # Single-port server: TERMINUSDB_SERVER_BACKEND selects the implementation
    # (swipl or rust). TERMINUSDB_SERVER_PORT sets the listen port (default 6363).
    export TERMINUSDB_SERVER_PORT=${TERMINUSDB_SERVER_PORT:-6363}
    export TERMINUSDB_SERVER_BACKEND=${TERMINUSDB_SERVER_BACKEND:-rust}

    # tdb-search plugin: disabled by default. Set TERMINUSDB_INDEXER_BACKEND=http_tdb_search
    # and TERMINUSDB_TDB_SEARCH_ENDPOINT=http://127.0.0.1:7372 to enable.
    export TERMINUSDB_INDEXER_BACKEND=${TERMINUSDB_INDEXER_BACKEND:-none}
    # When the indexer backend is http_tdb_search, export the endpoint so
    # the tdb_search plugin can find it via plugin_env/2. Without this,
    # suggest/search/similar handlers fail with TdbSearchEndpointNotConfigured.
    if [ "$TERMINUSDB_INDEXER_BACKEND" = "http_tdb_search" ]; then
        export TERMINUSDB_TDB_SEARCH_ENDPOINT=${TERMINUSDB_TDB_SEARCH_ENDPOINT:-http://127.0.0.1:7372}
    fi
    export TERMINUSDB_SEARCH_ADMIN_USER=${TERMINUSDB_SEARCH_ADMIN_USER:-admin}
    export TERMINUSDB_SEARCH_ADMIN_SECRET=${TERMINUSDB_SEARCH_ADMIN_SECRET:-root}

    # Worker pool size: 30 for test workloads with concurrent streaming
    # and indexing. Override with TERMINUSDB_WORKER_POOL_SIZE if needed.
    export TERMINUSDB_WORKER_POOL_SIZE=${TERMINUSDB_WORKER_POOL_SIZE:-30}

    # Start the server in a new session so it survives the script exiting.
    python3 -c "
import os, subprocess, sys
log_file = sys.argv[1]
pid_file = sys.argv[2]
cmd = sys.argv[3:]
proc = subprocess.Popen(cmd, stdout=open(log_file, 'w'), stderr=subprocess.STDOUT, start_new_session=True, env=os.environ)
with open(pid_file, 'w') as f:
    f.write(str(proc.pid))
" "$LOG_FILE" "$PID_FILE" ./terminusdb serve

    local pid
    pid=$(cat "$PID_FILE")
    echo "Selected backend: $SERVER_BACKEND"
    
    echo "TerminusDB test server starting (PID: $pid)..."
    echo "Storage: $STORAGE_DIR"
    echo "Logs: $LOG_FILE"
    
    # Wait for server to be ready
    echo -n "Waiting for server to be ready"
    local max_wait=5
    local waited=0
    while [ $waited -lt $max_wait ]; do
        # Check both if process is running and if API responds
        if ! ps -p "$pid" > /dev/null 2>&1; then
            echo " ✗"
            echo "ERROR: Server process died unexpectedly"
            echo "Check logs: $LOG_FILE"
            cat "$LOG_FILE"
            rm -f "$PID_FILE"
            return 1
        fi

        if curl -s -f --max-time 2 "http://127.0.0.1:${SERVER_PORT}/api/ok" > /dev/null 2>&1; then
            echo " ✓"
            echo "TerminusDB test server is ready!"
            echo "  Backend:         $SERVER_BACKEND"
            echo "  Server URL:      http://127.0.0.1:${SERVER_PORT}"
            echo "  User: admin"
            echo "  Pass: $ADMIN_PASS"
            return 0
        fi
        echo -n "."
        sleep 0.5
        waited=$((waited + 1))
    done
    
    echo " ✗"
    echo "ERROR: Server failed to start within ${max_wait}s"
    echo "Check logs: $LOG_FILE"
    cat "$LOG_FILE"
    stop_server
    return 1
}

function stop_server() {
    if [ ! -f "$PID_FILE" ]; then
        echo "No PID file found. Server may not be running."
        return 0
    fi
    
    local pid=$(cat "$PID_FILE")
    if ps -p "$pid" > /dev/null 2>&1; then
        echo "Stopping TerminusDB test server (PID: $pid)..."
        kill "$pid"
        
        # Wait for graceful shutdown
        local max_wait=10
        local waited=0
        while ps -p "$pid" > /dev/null 2>&1 && [ $waited -lt $max_wait ]; do
            sleep 1
            waited=$((waited + 1))
        done
        
        # Force kill if still running
        if ps -p "$pid" > /dev/null 2>&1; then
            echo "Forcing shutdown..."
            kill -9 "$pid" 2>/dev/null || true
        fi
        
        echo "Server stopped."
    else
        echo "Server not running (stale PID file)."
    fi
    
    rm -f "$PID_FILE"
}

function restart_server() {
    stop_server
    sleep 2
    start_server
}

function status() {
    local SERVER_PORT=${TERMINUSDB_SERVER_PORT:-6363}
    local SERVER_BACKEND=${TERMINUSDB_SERVER_BACKEND:-rust}
    if [ -f "$PID_FILE" ]; then
        local pid=$(cat "$PID_FILE")
        if ps -p "$pid" > /dev/null 2>&1; then
            echo "TerminusDB test server is running (PID: $pid)"
            echo "  Backend:    $SERVER_BACKEND"
            echo "  Server URL: http://127.0.0.1:${SERVER_PORT}"
            echo "  Logs:       $LOG_FILE"
            echo "  Storage:    $STORAGE_DIR"
            return 0
        else
            echo "TerminusDB test server is not running (stale PID file)"
            return 1
        fi
    else
        echo "TerminusDB test server is not running"
        return 1
    fi
}

function logs() {
    if [ -f "$LOG_FILE" ]; then
        cat "$LOG_FILE"
    else
        echo "No log file found at $LOG_FILE"
        return 1
    fi
}

function clean() {
    stop_server
    echo "Cleaning up test artifacts..."
    rm -rf "$STORAGE_DIR"
    rm -f "$LOG_FILE"
    echo "Cleanup complete."
}

# Main command dispatcher
case "${1:-}" in
    start)
        shift
        start_server "$@"
        ;;
    stop)
        stop_server
        ;;
    restart)
        shift
        stop_server
        sleep 1
        start_server "$@"
        ;;
    status)
        status
        ;;
    logs)
        shift
        logs "$@"
        ;;
    clean)
        clean
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|status|logs|clean}"
        echo ""
        echo "Commands:"
        echo "  start [--clean]        - Start TerminusDB test server"
        echo "                         --clean wipes storage"
        echo "  stop                   - Stop TerminusDB test server"
        echo "  restart [--clean]      - Restart server (--clean wipes storage)"
        echo "  status                 - Check if server is running"
        echo "  logs                   - Show server logs"
        echo "  clean                  - Stop server and remove all test data"
        echo ""
        echo "Examples:"
        echo "  $0 start               # Rust backend on 6363"
        echo "  $0 start --clean       # Start with fresh storage"
        echo "  $0 restart --clean     # Restart with fresh storage"
        echo ""
        echo "Environment variables:"
        echo "  TERMINUSDB_SERVER_PORT     - Listen port (default: 6363)"
        echo "  TERMINUSDB_SERVER_BACKEND  - Server backend: rust or swipl (default: rust)"
        echo "  TERMINUSDB_ADMIN_PASS      - Admin password (default: root)"
        exit 1
        ;;
esac
