#!/bin/bash

# terminusdb-test-server.sh
# Manages a local TerminusDB server for testing with a clean temporary storage

set -e

export TERMINUSDB_LOG_LEVEL=${TERMINUSDB_LOG_LEVEL:-DEBUG}
export TERMINUSDB_LOG_FORMAT=${TERMINUSDB_LOG_FORMAT:-text}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PID_FILE="$SCRIPT_DIR/.terminusdb-test.pid"
STORAGE_DIR="$SCRIPT_DIR/.terminusdb-test-storage"
LOG_FILE="$SCRIPT_DIR/.terminusdb-test.log"

# Default admin password (terminusdb default)
ADMIN_PASS="${TERMINUSDB_ADMIN_PASS:-root}"

function start_server() {
    local clean_storage=false
    if [ "$1" = "--clean" ]; then
        clean_storage=true
    fi
    
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
    
    # Check if another process is using port 6363
    if lsof -Pi :6363 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "ERROR: Port 6363 is already in use by another process:"
        lsof -Pi :6363 -sTCP:LISTEN
        echo ""
        echo "Stop the conflicting process with:"
        echo "  kill \$(lsof -t -i:6363)"
        echo ""
        echo "Or if it's a TerminusDB instance:"
        echo "  pkill -f terminusdb"
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
    export TERMINUSDB_SERVER_PORT=6363
    export TERMINUSDB_ADMIN_PASS="$ADMIN_PASS"
    export TERMINUSDB_SERVER_DB_PATH="$STORAGE_DIR"
    
    nohup ./terminusdb serve > "$LOG_FILE" 2>&1 &
    local pid=$!
    echo $pid > "$PID_FILE"
    
    echo "TerminusDB test server starting (PID: $pid)..."
    echo "Storage: $STORAGE_DIR"
    echo "Logs: $LOG_FILE"
    
    # Wait for server to be ready
    echo -n "Waiting for server to be ready"
    local max_wait=15
    local waited=0
    while [ $waited -lt $max_wait ]; do
        # Check both if process is running and if API responds
        if ! ps -p "$pid" > /dev/null 2>&1; then
            echo " ✗"
            echo "ERROR: Server process died unexpectedly"
            echo "Check logs: $LOG_FILE"
            tail -20 "$LOG_FILE"
            rm -f "$PID_FILE"
            return 1
        fi
        
        if curl -s -f --max-time 2 http://127.0.0.1:6363/api/ok > /dev/null 2>&1; then
            echo " ✓"
            echo "TerminusDB test server is ready!"
            echo "  URL: http://127.0.0.1:6363"
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
    if [ -f "$PID_FILE" ]; then
        local pid=$(cat "$PID_FILE")
        if ps -p "$pid" > /dev/null 2>&1; then
            echo "TerminusDB test server is running (PID: $pid)"
            echo "  URL: http://127.0.0.1:6363"
            echo "  Logs: $LOG_FILE"
            echo "  Storage: $STORAGE_DIR"
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
        tail "$@" "$LOG_FILE"
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
        if [ "$1" = "--clean" ]; then
            stop_server
            sleep 1
            start_server --clean
        else
            restart_server
        fi
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
        echo "  start [--clean]  - Start TerminusDB test server (--clean wipes storage)"
        echo "  stop             - Stop TerminusDB test server"
        echo "  restart [--clean]- Restart server (--clean wipes storage)"
        echo "  status           - Check if server is running"
        echo "  logs [-f]        - Tail server logs (use -f for follow)"
        echo "  clean            - Stop server and remove all test data"
        echo ""
        echo "Examples:"
        echo "  $0 start          # Start with existing storage"
        echo "  $0 start --clean  # Start with fresh storage"
        echo "  $0 restart        # Quick restart, keep storage"
        echo "  $0 restart --clean# Restart with fresh storage"
        echo ""
        echo "Environment variables:"
        echo "  TERMINUSDB_ADMIN_PASS - Admin password (default: root)"
        exit 1
        ;;
esac
