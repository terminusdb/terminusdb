DIST ?= community
# Default was 9.2.9
SWIPL_VERSION ?= 10.0.1

RONN_FILE=docs/terminusdb.1.ronn
ROFF_FILE=docs/terminusdb.1
TARGET=terminusdb

################################################################################

# Build the binary.
.PHONY: default
default:
	@$(MAKE) -f distribution/Makefile.prolog

# Build the development binary (macOS-friendly, no library stripping).
.PHONY: dev
dev:
	rm src/rust/target/release/libterminusdb_dylib.dylib || true
	rm src/rust/librust.* || true
	rm src/rust/librust.* || true
	@$(MAKE) -f distribution/Makefile.prolog $@

.PHONY: restart
restart:
	tests/terminusdb-test-server.sh restart

.PHONY: server-clean
server-clean:
	tests/terminusdb-test-server.sh start --clean

# Build the Docker image for development and testing. To use the TerminusDB
# container, see: https://github.com/terminusdb/terminusdb-bootstrap
# To make with swipl 10, use: make docker SWIPL_VERSION=10.0.0
.PHONY: docker
docker: export DOCKER_BUILDKIT=1
docker: SKIP_TESTS ?= false
docker:
	docker build . \
	  --file Dockerfile \
	  --tag terminusdb/terminusdb-server:local \
	  --build-arg SWIPL_VERSION="$(SWIPL_VERSION)" \
	  --build-arg SKIP_TESTS="$(SKIP_TESTS)" \
	  --build-arg DIST="$(DIST)" \
	  --build-arg TERMINUSDB_GIT_HASH="$$(git rev-parse --verify HEAD)"

# Build the Docker image for development using local swipl-rs sources.
.PHONY: docker-debug
docker-debug: export DOCKER_BUILDKIT=1
docker-debug: SKIP_TESTS ?= true
docker-debug:
	docker build . \
	  --file docker/debug/Dockerfile \
	  --tag terminusdb/terminusdb-server:debug \
	  --build-context swipl-rs=../swipl-rs \
	  --build-arg SWIPL_VERSION="$(SWIPL_VERSION)" \
	  --build-arg DIST="$(DIST)" \
	  --build-arg SKIP_TESTS="$(SKIP_TESTS)" \
	  --build-arg TERMINUSDB_GIT_HASH="$$(git rev-parse --verify HEAD)"

# Install minimal pack dependencies.
.PHONY: install-deps
install-deps: install-tus

# Install the tus pack.
.PHONY: install-tus
install-tus:
	@$(MAKE) -f distribution/Makefile.deps $@

# Install the jwt_io pack.
.PHONY: install-jwt
install-jwt:
	@$(MAKE) -f distribution/Makefile.deps $@

# Download the lint tool.
.PHONY: download-lint
download-lint:
	@$(MAKE) -f distribution/Makefile.prolog $@

# Download and run the lint tool.
.PHONY: lint
lint:
	@$(MAKE) -f distribution/Makefile.prolog $@

.PHONY: clippy
clippy:
	cargo clippy --message-format=json --all-features --manifest-path=src/rust/Cargo.toml

.PHONY: lint-mocha
lint-mocha:
	sh -c "cd tests; npx npm run check"

.PHONY: lint-mocha-fix
lint-mocha-fix:
	sh -c "cd tests; npx npm run lint"

# The 1.34.7 version is chosen as the others have a React/styled components
# dependency that is not resolved by npx, making it an issue using npx
.PHONY: lint-openapi
lint-openapi:
	sh -c "npx @redocly/cli@1.34.7 lint docs/openapi.yaml --skip-rule no-server-example.com"

# Build the dylib.
.PHONY: rust
rust:
	@$(MAKE) -f distribution/Makefile.rust

# Run unit tests in swipl; all, or just one suite.
# make test OR make test SUITE='[json,terminus_store,tables]'
.PHONY: test
test:
	@$(MAKE) -f distribution/Makefile.prolog $@

# Run the unit tests in node.
# Usage: make test-int                    # run all tests
#        make test-int SUITE=data-version # run test/data-version.js
#        make test-int SUITE="cli-*"      # run all cli tests
.PHONY: test-int
test-int: server-clean
ifdef SUITE
	sh -c "cd tests ; npx mocha 'test/$(SUITE).js'"
else
	sh -c "cd tests ; npx mocha"
endif

# Start Docker container for integration testing (no plugins).
# Rebuilds the docker image and recreates the container.
.PHONY: docker-test-server
docker-test-server: docker
	-docker stop terminusdb-sandbox-test-int 2>/dev/null
	-docker rm terminusdb-sandbox-test-int 2>/dev/null
	docker run -d --name terminusdb-sandbox-test-int \
		-p 6363:6363 \
		-e TERMINUSDB_ADMIN_PASS=root \
		-e TERMINUSDB_PLUGINS_PATH=/void \
		terminusdb/terminusdb-server:local
	@echo "Waiting for server to be ready..."
	@sleep 3
	@curl -sf http://127.0.0.1:6363/api/ok > /dev/null && echo "Docker test server ready at http://127.0.0.1:6363"

# Stop the Docker test server.
.PHONY: docker-test-server-stop
docker-test-server-stop:
	-docker stop terminusdb-sandbox-test-int 2>/dev/null
	-docker rm terminusdb-sandbox-test-int 2>/dev/null
	@echo "Docker test server stopped."

# Quick command for interactive
.PHONY: i
i:
	@$(MAKE) -f distribution/Makefile.prolog $@

# Remove the binary.
.PHONY: prolog-clean
prolog-clean:
	@$(MAKE) -f distribution/Makefile.prolog clean

# Remove everything.
.PHONY: clean
clean: realclean-rust clean-deps prolog-clean docs-clean

# Remove the dylib.
.PHONY: clean-rust
clean-rust:
	@$(MAKE) -f distribution/Makefile.rust clean

# Remove the dylib and all Rust build files.
.PHONY: realclean-rust
realclean-rust:
	@$(MAKE) -f distribution/Makefile.rust realclean

# Remove the deps
.PHONY: clean-deps
clean-deps:
	@$(MAKE) -f distribution/Makefile.deps clean-deps

.PHONY: docs-clean
docs-clean:
	@rm -f $(RONN_FILE)

# Build the documentation.
.PHONY: docs
docs: default $(ROFF_FILE)

################################################################################

# Create input for `ronn` from a template and the `terminusdb` help text.
$(RONN_FILE): docs/terminusdb.1.ronn.template $(TARGET)
	HELP="$$(./$(TARGET) help -m)" envsubst < $< > $@

# Create a man page from using `ronn`.
$(ROFF_FILE): $(RONN_FILE)
	ronn --roff $<

.PHONY: pr
pr: lint lint-mocha lint-openapi clean dev restart test test-int
