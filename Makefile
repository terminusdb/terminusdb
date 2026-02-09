DIST ?= community
SWIPL_VERSION ?= 9.2.9

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

# Build the Docker image for development and testing. To use the TerminusDB
# container, see: https://github.com/terminusdb/terminusdb-bootstrap
# To make with swipl 10, use: make docker SWIPL_VERSION=10.0.0
.PHONY: docker
docker: export DOCKER_BUILDKIT=1
docker:
	docker build . \
	  --file Dockerfile \
	  --tag terminusdb/terminusdb-server:local \
	  --build-arg SWIPL_VERSION="$(SWIPL_VERSION)" \
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

.PHONY: lint-openapi
lint-openapi:
	sh -c "npx @redocly/cli lint docs/openapi.yaml --skip-rule no-server-example.com"

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
.PHONY: test-int
test-int:
	sh -c "cd tests ; npx mocha"

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
