DIST ?= community

RONN_FILE=docs/terminusdb.1.ronn
ROFF_FILE=docs/terminusdb.1
TARGET=terminusdb

################################################################################

# Build the binary.
.PHONY: default
default:
	@$(MAKE) -f distribution/Makefile.prolog

# Build the Docker image for development and testing. To use the TerminusDB
# container, see: https://github.com/terminusdb/terminusdb-bootstrap
.PHONY: docker
docker: export DOCKER_BUILDKIT=1
docker:
	docker build . \
	  --file Dockerfile \
	  --tag terminusdb/terminusdb-server:local \
	  --build-arg DIST="$(DIST)" \
	  --build-arg TERMINUSDB_GIT_HASH="$$(git rev-parse --verify HEAD)"

# Install minimal pack dependencies.
.PHONY: install-deps
install-deps: install-tus

# Install the tus pack.
.PHONY: install-tus
install-tus:
	@$(MAKE) -f distribution/Makefile.deps $@

# Install the dashboard
.PHONY: install-dashboard
install-dashboard:
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

# Build the dylib.
.PHONY: rust
rust:
	@$(MAKE) -f distribution/Makefile.rust

# Run the unit tests in swipl.
.PHONY: test
test:
	@$(MAKE) -f distribution/Makefile.prolog $@

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
clean: realclean-rust clean-dashboard clean-deps prolog-clean

# Remove the dylib.
.PHONY: clean-rust
clean-rust:
	@$(MAKE) -f distribution/Makefile.rust clean

# Remove the dylib and all Rust build files.
.PHONY: realclean-rust
realclean-rust:
	@$(MAKE) -f distribution/Makefile.rust realclean

# Remove the dashboard
.PHONY: clean-dashboard
clean-dashboard:
	@$(MAKE) -f distribution/Makefile.deps clean-dashboard

# Remove the dashboard
.PHONY: clean-deps
clean-deps:
	@$(MAKE) -f distribution/Makefile.deps clean-deps

# Build the documentation.
.PHONY: docs
docs: $(ROFF_FILE)

################################################################################

# Create input for `ronn` from a template and the `terminusdb` help text.
$(RONN_FILE): docs/terminusdb.1.ronn.template $(TARGET)
	HELP="$$(./$(TARGET) help -m)" envsubst < $< > $@

# Create a man page from using `ronn`.
$(ROFF_FILE): $(RONN_FILE)
	ronn --roff $<
