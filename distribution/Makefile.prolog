DIST ?= community

SWIPL_LINT_VERSION := v0.8
SWIPL_LINT_PATH = .deps/pl_lint-$(SWIPL_LINT_VERSION).pl

COMMUNITY_SRC_DIR := src
ENTERPRISE_SRC_DIR := terminusdb-enterprise/prolog

SRC_DIRS := $(COMMUNITY_SRC_DIR)

export TERMINUSDB_ENTERPRISE := false

ifeq "$(DIST)" "enterprise"
  ifeq "$(wildcard $(ENTERPRISE_SRC_DIR))" ""
    $(error Error! Directory not found: $(CURDIR)/$(ENTERPRISE_SRC_DIR))
  endif
  SRC_DIRS += $(ENTERPRISE_SRC_DIR)
  export TERMINUSDB_ENTERPRISE := true
else ifneq "$(DIST)" "community"
  $(error Error! Unknown $$DIST: $(DIST))
endif

ifeq "$(wildcard $(COMMUNITY_SRC_DIR))" ""
  $(error Error! Directory not found: $(CURDIR)/$(COMMUNITY_SRC_DIR))
endif

ifeq "$(shell uname)" "Darwin"
  DYLIB_EXT := dylib
else
  DYLIB_EXT := so
endif

SWIPL = LANG=C.UTF-8 $(SWIPL_DIR)swipl
TARGET := terminusdb
RUST_TARGET := src/rust/librust.$(DYLIB_EXT)

# DIST-tracking stamp.
#
# librust.$(DYLIB_EXT) is a single fixed path regardless of distribution, but
# the community and enterprise dylibs export different Prolog FFI.
#
# The $(RUST_TARGET) rule below has no prerequisites, so once the file exists
# make never re-runs distribution/Makefile.rust, and switching DIST silently
# leaves the previous distribution's dylib in place. That manifests as
# "Unknown procedure" errors during tests.
#
# To make DIST switches transparent (no `make clean` required), we record the
# last DIST used to produce $(RUST_TARGET) in a stamp file. When the current
# DIST differs, we remove $(RUST_TARGET) at parse time so the normal rule
# fires and rebuilds it from the correct cargo build directory. Same logic
# also lives in distribution/Makefile.rust for direct `make rust` invocations.
#
# Important: this is only safe when distribution/Makefile.rust is actually
# present so the rebuild path exists. In the Docker base stage we ship a
# pre-built dylib and never copy Makefile.rust — removing the dylib there
# would leave the build with no way to recreate it. So we make the
# destructive part of the stamp logic conditional on Makefile.rust existing.
LAST_DIST_FILE := src/rust/.last-dist
LAST_DIST := $(shell cat $(LAST_DIST_FILE) 2>/dev/null)
ifneq ($(wildcard distribution/Makefile.rust),)
  ifneq ($(LAST_DIST),$(DIST))
    $(shell rm -f $(RUST_TARGET))
    $(shell printf '%s' '$(DIST)' > $(LAST_DIST_FILE))
  endif
endif

################################################################################

.PHONY: default
default: $(TARGET)

.PHONY: dev
dev: $(RUST_TARGET) dev-build

.PHONY: i
i: $(RUST_TARGET)
	$(SWIPL) -f src/interactive.pl

.PHONY: test
test: $(RUST_TARGET)
ifdef SUITE
	$(SWIPL) \
	  -g '(run_tests($(SUITE)) -> halt(0) ; halt(1))' \
	  -f src/interactive.pl
else
	$(SWIPL) \
	  -g '(run_tests -> halt(0) ; halt(1))' \
	  -f src/interactive.pl
endif

.PHONY: download-lint
download-lint: $(SWIPL_LINT_PATH)

.PHONY: lint
lint: $(SWIPL_LINT_PATH)
	$(SWIPL) -f src/load_paths.pl src/core/query/expansions.pl $(SWIPL_LINT_PATH)

.PHONY: clean
clean:
	$(RM) $(TARGET)

################################################################################

# Development build target (macOS-friendly, no library stripping)
.PHONY: dev-build
dev-build: $(shell find $(SRC_DIRS) -not -path 'src/rust/*' \( -name '*.pl' -o -name '*.ttl' -o -name '*.json' \))
	@echo "Building development binary (no library stripping)..."
	$(SWIPL) \
	  --on-error=halt \
	  --on-warning=halt \
	  --quiet \
	  -O \
	  -t 'main, halt' \
	  -f src/bootstrap_dev.pl
	@echo "Development binary 'terminusdb' created successfully"
	@echo "Run with: ./terminusdb help"

# Production build target (standalone, with library stripping)
$(TARGET): $(RUST_TARGET)
$(TARGET): $(shell find $(SRC_DIRS) -not -path 'src/rust/*' \( -name '*.pl' -o -name '*.ttl' -o -name '*.json' \))
	$(SWIPL) \
	  --on-error=halt \
	  --on-warning=halt \
	  --quiet \
	  -O \
	  -t 'main, halt' \
	  -f src/bootstrap.pl

$(RUST_TARGET):
	@$(MAKE) -f distribution/Makefile.rust $@

$(SWIPL_LINT_PATH):
	curl -L --create-dirs -o $@ "https://raw.githubusercontent.com/terminusdb-labs/swipl-lint/$(SWIPL_LINT_VERSION)/pl_lint.pl"
