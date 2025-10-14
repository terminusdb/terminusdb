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
	$(SWIPL) \
	  --on-error=halt \
	  --on-warning=halt \
	  -g 'run_tests, halt' \
	  -f src/interactive.pl

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
