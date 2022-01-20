VERSION=4.2.3
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"
SWIPL=LANG=C.UTF-8 $(SWIPL_DIR)swipl
RONN_FILE=docs/terminusdb.1.ronn
ROFF_FILE=docs/terminusdb.1
TARGET=terminusdb

RUST_FILES = src/rust/Cargo.toml src/rust/Cargo.lock $(shell find src/rust/src/ -type f -name '*.rs')
PROLOG_FILES = $(shell find ./ -not -path './rust/*' \( -name '*.pl' -o -name '*.ttl' -o -name '*.json' \))

ifeq ($(shell uname), Darwin)
	RUST_LIB_NAME := librust.dylib
else
	RUST_LIB_NAME := librust.so
endif

RUST_LIBRARY_FILE:=src/rust/target/release/$(RUST_LIB_NAME)
RUST_TARGET:=src/rust/$(RUST_LIB_NAME)

################################################################################

# Build the binary (default).
.PHONY: bin
bin: $(TARGET)

# Build the binary and the documentation.
.PHONY: all
all: bin docs

.PHONY: module
module: $(RUST_TARGET)

# Build a debug version of the binary.
.PHONY: debug
debug: $(RUST_TARGET)
	echo "main, halt." | $(SWIPL) -f src/bootstrap.pl

# Quick command for interactive
.PHONY: i
i: $(RUST_TARGET)
	$(SWIPL) -f src/interactive.pl

# Check for implicit imports by disabling autoload
.PHONY: check-imports
check-imports: $(RUST_TARGET)
	$(SWIPL) \
	  --on-error=status \
	  -g "set_prolog_flag(autoload, false)" \
	  -g "['src/bootstrap.pl']" \
	  -g halt

# Remove the binary.
.PHONY: clean
clean:
	rm -f $(TARGET)
	rm -f $(RUST_TARGET)
	cd src/rust && cargo clean

# Build the documentation.
.PHONY: docs
docs: $(ROFF_FILE)

# Remove the documentation.
.PHONY: docs-clean
docs-clean:
	rm -f $(RONN_FILE) $(ROFF_FILE)

################################################################################

$(TARGET): $(RUST_TARGET) $(PROLOG_FILES)
	# Build the target and fail for errors and warnings. Ignore warnings
	# having "qsave(strip_failed(..." that occur on macOS.
	$(SWIPL) -t 'main,halt.' -O -q -f src/bootstrap.pl 2>&1 | \
	  grep -v 'qsave(strip_failed' | \
	  (! grep -e ERROR -e Warning)

$(RUST_TARGET): $(RUST_FILES)
	cd src/rust && cargo build --release
	cp $(RUST_LIBRARY_FILE) $(RUST_TARGET)

# Create input for `ronn` from a template and the `terminusdb` help text.
$(RONN_FILE): docs/terminusdb.1.ronn.template $(TARGET)
	HELP="$$(./$(TARGET) help -m)" envsubst < $< > $@

# Create a man page from using `ronn`.
$(ROFF_FILE): $(RONN_FILE)
	ronn --roff $<
