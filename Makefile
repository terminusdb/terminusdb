VERSION=4.2.3
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"
TARGET=terminusdb

.DEFAULT_GOAL := bin
.PHONY: bin

all: bin docs

$(TARGET):
	# Build the target and fail for errors and warnings. Ignore warnings
	# having "qsave(strip_failed(..." that occur on macOS.
	LANG=C.UTF-8 $(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f src/bootstrap.pl 2>&1 | \
	  grep -v 'qsave(strip_failed' | \
	  (! grep -e ERROR -e Warning)

bin: $(TARGET)

debug:
	echo "main, halt." | swipl -f src/bootstrap.pl

.PHONY: docs
docs:
	src/utils/compile_docs.sh
	ronn docs/terminusdb.1.ronn --roff
	cp docs/terminusdb.1.ronn docs/CLI.md

clean:
	rm -f terminusdb
