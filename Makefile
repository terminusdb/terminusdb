VERSION=4.2.3
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"
DEB_TARGET=terminusdb_$(VERSION)_amd64.deb
TARGET=terminusdb

.DEFAULT_GOAL := bin
.PHONY: bin

all: bin deb docs

$(TARGET):
	# Build the target and fail for errors and warnings. Ignore warnings
	# having "qsave(strip_failed(..." that occur on macOS.
	LANG=C.UTF-8 $(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f src/bootstrap.pl 2>&1 | \
	  grep -v 'qsave(strip_failed' | \
	  (! grep -e ERROR -e Warning)

bin: $(TARGET)

deb: $(TARGET)
	 fpm -f -s dir -t deb -d libtcmalloc-minimal4 -d libarchive13 -n terminusdb -v $(VERSION) \
		--license $(LICENSE) -m $(MAINTAINER) \
		--vendor "TerminusDB" --description "TerminusDB, the revision control database" \
		./terminusdb=/usr/local/bin/

rpm: $(TARGET)
	 fpm -f -s dir -t rpm -d pl -n terminusdb -v $(VERSION) \
		--license $(LICENSE) -m $(MAINTAINER) --rpm-rpmbuild-define "_build_id_links none"  \
		--vendor "TerminusDB" --description "TerminusDB, the revision control database" \
		./terminusdb=/usr/bin/


debug:
	echo "main, halt." | swipl -f src/bootstrap.pl

.PHONY: docs
docs:
	src/utils/compile_docs.sh
	ronn docs/terminusdb.1.ronn --roff
	cp docs/terminusdb.1.ronn docs/CLI.md

clean:
	rm -f terminusdb
