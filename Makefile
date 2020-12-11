VERSION=4.0.1
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"
DEB_TARGET=terminusdb_$(VERSION)_amd64.deb
TARGET=terminusdb

.DEFAULT_GOAL := bin
.PHONY: bin

all: bin deb docs

$(TARGET):
	$(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f bootstrap.pl

bin: $(TARGET)

deb: $(TARGET)
	 fpm -f -s dir -t deb -d libtcmalloc-minimal4 -d libarchive13 -n terminusdb -v $(VERSION) \
		--license $(LICENSE) -m $(MAINTAINER) \
		--vendor "TerminusDB" --description "TerminusDB, the revision control database" \
		./terminusdb=/usr/local/bin/

rpm: $(TARGET)
	 fpm -f -s dir -t rpm -d pl -n terminusdb -v $(VERSION) \
		--license $(LICENSE) -m $(MAINTAINER) \
		--vendor "TerminusDB" --description "TerminusDB, the revision control database" \
		./terminusdb=/usr/bin/


debug:
	echo "main, halt." | swipl -f bootstrap.pl

docs:
	utils/compile_docs.sh
	ronn docs/terminusdb.1.ronn --roff
	cp docs/terminusdb.1.ronn docs/CLI.md
