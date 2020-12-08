VERSION=4.0.1
LICENSE=Apache-2.0
MAINTAINER="TerminusDB Team <team@terminusdb.com>"

.DEFAULT_GOAL := bin
.PHONY: bin

all: bin deb docs

bin:
	$(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f bootstrap.pl

deb: bin
	 fpm -f -s dir -t deb -d libtcmalloc-minimal4 -n terminusdb -v $(VERSION) \
		--license $(LICENSE) -m $(MAINTAINER) \
		--vendor "TerminusDB" --description "TerminusDB, the revision control database" \
		./terminusdb=/usr/local/bin/


debug:
	echo "main, halt." | swipl -f bootstrap.pl

docs:
	utils/compile_docs.sh
	ronn docs/terminusdb.1.ronn --roff
	cp docs/terminusdb.1.ronn docs/CLI.md
