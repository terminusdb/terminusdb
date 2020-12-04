.PHONY: all docs debug

all:
	$(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f bootstrap.pl

debug:
	echo "main, halt." | swipl -f bootstrap.pl

docs:
	utils/compile_docs.sh
	ronn docs/terminusdb.1.ronn --roff
	cp docs/terminusdb.1.ronn docs/CLI.md
