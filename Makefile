all:
	$(SWIPL_DIR)swipl -t 'main,halt.' -O -q -f bootstrap.pl

debug:
	echo "main, halt." | swipl -f bootstrap.pl
