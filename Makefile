all:
	echo "main, halt." | swipl -O -q -f bootstrap.pl

debug:
	echo "main, halt." | swipl -q -f bootstrap.pl
