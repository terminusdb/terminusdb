all:
	echo "main, halt." | swipl -O -q -f bootstrap.pl

debug:
	echo "main, halt." | swipl -f bootstrap.pl
