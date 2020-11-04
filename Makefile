all:
	echo "main, halt." | swipl -O -f bootstrap.pl

debug:
	echo "main, halt." | swipl -f bootstrap.pl
