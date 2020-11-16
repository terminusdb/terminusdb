#!/bin/sh

# Script because it is good for the Dockerfile to have the ability
# to compile without make
echo "main, halt." | "$SWIPL_DIR"swipl -O -q -f bootstrap.pl
