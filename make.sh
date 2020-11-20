#!/bin/sh

# Script because it is good for the Dockerfile to have the ability
# to compile without make
"$SWIPL_DIR"swipl -t 'main,halt.' -O -q -f bootstrap.pl
