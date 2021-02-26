#!/bin/sh

cat docs/terminusdb.1.ronn.template | HELP=$(terminusdb help -m) envsubst > docs/terminusdb.1.ronn
