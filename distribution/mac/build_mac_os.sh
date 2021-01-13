#!/bin/bash
if [[ $1 == "" ]]; then
    echo "Please add a terminus_store_prolog version number"
    exit 1
fi
if [[ $2 == "" ]]; then
    echo "Please add current branch"
    exit 1
fi
mkdir public/pack
git clone --single-branch --branch "$2" https://github.com/terminusdb/terminusdb.git public/terminusdb-server
git clone --single-branch --branch "$1" https://github.com/terminusdb/terminus_store_prolog.git public/pack/terminus_store_prolog
bash public/pack/terminus_store_prolog/.ci/install_swipl_dmg.sh
cp -r /Applications/SWI-Prolog.app public/
mkdir -p public/pack/terminus_store_prolog/lib/x86_64-darwin
curl -L "https://github.com/terminusdb/terminus_store_prolog/releases/download/$1/libterminus_store.dylib" > public/pack/terminus_store_prolog/lib/x86_64-darwin/libterminus_store.dylib
npm run build-mac-os
