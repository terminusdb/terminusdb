#!/bin/sh
CURRENT_DIR=$(pwd)
TERMINUSDB_BRANCH=$1
TERMINUSDB_STORE_PROLOG_DIR="app_dir/usr/lib/swi-prolog/pack/terminus_store_prolog"

mkdir -p app_dir/usr/share/terminusdb
mkdir -p app_dir/usr/lib/swi-prolog/pack
mkdir -p app_dir/usr/lib/x86_64-linux-gnu
git clone https://github.com/terminusdb/terminusdb-server.git
cd terminusdb-server && git checkout $TERMINUSDB_BRANCH && cd ..
cp -r terminusdb-server/* app_dir/usr/share/terminusdb/
cp -r /usr/lib/swi-prolog app_dir/usr/lib/
cp -L /usr/lib/x86_64-linux-gnu/libedit.so.2 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
cp -L /lib/x86_64-linux-gnu/libpcre.so.3 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
cp -L /usr/lib/x86_64-linux-gnu/libbsd.so.0 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
rm -rf app_dir/usr/lib/swi-prolog/bin/x86_64-linux/swipl-ld
git clone https://github.com/terminusdb/terminus_store_prolog.git "$TERMINUSDB_STORE_PROLOG_DIR"
cd "$TERMINUSDB_STORE_PROLOG_DIR"
./make.sh
rm -rf rust/target/release/build
rm -rf rust/target/release/deps
cd $CURRENT_DIR
#linuxdeploy-x86_64.AppImage --appdir ./app_dir --executable /lib/swi-prolog/bin/x86_64-linux/swipl --library /lib/swi-prolog --library ~/.local/share/swi-prolog/pack/terminus_store_prolog/rust/target/release/libterminus_store_prolog.so -d terminusdb.desktop -i swipl.png --custom-apprun AppRun --output appimage --verbosity=0
./linuxdeploy-x86_64.AppImage --appdir ./app_dir --executable /usr/lib/swi-prolog/bin/x86_64-linux/swipl --library "$TERMINUSDB_STORE_PROLOG_DIR/rust/target/release/libterminus_store_prolog.so" --library /lib/x86_64-linux-gnu/libpcre.so.3 -d terminusdb.desktop -i terminusdb.svg --custom-apprun AppRun --output appimage --verbosity=0
