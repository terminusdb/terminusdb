#!/bin/bash
CURRENT_DIR=$(pwd)
SOURCE="${BASH_SOURCE[0]}"
mkdir -p app_dir/usr/share/terminusdb
mkdir -p app_dir/usr/bin
mkdir -p app_dir/usr/lib/x86_64-linux-gnu
# Build TerminusDB
cd ../../ && make install-dashboard && make install-deps && make && cd "$CURRENT_DIR"
# Copy relevant files back to appimage dir
cp -r ../../terminusdb ../../dashboard app_dir/usr/share/terminusdb/
cp -r /usr/lib/swi-prolog app_dir/usr/lib/
cp -L /usr/lib/x86_64-linux-gnu/libedit.so.2 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
cp -L /lib/x86_64-linux-gnu/libpcre.so.3 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
cp -L /usr/lib/x86_64-linux-gnu/libbsd.so.0 app_dir/usr/lib/swi-prolog/lib/x86_64-linux/
LD_LIBRARY_PATH='/usr/lib/swi-prolog/lib/x86_64-linux/' ./linuxdeploy-x86_64.AppImage --appdir ./app_dir --executable /usr/lib/swi-prolog/bin/x86_64-linux/swipl --library /usr/lib/swi-prolog/lib/x86_64-linux/libswipl.so.9 --library /lib/x86_64-linux-gnu/libpcre.so.3 -d terminusdb.desktop -i terminusdb.svg --custom-apprun AppRun --output appimage --verbosity=0
