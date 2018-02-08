#!/bin/sh
set -e
rm ../lib/js/*.js 
git clean -dfx . ../lib
make clean
make themes
make -j9 check
BS_DEBUG=false make force-snapshotml
make -C .. -j9 world





