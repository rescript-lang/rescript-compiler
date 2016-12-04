#!/bin/sh
set -e
make -r -j30 all 
make depend
