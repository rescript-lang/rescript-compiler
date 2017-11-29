#!/bin/sh
set -e 
bsc.exe -I ../runtime -I ../stdlib -bs-no-assertfalse -bs-files *.ml *.mli
