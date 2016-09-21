#!/bin/sh
set -e 
bsc.exe -I ../runtime -I ../stdlib -bs-no-any-assert -bs-files *.ml *.mli
