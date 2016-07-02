#!/bin/sh
set -e 
bsc -I ../runtime -I ../stdlib -bs-no-any-assert -bs-files *.ml *.mli
