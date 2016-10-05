#!/bin/sh
set -e
B=true bspack.exe -bs-main a.ml -o a_B.ml 
C=true bspack.exe -bs-main a.ml -o a_C.ml
bspack.exe -bs-main a.ml -o a_none.ml
