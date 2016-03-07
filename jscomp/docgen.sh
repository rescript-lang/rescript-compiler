#!/bin/sh
set -e 
echo $BUCKLESCRIPT_API_DOC
ocamlbuild -build-dir $BUCKLESCRIPT_API_DOC -cflags -I,+compiler-libs -docflags -sort,-I,+compiler-libs,-css-style,../style.css compiler.docdir/index.html 2>> ./build.compile

