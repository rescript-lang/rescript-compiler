#!/bin/sh
set -e 
echo "Building" >build.compile
asciidoctor -a reproducible -D ../../docs/ ./Manual.adoc  2>> build.compile
asciidoctor -D ../../docs/blog ./blog/index.adoc 2>> build.compile
asciidoctor -a linkcss -a stylesheet! -a reproducible -D ../../docs/ ./index.adoc 2>> build.compile
echo "Finished" >> build.compile
reload
