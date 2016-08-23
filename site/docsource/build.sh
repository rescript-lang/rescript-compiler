#!/bin/sh
set -e 
echo "Building" >build.compile
asciidoctor -a reproducible -D ../../docs/ ./index.adoc 2>> build.compile
echo "Finished" >> build.compile
reload
