#!/bin/sh
set -e 
echo "Building" >build.compile
asciidoctor -D ../../docs/ ./BuckleScript_guide.adoc 2>> build.compile
echo "Finished" >> build.compile
reload
