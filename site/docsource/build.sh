#!/bin/sh
set -e
echo "Building" >build.compile
asciidoctor -a reproducible -D ../../docs/ ./Manual.adoc  2>> build.compile
asciidoctor -a reproducible -D ../../docs/blog ./blog/index.adoc 2>> build.compile
asciidoctor -a reproducible -a linkcss -a stylesheet!  -D ../../docs/ ./index.adoc 2>> build.compile
echo "Finished" >> build.compile
reload
# wkhtmltopdf -B 20 -T 20 ../../docs/Manual.html ../../docs/Manual.pdf &
