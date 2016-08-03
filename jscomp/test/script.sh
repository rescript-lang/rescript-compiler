#!/bin/sh
rm *.cm*
bsc -bs-package-name bs-platform -bs-no-version-header -bs-cross-module-opt -bs-diagnose -w -40 -I ../runtime -I ../stdlib -I ../others -bs-files *.ml *.mli
