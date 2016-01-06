#!/bin/sh
set -e
gmake -r -j30 all 
gmake depend
