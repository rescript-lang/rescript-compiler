#!/bin/sh
gitbook build >> build.compile

echo "copy files start..." >> build.compile
cp -r _book/* ../../scriptdoc.git/
echo "copy files finished" >> build.compile
