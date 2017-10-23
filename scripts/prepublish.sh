#!/bin/sh
MAIN_PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
STDLIB_PACKAGE_VERSION=$(node -p -e "require('./lib/package.json').version")

echo "**Preparing and publishing bs-stdlib...**\n"

if [ $MAIN_PACKAGE_VERSION == $STDLIB_PACKAGE_VERSION ]; then
  cd jscomp && make world && cd ../lib && echo $PACKAGE_VERSION && npm publish
else
  echo "Error: bs-platform and bs-stdlib don't have the same version.\nbs-platform is $MAIN_PACKAGE_VERSION and bs-stdlib is $STDLIB_PACKAGE_VERSION"
fi
