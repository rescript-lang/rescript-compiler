#!/bin/sh
BELT_FOLDER_NAME="belt-js"
MAIN_PACKAGE_VERSION=$(node -p -e "require('./package.json').version")
BELT_PACKAGE_VERSION=$(node -p -e "require('./$BELT_FOLDER_NAME/package.json').version")

echo "**Preparing and publishing belt-js...**\n"

if [ $MAIN_PACKAGE_VERSION == $BELT_PACKAGE_VERSION ]; then
  make && make install && \
  cd jscomp && cd ../belt-js && \
  mkdir -p ./lib/js && cp ../lib/js/* ./lib/js && \
  mkdir -p ./lib/amdjs && cp ../lib/amdjs/* ./lib/amdjs && \
  mkdir -p ./lib/es6 && cp ../lib/es6/* ./lib/es6 && \
  echo "Finished prepublishing $BELT_FOLDER_NAME, version $BELT_PACKAGE_VERSION"
  && npm publish
else
  echo "Error: bs-platform and belt-js don't have the same version.\nbs-platform is $MAIN_PACKAGE_VERSION and belt-js is $BELT_PACKAGE_VERSION"
fi
