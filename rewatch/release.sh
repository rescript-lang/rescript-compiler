#!/bin/sh

# ensure that we have at least one argument conforming to semver
if [ $# -ne 1 ] || ! echo $1 | grep -qE "^[0-9]+\.[0-9]+\.[0-9]+$"; then
  echo "Usage: $0 <version>"
  exit 1
fi

# ensure we are on the master branch otherwise exit
if [ $(git rev-parse --abbrev-ref HEAD) != "master" ]; then
  echo "Not on master branch, exiting"
  exit 1
fi

# update the version in package.json
sed -i '' -e "s/\"version\": \".*\"/\"version\": \"$1\"/" package.json


# update the version in cargo.toml
sed -i '' -e "s/^version = \".*\"/version = \"$1\"/" Cargo.toml
cargo build

# commit the changes with the version
git add Cargo.toml package.json Cargo.lock
git commit -m ":rocket: - Release v$1"

# tag current commit with the first argument
git tag -a v$1 -m ":rocket: - Release v$1"

# push the changes
git push origin master

# push the tag
git push origin v$1
