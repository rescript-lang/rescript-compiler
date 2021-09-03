#!/bin/sh
git branch --merged | grep -v '\*\|master\|main\|develop' | xargs -n 1 git branch -d