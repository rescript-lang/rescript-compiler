SHELL = /bin/bash

node_modules/.bin/tsc:
	npm install

test: node_modules/.bin/tsc
	npm run build

clean:
	npm run clean
	rm -rf node_modules lib

.DEFAULT_GOAL := test

.PHONY: clean test
