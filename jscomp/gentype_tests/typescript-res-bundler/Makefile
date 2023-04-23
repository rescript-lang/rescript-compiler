SHELL = /bin/bash

node_modules/.bin/tsc:
	npm install

test: node_modules/.bin/tsc
	npm run build
	npm run tsc

clean:
	rm -rf node_modules lib src/*.bs.js src/*.gen.tsx

.DEFAULT_GOAL := test

.PHONY: clean test
