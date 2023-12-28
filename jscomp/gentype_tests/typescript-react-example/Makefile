SHELL = /bin/bash

test:
	npm install
	npm run build
	npm run tsc
	npm run lint

clean:
	rm -rf node_modules lib src/*.res.js src/*.gen.tsx

.DEFAULT_GOAL := test

.PHONY: clean test
