BuckleScript inherits command line arguments from [ocamlc compiler](http://caml.inria.fr/pub/docs/manual-ocaml/comp.html). It add several flags

* -js-module

Specify which module system it would use, currently support `commonjs`, `amdjs` and `goog:module`

When you want to use goog module system, you can do things like this

```
bsc -js-module goog xx.ml # no namespace
bsc -js-module goog:bloomberg.buckle.test xx.ml # namespace is bloomberg.buckle.test
```

Note that you need a bundler for different module systems, `webpack` supports `commonjs`, `amdjs`, `google closure compiler` support all.

*  -js-gen-tds

Specify generate `.d.ts` file for typescript or not
## Hello world

Currently, `BuckleScript` shares the same command line options as `ocamlc`
bytecode compiler.

Create a file called `hello.ml` as below

```sh
echo 'print_endline "hello world"' > hello.ml
```

```sh
bsc -c hello.ml
```

If everything goes well, you should have `hello.js`

```
console.log('hello world')
```