BuckleScript inherits the command line arguments of the [OCaml compiler](http://caml.inria.fr/pub/docs/manual-ocaml/comp.html). It also adds several flags:

* -js-module

Specify the JavaScript module system to use in the generated JavaScript code. Supported values are `commonjs`, `amdjs` and `goog:<namespace>`

When you want to use `goog` module system, you can do things like this:

```bash
bsc -js-module goog xx.ml 
  # no namespace

bsc -js-module goog:bloomberg.buckle.test xx.ml 
  # namespace is bloomberg.buckle.test
```

You would then need a bundler for different the different module systems: `webpack` supports `commonjs` and `amdjs` while `google closure compiler` supports all.

*  -js-gen-tds

Trigger the generation of TypeScript `.d.ts` files.

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