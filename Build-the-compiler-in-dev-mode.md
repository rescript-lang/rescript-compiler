### Build the compiler

If you don't change the type definition of JS IR, i.e, [j.ml](https://github.com/bloomberg/bucklescript/blob/master/jscomp/j.ml),
then the only dependency is the build tool:
[ocamlbuild](http://caml.inria.fr/pub/docs/manual-ocaml-400/manual032.html)

```sh
ocamlbuild  -no-hygiene -cflags -g,-w,-40-30,-warn-error,+a-40-30,-keep-locs,-I,+compiler-libs  compiler.cmxa 
```

```sh
make  bin/bsc
```

If you do want to change the JS IR, you also need
[camlp4](https://github.com/ocaml/camlp4), note that the version does
not need match the exact the same version of compiler.

so you need run commands below whenver you  change the type definition
of  JS IR.
```
make js_map.ml js_fold.ml
```

### Build the runtime

```sh
cd ./runtime; make all
```
### Build the stdlib

```sh
cd ./stdlib; make all
```
