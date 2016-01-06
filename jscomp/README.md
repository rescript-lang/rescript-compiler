

# Code structure

The highlevel architecture is illustrated as below:

```
Lambda IR (OCaml compiler libs) ---+
  |   ^                            |                      
  |   |                     Lambda Passes (lam_* files) 
  |   |             Optimization/inlining/dead code elimination
  |   \                            |
  |    \ --------------------------+ 
  |
  |  Self tail call elimination
  |  Constant folding + propagation
  V
JS IR (J.ml)  ---------------------+
  |   ^                            |
  |   |                     JS Passes (js_* files)
  |   |            Optimization/inlining/dead code elimination
  |   \                            |
  |    \  -------------------------+
  |        
  |  Smart printer includes scope analysis 
  |
  V
Javascript Code 
```

Note that there is one design goal to keep in mind, never introduce
any meaningless symbol unless real necessary, we do optimizations,
however, it should also compile readable output code.


# Dependencies and Dev-Build


### Build the compiler

If you don't change the type definition of JS IR, i.e, [j.ml](./j.ml),
then the only dependency is the build tool:
[ocamlbuild](http://caml.inria.fr/pub/docs/manual-ocaml-400/manual032.html)

```sh
ocamlbuild  -no-hygiene -cflags -g,-w,-40-30,-warn-error,+a-40-30,-keep-locs,-I,+compiler-libs  compiler.cmxa 
```

```sh
make  bin/ocamlscript
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


