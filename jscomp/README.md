Hello! This is the main directory for BuckleScript. `jscomp` is just a name that mirrors OCaml's own `bytecomp` and `asmcomp` (bytecode compilation and native compilation logic respectively). For building it, please see [CONTRIBUTING.md](../CONTRIBUTING.md).

Extra info:

## Rebuilding the browser-based playground

### Get `js_of_ocaml` from the normal switch

```
opam switch 4.02.3
eval `opam config env`
opam install js_of_ocaml
which js_of_ocaml # symlink this into your $PATH, maybe /usr/local/bin or something
```

### Do everything else from the bucklescript switch

```
opam switch 4.02.3+buckle-master
eval `opam config env`
opam install camlp4 ocp-ocamlres
(cd vendor/ocaml && make world)
(cd jscomp && BS_RELEASE_BUILD=true BS_PLAYGROUND=../../bucklescript-playground node repl.js)
```

## Sub directories

### [stdlib](./stdlib)

A copy of standard library from OCaml distribution(4.02) for fast development,
so that we don't need bootstrap compiler, everytime we deliver a new feature.

- Files copied
  - sources
  - Makefile.shared Compflags .depend Makefile
- Patches
  Most in [Makefile.shared](./stdlib/Makefile.shared)


## [test](./test)

The directory containing unit-test files, some unit tests are copied from OCaml distribution(4.02)
