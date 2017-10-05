# Build Instructions

## Release mode

In release mode, assume you have NodeJS and
OCaml compiler  with the right version installed:

```sh
node scripts/install.js
```

The build process will generate configure file with correct `LIBDIR` path,
build all binaries and libraries and
install the binaries into `bin` and lib files into `lib`.

First it will try to generate `bin/config_whole_compiler.ml` based on existing
OCaml installation, if it fails, it will try to invoke `buildocaml.sh` to
install an OCaml compiler from scratch, and retry again


## Dev mode

### Setup

#### Use the correct opam switch for working on BuckleScript
```sh
opam update
opam switch 4.02.3+buckle-master
opam switch reinstall 4.02.3+buckle-master # do this if you get errors even from a clean compilation
opam install camlp4
eval `opam config env`
```

#### build BuckleScript's forked OCaml
```sh
cd vendor/ocaml
./configure -prefix `pwd`
make world.opt
make install
```

#### build all of Bucklescript
```sh
cd ../../jscomp
make world
```

### build the compiler (bsc.exe)

If you don't change the type definition of JS IR, i.e, [j.ml](./j.ml),
then the only dependency is the build tool:
`make`

```sh
rm -rf core/js_map.ml core/js_fold.ml && make core/js_map.ml core/js_fold.ml bin/bsc.exe
```

If you do want to change the JS IR, you also need
[camlp4](https://github.com/ocaml/camlp4), note that the version does
not need match the exact the same version of compiler.

### build the build system (bsb.exe)

```sh
make bin/bsb.exe && make bin/bsb_helper.ml && make -C bin bsb_helper.exe
```

Generate packed ML files for PR: `make force-snapshotml`

### build the runtime

```sh
cd ./runtime; make all
```

### build the stdlib

```sh
cd ./stdlib; make all
```

Several useful targets

- `make depend` generates the `.depend` files used by make to build things in the correct order
- `make check`  builds and runs the tests
- `make libs` builds the JS runtime

### Publish process
- Run `make force-snapshotml`
- Bump the compiler version
- Build the JS playground
  * Generate js_compiler.ml
  * Generate cmj data sets
  * Generate preload.js
  * Make sure AMDJS are up to date




## Code structure

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

# Rebuilding the browser-based playground

## Get `js_of_ocaml` from the normal switch

```
opam switch 4.02.3
eval `opam config env`
opam install js_of_ocaml.2.8.4
which js_of_ocaml # symlink this into your $PATH, maybe /usr/local/bin or something
```

## Do everything else from the bucklescript switch

```
opam switch 4.02.3+buckle-master
eval `opam config env`
opam install camlp4 ocp-ocamlres
(cd vendor/ocaml && make world)
(cd jscomp && node repl.js)
```

# Sub directories

## [stdlib](./stdlib)

A copy of standard library from OCaml distribution(4.02) for fast development,
so that we don't need bootstrap compiler, everytime we deliver a new feature.

- Files copied
  - sources
  - Makefile.shared Compflags .depend Makefile
- Patches
  Most in [Makefile.shared](./stdlib/Makefile.shared)


## [test](./test)

The directory containing unit-test files, some unit tests are copied from OCaml distribution(4.02)
