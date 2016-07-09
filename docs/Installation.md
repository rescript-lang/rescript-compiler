## Introduction

To use BuckleScript, you need both a specific version of the OCaml compiler (tweaked to make BuckleScript possible), and the BuckleScript software itself. The instructions below show how to install each of these using standard package managers as well as how to build from source for Unix/Linux systems. Build directions will be updated once Windows systems are supported.

## Default installation procedure (using package managers)

### 1. OCaml compiler from opam (optional)

When working with OCaml we recommend using [opam](https://opam.ocaml.org) package manager, available [here](https://opam.ocaml.org/doc/Install.html). You will benefit from the existing OCaml ecosystem.

Once you have `opam` installed, ask `opam` to switch to using our version of the compiler:

```sh
opam update
opam switch 4.02.3+buckle-1
eval `opam config env`
```

> This step is optional. npm installation will build the compiler from source if not installed from opam.

### 2. BuckleScript from npm (required)

The standard npm package management tool can be used to install BuckleScript. If you don't already have npm installed, follow the directions listed [here](https://docs.npmjs.com/getting-started/installing-node). Once npm is installed, run the following command:

```sh
npm install --save bs-platform
```

## Advanced installation procedure (from source)

BuckleScript has very few dependencies and building from source can easily be done.

### 1. OCaml compiler

To build OCaml compiler from source, download the modified OCaml compiler source [here](https://github.com/bloomberg/ocaml/releases) (version 4.02.3+buckle-1):

For detailed build instructions, follow the directions in the [repo](https://github.com/bloomberg/ocaml). A simple build would look something like this:

```sh
cd ocaml
./configure -prefix `pwd` # put your preferred directory
make world.opt
make install
```

The patched compiler is installed locally into your `$(pwd)/bin`
directory. To start using it temporarily, check if `ocamlc.opt` and `ocamlopt.opt` exist in `$(pwd)/bin`, and temporarily add the location to your `$(PATH)` (e.g. - `PATH=$(pwd)/bin:$PATH`).

### 2. Installing BuckleScript

The following directions assume you already have the correct version of `ocamlopt.opt` in your `$PATH`, having followed the process described in the previous section.

Download the source from the [repository](https://github.com/bloomberg/bucklescript/releases). Then run the following commands:

```sh
cd ./jscomp
make world
```
*******

At the end, you should have a binary called `bsc` under `jscomp/bin` directory,
which you can add to your `$PATH`. You could also set an environment variable
pointing to the stdlib, e.g. `BSC_LIB=/path/to/jscomp/stdlib` for ease of use.

Note that by default, `bsc` will generate `commonjs` modules, you can
override such behavior by picking your own module system:

```sh
MODULE_FLAGS='-bs-module amdjs' make world
MODULE_FLAGS='-bs-module commonjs' make world
MODULE_FLAGS='-bs-module goog:buckle' make world
```

Also see [Create a simple example with npm](./Create-a-simple-example-with-NPM.md)
