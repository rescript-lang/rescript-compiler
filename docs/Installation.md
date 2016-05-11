## Introduction

To use BuckleScript, you need both, a specific version of the OCaml compiler (tweaked to make BuckleScript possible), and the BuckleScript software itself. The instructions below show how to install both of these using standard package managers as well as how to build from source for Unix/Linux systems. Build directions will be updated once Windows systems are supported.

## Installing the BuckleScript OCaml compiler

### 1. Using opam
You can use the vanilla [opam](https://opam.ocaml.org) package manager, available [here](https://opam.ocaml.org/doc/Install.html), to access the OCaml compiler version necessary to use BuckleScript.
Once you have `opam` installed, ask `opam` to switch to using our version of the compiler:

```sh
opam update
opam switch ocaml-4.02.3+buckle-1
```

### 2. Building from source
To build OCaml compiler from source, download the compiler source [here](https://github.com/bloomberg/ocaml/releases) (version 4.02.3+buckle-1):

For detailed build instructions, follow the directions in the [repo](https://github.com/bloomberg/ocaml). A simple build would look something like this:

```sh
cd ocaml
./configure -prefix `pwd` # put your preferred directory
make world.opt
make install
```

The patched compiler is installed locally into your `$(pwd)/bin`
directory. To start using it temporarily, check if `ocamlc.opt` and `ocamlopt.opt` exist in `$(pwd)/bin`, and temporarily add the location to your `$(PATH)` (e.g. - `PATH=$(pwd)/bin:$PATH`).

## Installing BuckleScript

The following directions assume you already have the correct version of `ocamlopt.opt` in your `$PATH`, having followed the process described in the previous section.

### 1. Using npm

The standard npm package management tool can be used to install BuckleScript. If you don't already have npm installed, follow the directions listed [here](https://docs.npmjs.com/getting-started/installing-node). Once npm is installed, run the following command:

```sh
npm install --save bs-platform
```
  
### 2. Building from source

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
override such behavior by picking up your own module system:

```sh
MODULE_FLAGS='-js-module amdjs' make world
MODULE_FLAGS='-js-module commonjs' make world
MODULE_FLAGS='-js-module goog:buckle' make world
```

Also see [Create a simple example with npm](./Create-a-simple-example-with-npm.md)
