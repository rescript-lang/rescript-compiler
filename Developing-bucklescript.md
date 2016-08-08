# Developing BuckleScript

## Building BuckleScript from Source

#### 1. Recursively clone the BuckleScript repository

  ```sh
  git clone https://github.com/bloomberg/bucklescript.git --recursive
  ```

#### 2. Build the patched OCaml toolchain

  ```sh
  cd ./ocaml
  git checkout master
  ./configure -prefix `pwd`
  make world.opt
  make install
  export PATH=$(cwd)/bin:$PATH
  ```

  This install patched binaries into `./bin` and adds them to your current command path.

#### 3. Build the BuckleScript compiler

  Make sure you have `ocamlopt.opt` in your `$PATH` from the previous step, then do the following:

  ```sh
  cd ./jscomp
  npm_package_name=bs-platform make world
  export PATH=$(cwd)/jscomp/bin:$PATH
  ```

  This install the compiler `bsc` into `./jscomp/bin` and adds it to your current command path.

  **NOTE**: the dev build of `bsc` only supports `commonjs` modules. You can enable the other module
  systems by building with `BS_RELEASE_BUILD=1`:

  ```sh
  BS_RELEASE_BUILD=1 npm_package_name=bs-platform make world
  ```

## Check that the BuckleScript compiler works

  Create a simple OCaml program `bs-hello/hello.ml` in a directory adjacent to the `bucklescript`:

  ```sh
  cd .. # if you are still in the bucklescript directory
  mkdir bs-hello
  cd bs-hello
  echo 'print_endline "hello world";;' > hello.ml
  ```

  Next, compile it with `bsc`, making sure to include the BuckleScript `runtime` and `stdlib`:

  ```sh
  bsc -I ../bucklescript/jscomp/runtime -I ../bucklescript/jscomp/stdlib -c hello.ml
  ```

  You should now have a file `bs-hello/hello.js`. You can run it with Node or any other JavaScript engine:

  ```sh
  node hello.js
  ```

  If everything goes well, you will see `hello world` on your screen.
