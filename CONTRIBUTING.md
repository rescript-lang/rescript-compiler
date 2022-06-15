# Contributing

Welcome to the ReScript compiler project!

This document will give you guidance on how to get up and running to work on the ReScript compiler and toolchain.

(If you want to contribute to the documentation website, check out [rescript-association/rescript-lang.org](https://github.com/reason-association/rescript-lang.org). For contributions to the ReScript syntax, please visit the [rescript-lang/syntax](https://github.com/rescript-lang/syntax) project.)

We tried to keep the installation process as simple as possible. In case you are having issues or get stuck in the process, please let us know in the issue tracker.

Happy hacking!

## Setup

> Most of our contributors are working on Apple machines, so all our instructions are currently MacOS / Linux centric. Contributions for Windows development welcome!

- [NodeJS v16](https://nodejs.org/)
- C compiler toolchain (usually installed with `xcode` on Mac)
- `opam` (OCaml Package Manager)
- VSCode (+ [OCaml Platform Extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform))

## Install native OCaml compiler, dune, testing utilities

The ReScript compiler compiles with any recent OCaml compiler. We are using `dune` as a build system for easy workflows and proper IDE support.

Make sure you have [opam](https://opam.ocaml.org/doc/Install.html) installed on your machine.

```
opam init

# Install build system
opam install dune

# Install language server for IDE support
opam install ocaml-lsp-server

# Any recent OCaml version works as a development compiler
opam switch create 4.12.1

# We use NodeJS to run our test suites and other utilities.
npm install
```

## Initialize submodules

After a fresh checkout or deep clean, do:

```
git submodule init
git submodule update
```

## Configure and Build the Compiler (`ninja` workflow)

> Note: These instructions allow you to do full builds of the project. In case you only want to build the project for development purposes, you can use the `dune` workflow.

The ReScript project is built with a vendored version of `ninja`. It requires build files to correctly detect, compile and link all the OCaml files within our project. The build files are generated and managed by a NodeJS script (`./scripts/ninja.js`).

```sh
# Generate all the necessary ninja build files
./scripts/ninja.js config 

# Run ninja to read and execute the generated build files
./scripts/ninja.js build 

# Clean (remove) all ninja build files
./scripts/ninja.js clean
```

Whenever you edit a file, run `./scripts/ninja.js build` to rebuild the ReScript compiler. There's also an optional watcher to auto-rebuild on file changes: `node scripts/tasks.js`.

## Build the Project during Development (`dune` workflow) 

When working on a project you may want to use `dune` to compile all the files you've been working on. This is especially important for full IDE support, including auto-completion and showing compilation errors.

```
# One off build
dune build

# Watch mode
dune build -w
```

> Please note that `dune` will not build the final `rescript` binaries. Use the aforementioned `ninja` workflow if you want to build, test and distribute the final product.

### Troubleshoot a Broken Build

Usually whenever there's some issues with missing files, incompatible interfaces or stale artifacts, the easiest fix is to clean and rebuild the project:

```sh
./scripts/ninja.js clean # remove files not in version control
./scripts/ninja.js config
./scripts/ninja.js build
```

If this doesn't work (rare), then:
- Save your changes
- `git clean -xdf .` to wipe all artifacts
- Then do a clean build as instructed above

## Running tests for independent ReScript files

The simplest way for running tests is to run your locally built compiler on separate ReScript files:

```sh
# Make sure to rebuild the compiler before running any tests (./scripts/ninja.js config / build etc)
./darwinarm64/bsc.exe myTestFile.res
```

**Different architectures:**

- `darwinarm64/bsc.exe`: M1 Macs
- `darwin/bsc.exe`: Intel Macs
- `linux/bsc.exe`: Linux computers

### Testing the whole ReScript Package

If you'd like to bundle up and use your modified ReScript like an end-user, try:

```sh
node scripts/install -force-lib-rebuild # make sure lib/ocaml is populated

npm uninstall -g rescript # a cache-busting uninstall is needed, but only for npm >=7

# This will globally install your local build via npm
RESCRIPT_FORCE_REBUILD=1 npm install -g .
```

Then you may initialize and build your ReScript project as usual:

```sh
rescript init my-project
cd my-project
npm run build
```

### Running Automatic Tests

We provide different test suites for different levels of the compiler and build system infrastructure. Always make sure to locally build your compiler before running any tests.

**Run Mocha tests for our runtime code:**

This will run our `mocha` unit test suite defined in `jscomp/test`.

```
npx node scripts/ciTest.js -mocha
```

**Run build system test (integration tests):**

This will run the whole build system test suite defined in `jscomp/build_tests`.

```
# Make sure to globally install rescript via npm first
npm install -g .

npx node scripts/ciTest.js -bsb
```

**Run ounit tests:**

This will run unit tests for compiler related modules. The tests can be found in `jscomp/ounit_tests`.

```
npx node scripts/ciTest.js -ounit
```

## Contributing to the ReScript Runtime

Our runtime implementation is written in pure OCaml with some raw JS code embedded (`jscomp/runtime` directory).

The goal is to implement the runtime **purely in OCaml**. This includes removing all existing occurrences of embedded raw JS code as well, and you can help!

Each new PR should include appropriate testing.

Currently all tests are located in the `jscomp/test` directory and you should either add / update test files according to your changes to the compiler.

There are currently two formats for test files:

1. Proper mocha test files with executed javascript test code
2. Plain `.ml` files which are only supposed to be compiled to JS (without any logic validation)

Below we will discuss on how to write, build and run these test files.

### 1) Write a Mocha Test File

- Create a file `jscomp/test/feature_abc_test.ml`. Make sure to end the file name with `_test.ml`.
- Inside the file, add a mocha test suite. The mocha bindings are defined in `jscomp/test/mt.ml`. To get you started, here is a simple scaffold for a test suite with multiple test cases:

  ```ocaml
  let suites : _ Mt.pair_suites =
     ["hey", (fun _ -> Eq(true, 3 > 2));
      "hi", (fun _ -> Neq(2,3));
      "hello", (fun _ -> Approx(3.0, 3.0));
      "throw", (fun _ -> ThrowAny(fun _ -> raise 3))
     ]
  let () = Mt.from_pair_suites __FILE__ suites
  ```

- Build the test files: `node scripts/ninja.js clean && node scripts/ninja.js build`.
- Run the tests: `npx mocha jscomp/test/**/*test.js`.

### 2) Write a Plain `.ml` Test File

This is usually the file you want to create to test certain compile behavior without running the JS code formally as a test, i.e. if you add a new type alias to a specific module and you just want to make sure the compiler handles the types correctly (see [`jscomp/test/empty_obj.ml`](jscomp/test/empty_obj.ml) as an example).

- Create your test file `jscomp/test/my_file_test.ml`. Make sure to end the file name with `_test.ml`.
- Build the `.js` artifact: `node scripts/ninja.js config && node scripts/ninja.js build`.
- Verify the output, check in the `jscomp/test/my_file_test.ml` and `jscomp/test/my_file_test.js` to version control. The checked in `.js` file is essential for verifying regressions later on.
- Eventually check in other relevant files changed during the rebuild (depends on your compiler changes).

## Contribute to the ReScript Playground Bundle

> Note: These instructions are designed for building the 4.06 based version of ReScript (ReScript v6).

The "Playground bundle" is a JS version of the ReScript compiler; including all necessary dependency files (stdlib / belt etc). It is useful for building tools where you want to compile and execute arbitrary ReScript code in the browser.

The ReScript source code is compiled with a tool called [JSOO (js_of_ocaml)](https://ocsigen.org/js_of_ocaml/4.0.0/manual/overview), which uses OCaml bytecode to compile to JavaScript and is part of the bigger OCaml ecosystem.

Install `jsoo` via `opam`:

```sh
opam install js_of_ocaml.4.0.0
```

### Building the Bundle

The entry point of the JSOO bundle is located in `jscomp/main/jsoo_playground_main.ml`, the code for packing the compiler into a single compiler file is located in `jscomp/snapshot.ninja`, and the script for running JSOO can be found in `scripts/repl.js`. A full clean build can be done like this:

```
# We create a target directory for storing the bundle / stdlib files
mkdir playground && mkdir playground/stdlib

# We build the ReScript source code and also the bytecode for the JSOO entrypoint
node scripts/ninja.js config && node scripts/ninja.js build

# Now we run the repl.js script pointing to our playground directory (note how it needs to be relative to the repl.js file)
PLAYGROUND=../playground node scripts/repl.js
```

_Troubleshooting: if ninja build step failed with `Error: cannot find file '+runtime.js'`, make sure `ocamlfind` is installed with `opam install ocamlfind`._

After a successful compilation, you will find following files in your project:

- `playground/exports.js` -> This is the ReScript compiler, which binds the ReScript API to the `window` object.
- `playground/stdlib/*.js` -> All the ReScript runtime files.

You can now use the `exports.js` file either directly by using a `<script src="/path/to/exports.js"/>` inside a html file, use a browser bundler infrastructure to optimize it, or you can even use it with `nodejs`:

```
$ node
> require("./exports.js");
> let compiler = rescript_compiler.make()
> let result = compiler.rescript.compile(`Js.log(Sys.ocaml_version)`);
> eval(result.js_code);
4.06.2+BS
```

### Playground JS bundle API

As soon as the bundle is loaded, you will get access to the functions exposed in [`jsoo_playground_main.ml`](jscomp/main/jsoo_playground_main.ml). Best way to check out the API is by inspecting a compiler instance it either in node, or in the browser:

```
$ node
require('./exports.js')

> let compiler = rescript_compiler.make()
> console.log(compiler)
```

### Working on the Playground JS API

Whenever you are modifying any files in the ReScript compiler, or in the `jsoo_playground_main.ml` file, you'll need to rebuild the source and recreate the JS bundle.

```sh
node scripts/ninja.js config && node scripts/ninja.js build
PLAYGROUND=../playground node scripts/repl.js
```

**.cmj files in the Web**

A `.cmj` file contains compile information and JS package information of ReScript build artifacts (your `.res / .ml` modules) and are generated on build (`scripts/ninja.js build`).

A `.cmi` file is an [OCaml originated file extension](https://waleedkhan.name/blog/ocaml-file-extensions/) and contains all interface information of a certain module without any implementation.

In this repo, these files usually sit right next to each compiled `.ml` / `.res` file. The structure of a `.cmj` file is defined in [js_cmj_format.ml](jscomp/core/js_cmj_format.ml). You can run a tool called `./jscomp/bin/cmjdump.exe [some-file.cmj]` to inspect the contents of given `.cmj` file.

`.cmj` files are required for making ReScript compile modules (this includes modules like ReasonReact). ReScript includes a subset of modules by default, which can be found in `jscomp/stdlib-406` and `jscomp/others`. You can also find those modules listed in the `jsoo` call in `scripts/repl.js`. As you probably noticed, the generated `playground` files are all plain `.js`, so how are the `cmj` / `cmi` files embedded?

`repl.js` calls an executable called `cmjbrowser.exe` on every build, which is a compile artifact from `jscomp/main/jscmj_main.ml`. It is used to serialize `cmj` / `cmi` artifacts into two files called `jscomp/core/js_cmj_datasets.ml`. These files are only linked for the browser target, where ReScript doesn't have access to the filesystem. When working on BS, you'll see diffs on those files whenever there are changes on core modules, e.g. stdlib modules or when the ocaml version was changed. We usually check in these files to keep it in sync with the most recent compiler implementation. JSOO will pick up those files to encode them into the `exports.js` bundle.

For any other dependency needed in the playground, such as `ReasonReact`, you will be required to serialize your `.cmi` / `.cmt` files accordingly from binary to hex encoded strings so that BS Playground's `ocaml.load` function can load the data. Right now we don't provide any instructions inside here yet, but [here's how the official ReasonML playground did it](https://github.com/reasonml/reasonml.github.io/blob/source/website/setupSomeArtifacts.js#L65).

## Contribute to the API Reference

The API reference is generated from doc comments in the source code. [Here](https://github.com/rescript-lang/rescript-compiler/blob/99650/jscomp/others/js_re.mli#L146-L161)'s a good example.

Some tips:

- The first sentence or line should be a very short summary. This is used in indexes and by tools like merlin.
- Ideally, every function should have **at least one** `@example`.
- Cross-reference another definition with `{! identifier}`. But use them sparingly, they’re a bit verbose (currently, at least).
- Wrap non-cross-referenced identifiers and other code in `[ ... ]`.
- Escape `{`, `}`, `[`, `]` and `@` using `\`.
- It’s possible to use `{%html ...}` to generate custom html, but use this very, very sparingly.
- A number of "documentation tags" are provided that would be nice to use, but unfortunately they’re often not supported for \`external\`s. Which is of course most of the API.
- `@param` usually doesn’t work. Use `{b <param>} ...` instead
- `@returns` usually doesn’t work. Use `{b returns} ...` instead.
- Always use `@deprecated` when applicable.
- Always use `@raise` when applicable.
- Always provide a `@see` tag pointing to MDN for more information when available.

See [Ocamldoc documentation](http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec333) for more details.

To generate the html:

```sh
../scripts/ninja docs
```

## Code structure

The highlevel architecture is illustrated as below:

```
Source Language
  |
  | (Parser)
  v
Surface Syntax Tree
  |
  | (Built-in Syntax tree transformation)
  v
Surface Syntax Tree
  |
  | (Reuse OCaml Type checker)
  v
Typedtree
  |
  | (Reuse OCaml pattern match compiler and erase types)
  v
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

Note that there is one design goal to keep in mind, never introduce any meaningless symbol unless necessary, we do optimizations, however, it should also compile readable output code.

## Contribution Licensing

Since ReScript is distributed under the terms of the [LGPL Version 3](LICENSE), contributions that you make are licensed under the same terms. In order for us to be able to accept your contributions, we will need explicit confirmation from you that you are able and willing to provide them under these terms, and the mechanism we use to do this is called a Developer's Certificate of Origin [DCO](DCO.md). This is very similar to the process used by the Linux(R) kernel, Samba, and many other major open source projects.

To participate under these terms, all that you must do is include a line like the following as the last line of the commit message for each commit in your contribution:

    Signed-Off-By: Random J. Developer <random@developer.example.org>

You must use your real name (sorry, no pseudonyms, and no anonymous contributions).
