# Contributing

Welcome to the ReScript compiler project!

This document will give you guidance on how to get up and running to work on the ReScript compiler and toolchain.

(If you want to contribute to the documentation website, check out [rescript-association/rescript-lang.org](https://github.com/reason-association/rescript-lang.org).)

We tried to keep the installation process as simple as possible. In case you are having issues or get stuck in the process, please let us know in the issue tracker.

Happy hacking!

## Setup

> Most of our contributors are working on Apple machines, so all our instructions are currently macOS / Linux centric. Contributions for Windows development welcome!

- [NodeJS v16](https://nodejs.org/)
- C compiler toolchain (usually installed with `xcode` on Mac)
- `opam` (OCaml Package Manager)
- VSCode (+ [OCaml Platform Extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform))

## Cloning the Git Repo

The rescript-compiler git repo is very large because, prior to tooling improvements made for ReScript 11 development, build artifacts were checked in to the repo ("snapshotted"). Therefore, cloning the repo in full will consume a lot of bandwidth and disk space (> 2GB).

If you are only interested in the latest master commit, you can perform a shallow clone instead as follows:

```sh
git clone --depth 1 https://github.com/rescript-lang/rescript-compiler.git
```

This will only consume less than 50MB.

## Install OCaml compiler + dependencies

The ReScript compiler compiles with any recent OCaml compiler. We are using `dune` as a build system for easy workflows and proper IDE support.

Make sure you have [opam](https://opam.ocaml.org/doc/Install.html) installed on your machine.

```sh
opam init

# Any recent OCaml version works as a development compiler
opam switch create 4.14.0 # can also create local switch with opam switch create . 4.14.0

# Install dev dependencies from OPAM
opam install . --deps-only

# For IDE support, install the OCaml language server
opam install ocaml-lsp-server
```

## npm install

Run `npm install --ignore-scripts`. This will install the npm dependencies required for the build scripts.

## Building the Compiler

The compiler binaries themselves can be built directly with dune as follows:

```sh
# One off build
dune build

# Watch mode
dune build -w
```

For all additional operations, a Makefile is provided:

```sh
# Build the compiler using dune and copy the exes into the platform dir
make

# Build the ninja build tool
make ninja

# Build the ReScript standard library using ninja and the compiler
make lib

# Run compiler tests
make test

# Run syntax tests
make test-syntax

# Run syntax tests including roundtrip tests
make test-syntax-roundtrip

# Populate lib/ocaml and update artifact list
make artifacts
```

## Adding new Files to the Npm Package

To make sure that no files are added to or removed from the npm package inadvertently, an artifact list is kept at `packages/artifacts.txt`. During CI build, it is verified that only the files that are listed there are actually included in the npm package.

After adding a new file to the repository that should go into the npm package - e.g., a new stdlib module -, run `make artifacts`.

## Test the compiler

### Single file

```sh
make lib # Build compiler and standard library
./bsc myTestFile.res
```

### Project

```sh
make artifacts # Build compiler and standard library and populate lib/ocaml
npm link
cd myProject
npm install
npm link rescript
```

### Running Automatic Tests

We provide different test suites for different levels of the compiler and build system infrastructure. Always make sure to locally build your compiler before running any tests.

To run all tests:

```sh
make test
```

**Run Mocha tests only (for our runtime code):**

This will run our `mocha` unit test suite defined in `jscomp/test`.

```
node scripts/ciTest.js -mocha
```

**Run build system test (integration tests):**

This will run the whole build system test suite defined in `jscomp/build_tests`.

```
node scripts/ciTest.js -bsb
```

**Run ounit tests:**

This will run unit tests for compiler related modules. The tests can be found in `jscomp/ounit_tests`.

```
node scripts/ciTest.js -ounit
```

## Contributing to the Runtime

The runtime implementation is written in OCaml with some raw JS code embedded (`jscomp/runtime` directory).

The goal is to implement the runtime **purely in OCaml**. This includes removing all existing occurrences of embedded raw JS code as well whenever possible, and you can help!

Each new PR should include appropriate testing.

Currently all tests are located in the `jscomp/test` directory and you should either add / update test files according to your changes to the compiler.

There are currently two formats for test files:

1. Mocha test files that run javascript test code
2. Plain `.ml` files to check the result of compilation to JS (expectation tests)

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

# Now we run the repl.js script which will create all the required artifacts in the `./playground` directory
node scripts/repl.js
```

In case you want to build the project with our default third party packages (like `@rescript/react`), prepare the `playground-bundling` project and then run `repl.js` with `BUILD_THIRD_PARTY` enabled:

```
# Prepare the `playground-bundling` project to allow building of the third party cmij packages
npm link
cd packages/playground-bundling
npm install
npm link rescript

BUILD_THIRD_PARTY=true node scripts/repl.js
```

_Troubleshooting: if ninja build step failed with `Error: cannot find file '+runtime.js'`, make sure `ocamlfind` is installed with `opam install ocamlfind`._

After a successful compilation, you will find following files in your project:

- `playground/compiler.js` -> This is the ReScript compiler, which binds the ReScript API to the `window` object.
- `playground/stdlib/*.js` -> All the ReScript runtime files.
- `playground/packages` -> Contains third party deps with cmij.js files (as defined in `packages/playground-bundling/bsconfig.json`)

You can now use the `compiler.js` file either directly by using a `<script src="/path/to/compiler.js"/>` inside a html file, use a browser bundler infrastructure to optimize it, or you can even use it with `nodejs`:

```
$ node
> require("./compiler.js");
> let compiler = rescript_compiler.make()
> let result = compiler.rescript.compile(`Js.log(Sys.ocaml_version)`);
> eval(result.js_code);
4.06.2+BS
```

You can also run `node playground/playground_test.js` for a quick sanity check to see if all the build artifacts are working together correctly.

### Playground JS bundle API

As soon as the bundle is loaded, you will get access to the functions exposed in [`jsoo_playground_main.ml`](jscomp/main/jsoo_playground_main.ml). Best way to check out the API is by inspecting a compiler instance it either in node, or in the browser:

```
$ node
require('./compiler.js')

> let compiler = rescript_compiler.make()
> console.log(compiler)
```

### Working on the Playground JS API

Whenever you are modifying any files in the ReScript compiler, or in the `jsoo_playground_main.ml` file, you'll need to rebuild the source and recreate the JS bundle.

```sh
node scripts/ninja.js config && node scripts/ninja.js build
node scripts/repl.js
```

**.cmj files in the Web**

A `.cmj` file contains compile information and JS package information of ReScript build artifacts (your `.res / .ml` modules) and are generated on build (`scripts/ninja.js build`).

A `.cmi` file is an [OCaml originated file extension](https://waleedkhan.name/blog/ocaml-file-extensions/) and contains all interface information of a certain module without any implementation.

In this repo, these files usually sit right next to each compiled `.ml` / `.res` file. The structure of a `.cmj` file is defined in [js_cmj_format.ml](jscomp/core/js_cmj_format.ml). You can run a tool called `./jscomp/bin/cmjdump.exe [some-file.cmj]` to inspect the contents of given `.cmj` file.

`.cmj` files are required to compile modules (this includes modules like RescriptReact). ReScript includes a subset of modules by default, which can be found in `jscomp/stdlib-406` and `jscomp/others`. You can also find those modules listed in the JSOO call in `scripts/repl.js`. As you probably noticed, the generated `playground` files are all plain `.js`, so how are the `cmj` / `cmi` files embedded?

JSOO offers an `build-fs` subcommand that takes a list of `.cmi` and `.cmj` files and creates a `cmij.js` file that can be loaded by the JS runtime **after** the `compiler.js` bundle has been loaded (either via a `require()` call in Node, or via `<link/>` directive in an HTML file). Since we are shipping our playground with third party modules like `RescriptReact`, we created a utility directory `packages/playground-bundling` that comes with a utility script to do the `cmij.js` file creation for us. Check out `packages/playground-bundling/scripts/generate_cmijs.js` for details.

### Publishing the Playground Bundle on our KeyCDN

> Note: If you want to publish from your local machine, make sure to set the `KEYCDN_USER` and `KEYCDN_PASSWORD` environment variables accordingly (credentials currently managed by @ryyppy). Our CI servers / GH Action servers are already pre-configured with the right env variable values.

Our `compiler.js` and third-party packages bundles are hosted on [KeyCDN](https://www.keycdn.com) and uploaded via FTPS.

After a successful bundle build, run our upload script to publish the build artifacts to our server:

```
playground/upload_bundle.sh
```

The script will automatically detect the ReScript version from the `compiler.js` bundle and automatically create the correct directory structure on our CDN ftp server.

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

## Release Process

To build a new version and release it on NPM, follow these steps:

1. Increment the version number in `package.json`.
1. Run `node scripts/setVersion.js` to take that version number over into other files.
1. Snapshot (run `make` to regenerate `whole_compiler.ml` etc.).
1. Update `CHANGELOG.md`.
1. Create a PR.
1. Once that PR is merged, download the `npm-packages.zip` artifact for that commit from the Github Actions page.
1. Extract `npm-packages.zip` to get the package tarballs to publish.
1. Run the publish commands with `--dry-run` to see if everything (especially the version number) looks good:
   ```sh
   # Use the tag "next" when publishing an alpha/beta version.
   npm publish rescript-<version>.tgz [--tag next] --dry-run
   npm publish rescript-std-<version>.tgz [--tag next] --dry-run
   ```
1. Publish for real:
   ```sh
   # Use the tag "next" when publishing an alpha/beta version.
   npm publish rescript-<version>.tgz [--tag next]
   npm publish rescript-std-<version>.tgz [--tag next]
   ```
1. Tag the commit with the version number (e.g., "10.0.0", or "10.0.0-beta.1") and push the tag.
1. Create a release entry for that tag on the [Github Releases page](https://github.com/rescript-lang/rescript-compiler/releases), copying the changes from `CHANGELOG.md`.
1. Coordinate any forum/blog posts with [@ryyppy](https://github.com/ryyppy).

## Contribution Licensing

Since ReScript is distributed under the terms of the [LGPL Version 3](LICENSE), contributions that you make are licensed under the same terms. In order for us to be able to accept your contributions, we will need explicit confirmation from you that you are able and willing to provide them under these terms, and the mechanism we use to do this is called a Developer's Certificate of Origin [DCO](DCO.md). This is very similar to the process used by the Linux(R) kernel, Samba, and many other major open source projects.

To participate under these terms, all that you must do is include a line like the following as the last line of the commit message for each commit in your contribution:

    Signed-Off-By: Random J. Developer <random@developer.example.org>

You must use your real name (sorry, no pseudonyms, and no anonymous contributions).
