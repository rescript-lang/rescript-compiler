# Contributing

Thanks for your help! Due to ReScript's nature, the contribution setup isn't all straightforward. If something isn't working, please file an issue!

## Prerequisites

- [NodeJS](https://nodejs.org/)
- C compiler toolchain (you probably already have it installed)
- OS: Mac/Linux (ReScript works on Windows, but developing the repo using Windows isn't tested. Contribution welcome!)

## Build

```sh
git submodule update --init # we vendor a fork of ocaml and a syntax repo
node scripts/buildocaml.js # buid the vendored ocaml compiler
npm install # install some JS tools for testing purposes
./scripts/ninja.js config # the repo is build with Ninja. Generate the ninja build files
./scripts/ninja.js build # runs `ninja` under the hood against the generated ninja build files
```

Whenever you edit a file, run `./scripts/ninja.js build` to rebuild. Optional watcher to auto-rebuild on file changes: `node scripts/tasks.js`.

In the rare case there you're making changes to the vendored OCaml fork, rebuild the fork with `node scripts/buildocaml.js` then run `./scripts/ninja.js cleanbuild`. `cleanbuild` (aka `clean` + `build`) is necessary since the binary artifacts between versions of compiler may be incompatible.

### Troubleshoot Broken Build

```sh
./scripts/ninja.js clean # remove files not in version control
./scripts/ninja.js config
./scripts/ninja.js build
```

If this doesn't work (rare), then:
- Save your changes
- `git clean -xdf .` to wipe all artifacts
- Then do a clean build as instructed above

## Test

```sh
./darwin/bsc.exe myTestFile.ml
```

(`./linux/bsc` for linux developers).

> Note: production, only `path/to/bsc myTestFile.ml` is needed. During development, you need to pass the `-I jscomp/runtime/` flag for various reasons (e.g. to avoid cyclic dependencies).

Tips:
- To get a nice stack trace when you debug type errors from running `bsc`/`bsb`, uncomment the conditional compilation check in [`rescript_compiler_main.ml`](https://github.com/rescript-lang/rescript-compiler/blob/496c70d1d4e709c26dba23629e430dc944bd59f9/jscomp/main/rescript_compiler_main.ml#L501).

### Integration Test

If you'd like to use your modified ReScript like an end-user, try:

```sh
npm uninstall -g bs-platform # a cache-busting uninstall is needed, but only for npm >=7
BS_TRAVIS_CI=1 npm install -g .
```

Then go somewhere and create a dummy project:

```sh
bsb -init foo -theme basic
cd foo
npm run build
```

## Editor Support for Developing This Repo

This is hard to set up and therefore not entirely encouraged. Use this deprecated [VSCode extension](https://marketplace.visualstudio.com/items?itemName=hackwaly.ocaml).
The extension requires using an opam switch for ocaml 4.02.3, with `merlin` and `ocp-indent` are installed.

## Contribute to the Documentation

See https://github.com/reason-association/rescript-lang.org

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

## Contributing to the Runtime

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

## Contribute to the BS Playground Bundle

> Note: These instructions are designed for building the 4.06 based version of ReScript (ReScript v6).

The "Playground bundle" is the BS compiler compiled to JavaScript, including all necessary dependency files (stdlib / belt etc). It is useful for building tools where you want to compile and execute arbitrary Reason / OCaml in the browser.

The ReScript source code is compiled with a tool called [JSOO (js_of_ocaml)](https://ocsigen.org/js_of_ocaml/3.5.1/manual/overview), which uses OCaml bytecode to compile to JavaScript and is part of the bigger OCaml ecosystem. Before we can compile anything, we need to install the required tools (requires [`opam`](https://opam.ocaml.org/doc/Install.html) to be installed):

```sh
# Create the right switch, if not created yet (first install)
opam switch create 4.06.1

# Makes sure to be on the right switch
opam switch 4.06.1
eval `opam config env`

opam install js_of_ocaml.3.5.1
```

### Build the Bundle

The entry point of the JSOO bundle is located in `jscomp/main/jsoo_main.ml` and the script for running JSOO can be found in `scripts/repl.js`. A full clean build can be done like this:

```
# We create a target directory for storing the bundle / stdlib files
mkdir playground && mkdir playground/stdlib

# We build the ReScript source code and also the bytecode for jsoo_main.ml
node scripts/ninja.js config && node scripts/ninja.js build

# Now we run the repl.js script pointing to our playground directory (note how it needs to be relative to the repl.js file)
BS_PLAYGROUND=../playground node scripts/repl.js
```

_Troubleshooting: if ninja build step failed with `Error: cannot find file '+runtime.js'`, make sure `ocamlfind` is installed with `opam install ocamlfind`._

**You should now find following files:**

- `playground/exports.js` -> This is the ReScript compiler, which binds the ReScript API to the `window` object.
- `playground/stdlib/*.js` -> All the ReScript runtime files.

You can now use the `exports.js` file either directly by using a `<script src="/path/to/exports.js"/>` inside a html file, use a browser bundler infrastructure to optimize it, or you can even use it with `nodejs`:

```
$ node
> require("./exports.js");
undefined
> let compile_result = ocaml.compile(`Js.log Sys.ocaml_version`); // You can change the code here
undefined
> eval(compile_result);
4.06.2+BS
undefined
```

### Playground JS bundle API

As soon as the bundle is loaded, you will get access to following functions (as seen in [`jsoo_main.ml`](jscomp/main/jsoo_main.ml)):

- `window.ocaml`:
  - `compile(code: string)`: Compiles given code
  - `shake_compile(code: string)`: Compiles given code with tree-shaking
  - `compile_super_errors(code: string)`: Compiles given code and outputs `super_errors` related messages on `console.error`
  - `compile_super_errors_ppx_v2(code: string)`: Compiles given code with the React v2 syntax
  - `compile_super_errors_ppx_v3(code: string)`: Compiles given code with the React v3 syntax
  - `load_module(cmi_path: string, cmi_content: string, cmj_name: string, cmj_content: string)`: Loads a module into the compiler (see notes on `cmj` / `cmi` below)

For each compile every successful operation will return `{js_code: string}`. On compile errors, the returned object will be `{js_error_msg: string}`.

### Working on the Playground JS API

Whenever you are modifying any files in the ReScript compiler, or in the `jsoo_main.ml` file, you'll need to rebuild the source and recreate the JS bundle.

```sh
node scripts/ninja.js config && node scripts/ninja.js build
BS_PLAYGROUND=../playground node scripts/repl.js
```

**.cmj files in the Web**

A `.cmj` file contains compile information and JS package information of ReScript build artifacts (your `.re / .ml` modules) and are generated on build (`scripts/ninja.js build`).

A `.cmi` file is an [OCaml originated file extension](https://waleedkhan.name/blog/ocaml-file-extensions/) and contains all interface information of a certain module without any implementation.

In this repo, these files usually sit right next to each compiled `.ml` / `.re` file. The structure of a `.cmj` file is defined in [js_cmj_format.ml](jscomp/core/js_cmj_format.ml). You can run a tool called `./jscomp/bin/cmjdump.exe [some-file.cmj]` to inspect the contents of given `.cmj` file.

`.cmj` files are required for making ReScript compile modules (this includes modules like ReasonReact). ReScript includes a subset of modules by default, which can be found in `jscomp/stdlib-406` and `jscomp/others`. You can also find those modules listed in the `jsoo` call in `scripts/repl.js`. As you probably noticed, the generated `playground` files are all plain `.js`, so how are the `cmj` / `cmi` files embedded?

`repl.js` calls an executable called `cmjbrowser.exe` on every build, which is a compile artifact from `jscomp/main/jscmj_main.ml`. It is used to serialize `cmj` / `cmi` artifacts into two files called `jscomp/core/js_cmj_datasets.ml`. These files are only linked for the browser target, where ReScript doesn't have access to the filesystem. When working on BS, you'll see diffs on those files whenever there are changes on core modules, e.g. stdlib modules or when the ocaml version was changed. We usually check in these files to keep it in sync with the most recent compiler implementation. JSOO will pick up those files to encode them into the `exports.js` bundle.

For any other dependency needed in the playground, such as `ReasonReact`, you will be required to serialize your `.cmi` / `.cmt` files accordingly from binary to hex encoded strings so that BS Playground's `ocaml.load` function can load the data. Right now we don't provide any instructions inside here yet, but [here's how the official ReasonML playground did it](https://github.com/reasonml/reasonml.github.io/blob/source/website/setupSomeArtifacts.js#L65).

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
