# Contributing

Thanks for your help! Due to BuckleScript's nature, the contribution setup isn't all straightforward (though well documented). If something isn't working, please file an issue or ping us in [Discord](https://discord.gg/reasonml)!

## Setup

Prerequisites:

- [NodeJS](https://nodejs.org/)
- C compiler toolchain (you probably already have it installed)
- OS: Mac/Linux (BuckleScript works on Windows, but developing the repo using Windows isn't tested. Contribution welcome!)

### Build the vendored ocaml compiler

```
git submodule update --init && node scripts/buildocaml.js
```

### Build everything in dev mode using vendored compiler

```
./scripts/ninja.js config && ./scripts/ninja.js build
```

`scripts/ninja.js` will generate many `.ninja` build files inside the `jscomp` directory which will be invoked by `./scripts/ninja.js build`.

### Install JS tools

This is required to have all the necessary tools installed, such as the `mocha`
JS testing framework, etc.

```
npm install
```

### Editor support

Use this deprecated [VSCode extension](https://marketplace.visualstudio.com/items?itemName=hackwaly.ocaml).
The extension requires using an opam switch for ocaml 4.02.3, where `merlin` and `ocp-indent` are installed.

### Building docs

```
./scripts/ninja.js docs
```

#### Edit file and test changes

In general, you'd edit files and rerun `./scripts/ninja.js build`.

We have an optional watcher to auto rebuild on file changes. Suppose you are in `jscomp`:

```sh
node ../scripts/tasks.js
```

### Troubleshooting broken builds

Try to run:

```
./scripts/ninja.js clean # it will remove files not in version control
./scripts/ninja.js config
./scripts/ninja.js build
```

### Advanced: building everything in dev mode using a different compiler

This is primarily used when you need to test the repo with a different OCaml than the vendored one.

For example, you might be testing BuckleScript on the 4.06 OCaml compiler instead of 4.02. Clone our patched [OCaml 4.06](https://github.com/bucklescript/ocaml), then do:

```
git -C ocaml checkout 4.06.1+BS && node ./scripts/buildocaml.js
```

```
./scripts/ninja.js cleanbuild
```

Note: clean up is necessary since the binary artifacts between versions of compiler may be incompatible.

### Using esy in development (experimental)

You can use [esy](https://esy.sh) to develop bucklescript. By default esy will do "out of source builds" which means that all the changes will happen outside the current directory. To be able to bspack the needed files and get sources that you can commit you will have to change the following in the `esy.json`.

```diff
-"buildsInSource": true,
+"buildsInSource": "unsafe",
```

When that is changed you just run the following command it will build everything you need.

    esy

By default we depend on the 4.02.3+BS OCaml compiler. To be able to build with the 4.06.1+BS compiler we have a `4061.json` file that just extends the `esy.json` and overrides the OCaml dependency. To use this file instead you simply run this command instead.

    esy @4061

If there are problems building try to run one of the following depending on what you're building

```
esy clean
esy @4061 clean
```

## Test on a Dummy Project

Go somewhere else and do this:

```
bsb -init foo -theme basic-reason
cd foo
npm run build
```

And whenever you modify a file in bucklescript, run this:

```
npm install -g .
```

## Change the Vendored OCaml Compiler

This section is reserved for when you're making a change to the vendored ocaml compiler itself, in `ocaml`, and then testing on super-errors changes at the same time. If you're doing this for whatever reason, then the previous quick iteration workflow wouldn't work. Here's what you have to do after each change:

```
# at project root
cd ocaml && make -j9 world.opt && make install && cd ..
./scripts/ninja.js cleanbuild
```

## Contributing to the runtime

BuckleScript runtime implementation is written in pure OCaml with some raw JS
code embedded (`jscomp/runtime` directory).

The goal is to implement the runtime **purely in OCaml**. This includes
removing all existing occurrences of embedded raw JS code as well, and you can
help!

Each new PR should include appropriate testing.

Currently all tests are located in the `jscomp/test` directory and you should
either add / update test files according to your changes to the compiler.

There are currently two formats for test files:

1. Proper mocha test files with executed javascript test code
2. Plain `.ml` files which are only supposed to be compiled to JS (without any logic validation)

Below we will discuss on how to write, build and run these test files.

### 1) Writing a mocha test file

- Create a file `jscomp/test/feature_abc_test.ml`. Make sure to end the file name with `_test.ml`.
- Inside the file, add a mocha test suite. The mocha bindings are defined in
  `jscomp/test/mt.ml`. To get you started, here is a simple scaffold for a test
  suite with multiple test cases:

  ```ocaml
  let suites : _ Mt.pair_suites =
     ["hey", (fun _ -> Eq(true, 3 > 2));
      "hi", (fun _ -> Neq(2,3));
      "hello", (fun _ -> Approx(3.0, 3.0));
      "throw", (fun _ -> ThrowAny(fun _ -> raise 3))
     ]
  let () = Mt.from_pair_suites __FILE__ suites
  ```

- Build the test files:
  `node scripts/ninja.js clean && node scripts/ninja.js build`
- Run the tests:
  `npx mocha jscomp/test/**/*test.js`

### 2) Writing a plain `.ml` test file

This is usually the file you want to create to test certain compile behavior
without running the JS code formally as a test, i.e. if you add a new type
alias to a specific module and you just want to make sure the compiler handles
the types correctly (see
[`jscomp/test/empty_obj.ml`](jscomp/test/empty_obj.ml) as an example).

- Create your test file `jscomp/test/my_file_test.ml`. Make sure to end the
  file name with `_test.ml`.

- Build the `.js` artifact: `node scripts/ninja.js config && node scripts/ninja.js build`
- Verify the output, check in the `jscomp/test/my_file_test.ml` and `jscomp/test/my_file_test.js` to
  version control. The checked in `.js` file is essential for verifying
  regressions later on.
- Eventually check in other relevant files changed during the rebuild (depends on
  your compiler changes)

## Contributing to the BS Playground Bundle

> Note: These instructions are designed for building the 4.06 based version of BuckleScript (BS v6)

The "BuckleScript Playground bundle" is the BS compiler compiled to JavaScript, including all necessary dependency files (stdlib / belt etc).
It is useful for building tools where you want to compile and execute arbitrary Reason / OCaml in the browser.

The BuckleScript source code is compiled with a tool called [JSOO (js_of_ocaml)](https://ocsigen.org/js_of_ocaml/3.5.1/manual/overview), which uses OCaml
bytecode to compile to JavaScript and is part of the bigger OCaml ecosystem. Before we can compile anything, we need to install the required
tools (requires [`opam`](https://opam.ocaml.org/doc/Install.html) to be installed):

```
# Create the right switch, if not created yet (first install)
opam switch create 4.06.1

# Makes sure to be on the right switch
opam switch 4.06.1
eval `opam config env`

opam install js_of_ocaml.3.5.1
```

### Building the bundle

The entry point of the JSOO bundle is located in `jscomp/main/jsoo_main.ml` and the script for running JSOO can be found in `scripts/repl.js`.
A full clean build can be done like this:

```
# We create a target directory for storing the bundle / stdlib files
mkdir playground && mkdir playground/stdlib

# We build the BuckleScript source code and also the bytecode for jsoo_main.ml
node scripts/ninja.js config && node scripts/ninja.js build

# Now we run the repl.js script pointing to our playground directory (note how it needs to be relative to the repl.js file)
BS_PLAYGROUND=../playground node scripts/repl.js
```

_Troubleshooting: if ninja build step failed with `Error: cannot find file '+runtime.js'`, make sure `ocamlfind` is installed with `opam install ocamlfind`._

**You should now find following files:**

- `playground/exports.js` -> This is the BuckleScript compiler, which binds the BuckleScript API to the `window` object
- `playground/stdlib/*.js` -> All the BuckleScript runtime files

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

For each compile every successful operation will return `{js_code: string}`.
On compile errors, the returned object will be `{js_error_msg: string}`.

### Working on the Playground JS API

Whenever you are modifying any files in the BuckleScript compiler, or in the `jsoo_main.ml` file, you'll need to rebuild the source and recreate the JS bundle.

```
node scripts/ninja.js config && node scripts/ninja.js build
BS_PLAYGROUND=../playground node scripts/repl.js
```

**.cmj files in the Web**

A `.cmj` file contains compile information and JS package information of
BuckleScript build artifacts (your `.re / .ml` modules) and are generated on
build (`scripts/ninja.js build`).

A `.cmi` file is an [OCaml originated file
extension](https://waleedkhan.name/blog/ocaml-file-extensions/) and contains all
interface information of a certain module without any implementation.

In this repo, these files usually sit right next to each compiled `.ml` / `.re`
file. The structure of a `.cmj` file is defined in
[js_cmj_format.ml](jscomp/core/js_cmj_format.ml). You can run a tool called
`./jscomp/bin/cmjdump.exe [some-file.cmj]` to inspect the contents of given
`.cmj` file.

`.cmj` files are required for making BuckleScript compile modules (this includes
modules like ReasonReact). BuckleScript includes a subset of modules by default,
which can be found in `jscomp/stdlib-406` and `jscomp/others`. You can also find
those modules listed in the `jsoo` call in `scripts/repl.js`. As you probably
noticed, the generated `playground` files are all plain `.js`, so how are the `cmj` /
`cmi` files embedded?

`repl.js` calls an executable called `cmjbrowser.exe` on every build, which is a
compile artifact from `jscomp/main/jscmj_main.ml`. It is used to serialize `cmj`
/ `cmi` artifacts into two files called `jscomp/core/js_cmj_datasets.ml`. These files are only linked for the browser
target, where BuckleScript doesn't have access to the filesystem. When working
on BS, you'll see diffs on those files whenever there are changes on core
modules, e.g. stdlib modules or when the ocaml version was changed. We usually
check in these files to keep it in sync with the most recent compiler
implementation. JSOO will pick up those files to encode them into the `exports.js`
bundle.

For any other dependency needed in the playground, such as `ReasonReact`, you
will be required to serialize your `.cmi` / `.cmt` files accordingly from binary
to hex encoded strings so that BS Playground's `ocaml.load` function can
load the data. Right now we don't provide any instructions inside here yet, but
[here's how the official ReasonML playground did
it](https://github.com/reasonml/reasonml.github.io/blob/source/website/setupSomeArtifacts.js#L65).

## Upgrading the Reason version within BuckleScript

Each BuckleScript release is coupled to a specific Reason syntax version, which currently needs to be updated manually once in a while.

It's important that we need to update two specific files:

- `jscomp/main/refmt_api.ml`: Contains the programmatic interface for the refmt syntax converter (responsible for transforming Reason string code to an OCaml AST) -> Only used in the BuckleScript JS Playground
- `lib/4.06.1/refmt_main3.ml`: The refmt binary used within BuckleScript itself. The `3` corresponds to the corresponding major version of refmt -> Used to build the vendored `refmt`, aka. `bsrefmt`

Both files are generated by using the `jscomp/bin/bspack.exe` binary (which is also built automatically when you build the compiler inside this repository) on the refmt parser. In more detail, `bspack.exe` resolves all dependencies of one specific `.ml` input file, puts them in the right order and copies all the source code with the target input file in one huge `.ml` bundle.

So the two files mentioned above, `refmt_api.ml` and `refmt_main3.ml`, are bspacked within the Reason repository and then checked into the BuckleScript repository (we call this `vendoring` or `snapshotting`).
Here are the instructions on building your own Reason snapshots (make sure you to have everything set up for building the playground bundle first, as mentioned above):

```
# Let's go up one level and clone Reason in a sibling directory next to your `bucklescript` repo
cd ..
git clone https://github.com/facebook/reason

cd reason

# You should already have created this switch by now, see playground build setup instructions in "Contributing to the BS Playground Bundle"
opam switch 4.06.1
opam pin add -y reason .
opam pin add -y rtop .

# Let's do the bspacking process for refmt_api.ml and refmt_binary.ml
cd bspacks

# Initial setup of certain dependencies before we can bspack everthing in one file
./downloadSomeDependencies.sh

# bspack and compile the files
BSPACK_EXE=/path/to/bucklescript/jscomp/bin/bspack.exe ./reason_bspack406.sh
```

Now copy the files to bucklescript and do a rebuild to verify the changes:

```
# still in reason/bspacks directory
cp build/refmt_api.ml ../../bucklescript/jscomp/main/refmt_api.ml
cp build/refmt_binary.ml ../../bucklescript/lib/4.06.1/refmt_main3.ml

# Build the whole compiler
node scripts/ninja.js config && node scripts/ninja.js build

# Build the playground
BS_PLAYGROUND=../playground node scripts/repl.js
```

You should now have the newest `refmt` binary for the actual compiler, and for the playground, a new `playground/exports.js` file with the new Reason version included.

**Important:** Always verify that the updated Reason version is in sync in the
`refmt.exe` and the playground bundle. Use `lib/bsrefmt --version` and for the
playground API `window.reason.version` (not final) to get the bundled
version.

## Contributing to the Documentation

See https://github.com/BuckleScript/bucklescript.github.io

## Contributing to the API Reference

The API reference is generated from doc comments in the source code.
[Here](https://github.com/bucklescript/bucklescript/blob/99650/jscomp/others/js_re.mli#L146-L161)'s a good example

Some tips and guidelines:

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

To generate the html, run `make docs` in `jscomp/`.

Html generation uses a custom generator located in `odoc_gen/` and
custom styles located in `docs/api_static`.

## Make a Release

In release mode, assuming you have NodeJS and OCaml compiler with the right version installed:

```sh
node scripts/install.js
```

The build process will generate configure file with correct `LIBDIR` path,
build all binaries and libraries and
install the binaries into `bin` and lib files into `lib`.

First it will try to generate `bin/config_whole_compiler.ml` based on existing
OCaml installation, if it fails, it will try to invoke `node scripts/buildocaml.js` to
install an OCaml compiler from scratch, and retry again.

### Publish Process

- Run `make force-snapshotml`
- Bump the compiler version

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

## Contribution Licensing

Since BuckleScript is distributed under the terms of the [LGPL Version 3](LICENSE), contributions that you make
are licensed under the same terms. In order for us to be able to accept your contributions,
we will need explicit confirmation from you that you are able and willing to provide them under
these terms, and the mechanism we use to do this is called a Developer's Certificate of Origin
[DCO](DCO.md). This is very similar to the process used by the Linux(R) kernel, Samba, and many
other major open source projects.

To participate under these terms, all that you must do is include a line like the following as the
last line of the commit message for each commit in your contribution:

    Signed-Off-By: Random J. Developer <random@developer.example.org>

You must use your real name (sorry, no pseudonyms, and no anonymous contributions).
