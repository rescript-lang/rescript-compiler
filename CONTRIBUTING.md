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

### Building docs

```
./script/ninja.js docs
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
./configure -prefix `pwd` && make -j9 world.opt && make install
```

Make sure the new compiler (`ocamlopt.opt`) is in your environment:

```
export PATH="/absolute-path-to-the-bucklescript-forked-ocaml-repo/bin:$PATH"
```

Then:

```
./scripts/ninja.js -env && ninja -C jscomp -f env.ninja
```

Done!

Tip: you can also quickly switch between the environment compiler and the vendored one like so:

```
ninja -C jscomp -f env.ninja -t clean && ninja -C jscomp
```

```
ninja -C jscomp -t clean && ninja -C jscomp -f env.ninja
```

Note: clean up is necessary since the binary artifacts between versions of compiler may be incompatible.

## Test on a Dummy Project

Go somewhere else and do this:

```
bsb -init foo -theme basic-reason
cd foo
npm run build
```

And whenever you modify a file in bucklescript, run this inside `jscomp/`:

```
make ../lib/bsc.exe && ./install-bsc.sh # build the compiler and make it available globally
make ../lib/bsb.exe && ./install-bsb.sh # build the build system and make it available globally
```

This will substitute the global `bsc.exe` & `bsb.exe` you just installed with the newly built one. Then run `npm run build` again in the dummy project and see the changes! The iteration cycle for testing these should be around 2 seconds =).

## Troubleshooting

Did any of the above step not work?

- If you get compilation errors even from a supposedly clean compilation, you might have skipped the opam reinstall step above: `opam switch reinstall 4.02.3+buckle-master`
- Make sure you did ``eval `opam config env` `` in your CLI/bashrc/zshrc
- **If the vendored ocaml changed between when you last iterated on the repo and now**, you probably skipped the `opam switch reinstall 4.02.3+buckle-master` part. You'll have to do `git clean -xdf` and then restart with the build instructions. Careful, as `git clean` removes your uncommitted changes.
- **If these fail too**, make sure you do have the correct `ocamlopt` in your environment: `which ocamlopt` should show an `opam` path, not `reason-cli` path. If you see the latter, this means it overrode the global `ocamlopt` BuckleScript needed. In this case, either temporarily uninstall reason-cli or make sure your opam PATH overrides the reason-cli PATH (and not the other way around) in your bashrc/zshrc.

Whenever there are dependencies changes: do `make depend` in the specific directory, when available. This allows the makefile to track new dependencies.

## Change the Vendored OCaml Compiler

This section is reserved for when you're making a change to the vendored ocaml compiler itself, in `ocaml`, and then testing on super-errors changes at the same time. If you're doing this for whatever reason, then the previous quick iteration workflow wouldn't work. Here's what you have to do after each change:

```
# at project root
cd jscomp
make force-snapshotml # make sure your changes are reflected in jscomp/bin/whole_compiler.ml
make -C ../lib bsc.exe && ./install-bsc.sh
make -C ../lib bsb.exe && ./install-bsb.sh
```

## Contributing to the runtime

BuckleScript runtime implementation is currently a mix of OCaml and JavaScript. (`jscomp/runtime` directory). The JavaScript code is defined in the `.ml` file using the `bs.raw` syntax extension.

The goal is to implement the runtime **purely in OCaml** and you can help!

Each new PR should include appropriate testing.

Currently all tests are in `jscomp/test` directory and you should either add/modify a test file which covers the part of the compiler you modified.

- Add the filename in `jscomp/test/test.mllib`
- Add a test suite. The specification is in `jscomp/test/mt.ml`. For example some simple tests would be like:
  ```ocaml
  let suites : _ Mt.pair_suites =
     ["hey", (fun _ -> Eq(true, 3 > 2));
         "hi", (fun _ -> Neq(2,3));
         "hello", (fun _ -> Approx(3.0, 3.0));
         "throw", (fun _ -> ThrowAny(fun _ -> raise 3))
         ]
  let () = Mt.from_pair_suites __FILE__ suites
  ```

- Run the tests:
  `mocha -R list jscomp/test/your_test_file.js`
  To build libs, tests and run all tests:
  `make libs && make -C jscomp/test all && npm test`

- See the coverage: `npm run cover`

## Contributing to Documentation

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
[DCO](DCO.md).  This is very similar to the process used by the Linux(R) kernel, Samba, and many
other major open source projects.

To participate under these terms, all that you must do is include a line like the following as the
last line of the commit message for each commit in your contribution:

    Signed-Off-By: Random J. Developer <random@developer.example.org>

You must use your real name (sorry, no pseudonyms, and no anonymous contributions).
