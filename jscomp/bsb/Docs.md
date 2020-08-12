# BSB

Bsb is ReScript's build system. User-facing documentations are [here](https://rescript-lang.org/docs/manual/latest/build-overview).

This directory hosts its implementation. It reads into `bsconfig.json`, uses some BS/OCaml/Reason-specific logic, and generates a [ninja](https://ninja-build.org) build file then calls `ninja` on it. So much of the incremental build and perf work is delegated to Ninja.

There's a `templates/` subdirectory. It's the thing shown when you do `bsb -themes`. To generate a template for the user, it basically picks the chosen template from `templates/` and copy pastes it into the destined user directory while substituting some strings in those templates, like `${bsb:proj-version}` in the `package.json`s. The copy-pasting of folders isn't actually done naively through a call to unix `cp`. It's cleverly achieved through something called ocamlres. Please see more descriptions in `pack-templates.sh`.

## Add/edit a template

The content of `templates` is packed into `bsb_templates.ml` automatically when running `pack-templates.sh`, located in this directory.

When adding/editing a template the script needs to be rerun to update the relevant parts in `bsb_templates.ml`. The script uses [`ocamlres`](https://github.com/OCamlPro/ocp-ocamlres), a tool for embedding files inside ocaml executables. You will need to install it with [`opam`](https://opam.ocaml.org/doc/Install.html), ocaml package manager. Follow the [instructions](https://opam.ocaml.org/doc/Install.html) to install `opam`, if you haven't installed it yet. To install `ocp-ocamlres` run:

```sh
opam install ocp-ocamlres
```

## Testing a template locally

Do the following setup steps to build the compiler: [build ocaml compiler](https://github.com/rescript-lang/rescript-compiler/blob/master/CONTRIBUTING.md#build-the-vendored-ocaml-compiler) and [build everything in dev mode](https://github.com/rescript-lang/rescript-compiler/blob/master/CONTRIBUTING.md#build-everything-in-dev-mode-using-vendored-compiler).

The built binaries will end up under `lib` where you can run local `bsb`:

```sh
./bsb -init test-theme -theme new_theme
```
