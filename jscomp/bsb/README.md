# BSB

Bsb is BuckleScript's build system. User-facing documentations are [here](https://bucklescript.github.io/docs/en/build-overview.html).

This directory hosts its implementation. It reads into `bsconfig.json`, uses some BS/OCaml/Reason-specific logic, and generates a [ninja](https://ninja-build.org) build file then calls `ninja` on it. So much of the incremental build and perf work is delegated to Ninja.

There's a `templates/` subdirectory. It's the thing shown when you do `bsb -themes`. To generate a template for the user, it basically picks the chosen template from `templates/` and copy pastes it into the destined user directory while substituting some strings in those templates, like `${bsb:proj-version}` in the `package.json`s. The copy-pasting of folders isn't actually done naively through a call to unix `cp`. It's cleverly achieved through something called ocamlres. Please see more descriptions in `pack-templates.sh`.
