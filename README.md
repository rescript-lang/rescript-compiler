[BuckleScript](http://bucklescript.github.io/bucklescript/): A JavaScript backend for [OCaml](https://ocaml.org/) focused on smooth integration and clean generated code.

[![NPM](https://nodei.co/npm/bs-platform.png?compact=true)](https://nodei.co/npm/bs-platform/) [![Build Status](https://travis-ci.org/BuckleScript/bucklescript.svg?branch=master)](https://travis-ci.org/bucklescript/bucklescript) [![Coverage Status](https://coveralls.io/repos/github/BuckleScript/bucklescript/badge.svg?branch=master)](https://coveralls.io/github/BuckleScript/bucklescript?branch=master)

## Documentation

Please see the [documentation site](https://bucklescript.github.io).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Acknowledgments

* Thanks to the [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this backend would not exist
* Thanks to [ninja-build](https://ninja-build.org), BuckleScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool
* Thanks to [Bloomberg](https://www.techatbloomberg.com)! This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. Now that the project has grown and developed its own community, it has moved to its own GitHub organization.

## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

The [`ocaml`](vendor/ocaml) directory contains the official [OCaml](https://ocaml.org) compiler (version 4.02.3).
Refer to its copyright and license notices for information about its licensing.

The [`ninja-build`](ninja-build) directory contains the official [ninja-build](https://github.com/ninja-build/ninja) (version 1.7.2).
Refer to its copyright and license notices for information about its licensing.

BuckleScript builds on parts of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml):

* [`jscomp/core/js_dump.ml`](jscomp/core/js_dump.ml) (pretty printer)
* [`jscomp/runtime`](jscomp/runtime)

BuckleScript builds on parts of OCaml:

* [`jscomp/core/lam_pass_exits.ml`](jscomp/core/lam_pass_exits.ml)
* [`jscomp/core/lam_pass_lets_dce.ml`](jscomp/core/lam_pass_lets_dce.ml)

These modules were adapted from [`ocaml/bytecomp/simplif.ml`](vendor/ocaml/bytecomp/simplif.ml) for
JavaScript specific optimization purposes.

* [`jscomp/core/js_main.ml`](jscomp/core/js_main.ml)

`jscomp/core/js_main.ml` is adapted from [`ocaml/driver/main.ml`](vendor/ocaml/driver/main.ml). It is not
actively used but demonstrates that it is easy to assemble a whole compiler using the OCaml compiler
libraries. It also shows how to add more compilation flags to a JS backend.

* [`jscomp/stdlib`](jscomp/stdlib)

`jscomp/stdlib` is copied from [`ocaml/stdlib`](vendor/ocaml/stdlib). It is compiled to JavaScript and
included with BuckleScript.

* [`jscomp/test`](jscomp/test)

`jscomp/test` is based on [`ocaml/testsuite`](vendor/ocaml/testsuite).

BuckleScript unittest builds on parts of [OUnit](http://ounit.forge.ocamlcore.org/)

* [`jscomp/ounit`](jscomp/ounit) is adapted from ounit, the unit test
  utilities are only used for dev purpose, they are not required for distribution
