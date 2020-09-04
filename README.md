# [ReScript](https://rescript-lang.org)

The compiler for ReScript.

[![npm version](https://badge.fury.io/js/bs-platform.svg)](https://badge.fury.io/js/bs-platform) ![Build Status](https://circleci.com/gh/rescript-lang/rescript-compiler.svg?style=svg)

## Documentation

Please see the [documentation site](https://rescript-lang.org).

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Acknowledgments

* Thanks to the [OCaml](https://ocaml.org) team, obviously, without such a beautiful yet practical language, this backend would not exist
* Thanks to [ninja-build](https://ninja-build.org), ReScript also comes with a blazing fast build tool on top of it, `ninja` is a truly [well engineered](http://aosabook.org/en/posa/ninja.html) scalable build tool
* Thanks to [Bloomberg](https://www.techatbloomberg.com) and [Facebook](https://github.com/facebook/). This project began at Bloomberg and was published in 2016; without the support of Bloomberg, it would not have happened. This project's funded by Facebook since July/2017

## [Roadmap](https://github.com/rescript-lang/rescript-compiler/wiki)

## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

The [`ocaml`](ocaml) directory contains the official [OCaml](https://ocaml.org) compiler (version 4.06.1).
Refer to its copyright and license notices for information about its licensing.

The `vendor/ninja.tar.gz` contains the vendored [ninja](https://github.com/ninja-build/ninja).
Refer to its copyright and license notices for information about its licensing.

ReScript builds on parts of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml):

* [`jscomp/core/js_dump.ml`](jscomp/core/js_dump.ml) (pretty printer)

ReScript builds on parts of OCaml:

* [`jscomp/core/lam_pass_exits.ml`](jscomp/core/lam_pass_exits.ml)
* [`jscomp/core/lam_pass_lets_dce.ml`](jscomp/core/lam_pass_lets_dce.ml)

These modules were adapted from [`ocaml/bytecomp/simplif.ml`](ocaml/bytecomp/simplif.ml) for
JavaScript specific optimization purposes.

* [`jscomp/main/js_main.ml`](jscomp/main/js_main.ml)

`jscomp/main/js_main.ml` is adapted from [`ocaml/driver/main.ml`](ocaml/driver/main.ml). It is the main entry
point of the underlying compiler.

*  [`jscomp/stdlib-406`](jscomp/stdlib-406)

`jscomp/stdlib-*` is copied from [`ocaml/stdlib`](ocaml/stdlib). It is compiled to JavaScript and included with ReScript.

* [`jscomp/test`](jscomp/test)

`jscomp/test` is based on [`ocaml/testsuite`](ocaml/testsuite).

ReScript unit test builds on parts of [OUnit](http://ounit.forge.ocamlcore.org/)

* [`jscomp/ounit`](jscomp/ounit) is adapted from ounit, the unit test
  utilities are only used for dev purpose, they are not required for distribution
