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

## History

This project was originally created by [Hongbo Zhang](https://github.com/bobzhang) in 2015 and 
still actively maintained by Hongbo Zhang hosted at 
[here](https://github.com/rescript-lang/rescript-compiler)

It was named BuckleScript and rebranded into [ReScript](https://rescript-lang.org/) in 2020.
The major contributions from contributors include super_errors from 
[Cheng](https://github.com/chenglou) and [Cristiano](https://github.com/cristianoc), react_jsx_ppx from [Ricky](https://github.com/rickyvetter). 
Cristiano also contributed to several important patches in the upstream native compiler,
in particular, the pattern match compilation.

More details are available [here](https://github.com/rescript-lang/rescript-compiler/graphs/contributors).


## Licensing

See [COPYING](./COPYING) and [COPYING.LESSER](./COPYING.LESSER)

The [`ocaml`](ocaml) directory contains the official [OCaml](https://ocaml.org) compiler (version 4.06.1).
Refer to its copyright and license notices for information about its licensing.

The `vendor/ninja.tar.gz` contains the vendored [ninja](https://github.com/ninja-build/ninja).
Refer to its copyright and license notices for information about its licensing.

Note that OSS is for sharing of knowledge, but the authorship should be respected. Copyright headers in each file, Acknowledgements and History section in this file should be kept **intact**.

See [Credits](./Credits.md) for more details.
