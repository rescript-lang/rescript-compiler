# Credits

ReScript builds on parts of OCaml:

* [`jscomp/core/lam_pass_exits.ml`](jscomp/core/lam_pass_exits.ml)
* [`jscomp/core/lam_pass_lets_dce.ml`](jscomp/core/lam_pass_lets_dce.ml)

These modules were adapted from [`ocaml/bytecomp/simplif.ml`](ocaml/bytecomp/simplif.ml) for
JavaScript specific optimization purposes.

* [`jscomp/main/rescript_compiler_main.ml`](jscomp/main/rescript_compiler_main.ml)

`jscomp/main/rescript_compiler_main.ml` is adapted from [`ocaml/driver/main.ml`](ocaml/driver/main.ml). It is the main entry
point of the underlying compiler.

*  [`jscomp/stdlib-406`](jscomp/stdlib-406)

`jscomp/stdlib-*` is copied from [`ocaml/stdlib`](ocaml/stdlib). It is compiled to JavaScript and included with ReScript.

ReScript imported one file from [jsoo](https://github.com/ocsigen/js_of_ocaml)

* [`jscomp/core/js_dump.ml`](jscomp/core/js_dump.ml) (pretty printer)

This file was imported but changed significantly in iterations later.

* [`jscomp/test`](jscomp/test)

`jscomp/test` is based on [`ocaml/testsuite`](ocaml/testsuite).

ReScript unit test builds on parts of [OUnit](http://ounit.forge.ocamlcore.org/)

* [`jscomp/ounit`](jscomp/ounit) is adapted from ounit, the unit test
  utilities are only used for dev purpose, they are not required for distribution
