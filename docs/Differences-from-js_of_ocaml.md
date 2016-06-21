[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml) is a popular compiler which compiles OCaml's bytecode into JavaScript. It is the inspiration for this project, and has already been under development for several years and is ready for production. In comparison, BuckleScript, while moving fast, is still a very young project. BuckleScript's motivation, like `js_of_ocaml`, is to unify the ubiquity of the JavaScript platform and the truly sophisticated type system of OCaml, however, there are some areas where we view things differently from `js_of_ocaml`. We describe below, some of these differences, and also refer readers to some of the original informal [discussions](https://github.com/ocsigen/js_of_ocaml/issues/338).

- Js_of_ocaml takes lowlevel bytecode from OCaml compiler, BuckleScript takes the highlevel rawlambda representation from OCaml compiler
- Js_of_ocaml focuses more on existing OCaml eco-system(opam) while BuckleScript's major goal is to target npm
- Js_of_ocaml and BuckleScript have slightly different runtime encoding in several places, for example, BuckleScript encodes OCaml Array as JS Array while js_of_ocaml requires its index 0 to be of value 0.

Both projects are improving quickly, so this can change in the future!
