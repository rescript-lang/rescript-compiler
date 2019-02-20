let b = Foo.b;
/*
File "/[elided]/normal_modules_case0.re", line 1, characters 8-13:
Error: Unbound module Foo
=====

  We've found a bug for you!
  /[elided]/normal_modules_case0.re 1:9-13

  1 │ let b = Foo.b;

  [1;33mThe module or file Foo can't be found.[0m
  - If it's a third-party dependency:
    - Did you list it in bsconfig.json?
    - Did you run `bsb` instead of `bsb -make-world`
      (latter builds third-parties)?
  - Did you include the file's directory in bsconfig.json?
*/

let b = List.b;
/*
File "/[elided]/normal_modules_case1.re", line 1, characters 8-14:
Error: Unbound value List.b
=====

  We've found a bug for you!
  /[elided]/normal_modules_case1.re 1:9-14

  1 │ let b = List.b;

  The value b can't be found in List
*/

module A = {
  module B = {
    module C = {
      module D = {
        let aaaa = 1;
      }
    }
  }
};

let asd = A.B.C.D.aaa;
/*
File "/[elided]/normal_modules_case2.re", line 11, characters 10-21:
Error: Unbound value A.B.C.D.aaa
Hint: Did you mean aaaa?
=====

  We've found a bug for you!
  /[elided]/normal_modules_case2.re 11:11-21

   9 │ };
  10 │
  11 │ let asd = A.B.C.D.aaa;

  The value aaa can't be found in A.B.C.D

  [1;33mHint: Did you mean aaaa?[0m
*/

let asd = JS.toOption
/*
File "/[elided]/normal_modules_case3.re", line 1:
Error: Wrong file naming: /Users/chenglou/github/bucklescript/jscomp/runtime/jS.cmi
contains the compiled interface for
JS when Js was expected
=====

  We've found a bug for you!
  /[elided]/normal_modules_case3.re

  You referred to the module JS, but we've found one called Js instead.
  Is the name's casing right?
*/
