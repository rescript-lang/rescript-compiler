let x = (a, b) => a + b;
let z = () => {
  x(10);
  10
};
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_warnings_case0.re", line 3, characters 2-7:
Warning 5: this function application is partial,
maybe some arguments are missing.
=====

  Warning number 5
  /Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_warnings_case0.re 3:3-7

  1 │ let x = (a, b) => a + b;
  2 │ let z = () => {
  3 │   x(10);
  4 │   10
  5 │ };

  this function application is partial,
maybe some arguments are missing.
*/

let z = () => {
  10;
  10
};
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_warnings_case1.re", line 2, characters 2-4:
Warning 10: this expression should have type unit.
=====

  Warning number 10
  /Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_warnings_case1.re 2:3-4

  1 │ let z = () => {
  2 │   10;
  3 │   10
  4 │ };

  This expression returns a value, but you're not doing anything with it. If this is on purpose, put `|> ignore` at the end.
*/
