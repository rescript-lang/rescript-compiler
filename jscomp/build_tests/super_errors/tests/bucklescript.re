/* bs error */
let app = [@bs] (f, x, y) => f(x);

app(((x) => x + 1), 2);
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_bucklescript_case0.re", line 4, characters 0-3:
Error: This expression has type ((('a) => 'b, 'a, 'c) => 'b) [@bs]
       This is not a function; it cannot be applied.
=====

  We've found a bug for you!
  /Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_bucklescript_case0.re 4:1-3

  2 │ let app = [@bs] (f, x, y) => f(x);
  3 │
  4 │ app(((x) => x + 1), 2);

  This is an uncurried bucklescript function. It must be applied with [@bs].
*/
