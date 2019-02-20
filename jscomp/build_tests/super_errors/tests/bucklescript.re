/* bs error */
let app = [@bs] (f, x, y) => f(x);

app(((x) => x + 1), 2);
/*
File "/[elided]/normal_bucklescript_case0.re", line 4, characters 0-3:
Error: This expression has type (. 'a => 'b, 'a, 'c) => 'b
       This is not a function; it cannot be applied.
=====

  We've found a bug for you!
  /[elided]/normal_bucklescript_case0.re 4:1-3

  2 │ let app = [@bs] (f, x, y) => f(x);
  3 │
  4 │ app(((x) => x + 1), 2);

  This is an uncurried BuckleScript function. [1;33mIt must be applied with a dot[0m.

  Like this: [1;33mfoo(. a, b)[0m
  Not like this: [2mfoo(a, b)[0m

  This guarantees that your function is fully applied. More info here:
  https://bucklescript.github.io/docs/en/function.html#solution-guaranteed-uncurrying
*/
