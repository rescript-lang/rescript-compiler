let x = (~a, ~b) => a + b;
let y = x(~a=2) + 2;
/*
File "/[elided]/normal_moreArguments_case0.re", line 2, characters 8-15:
Error: This expression has type (~b: int) => int
       but an expression was expected of type int
=====

  We've found a bug for you!
  /[elided]/normal_moreArguments_case0.re 2:9-15

  1 │ let x = (~a, ~b) => a + b;
  2 │ let y = x(~a=2) + 2;

  [1;33mThis call is missing an argument[0m of type (~b: int)
*/

let x = (a, b) => a + b;
let y = x(2) + 2;
/*
File "/[elided]/normal_moreArguments_case1.re", line 2, characters 8-12:
Error: This expression has type int => int
       but an expression was expected of type int
=====

  We've found a bug for you!
  /[elided]/normal_moreArguments_case1.re 2:9-12

  1 │ let x = (a, b) => a + b;
  2 │ let y = x(2) + 2;

  [1;33mThis call is missing an argument[0m of type int
*/

let x = (a, b, c, d) => a + b;
let y = x(2) + 2;
/*
File "/[elided]/normal_moreArguments_case2.re", line 2, characters 8-12:
Error: This expression has type (int, 'a, 'b) => int
       but an expression was expected of type int
=====

  We've found a bug for you!
  /[elided]/normal_moreArguments_case2.re 2:9-12

  1 │ let x = (a, b, c, d) => a + b;
  2 │ let y = x(2) + 2;

  [1;33mThis call is missing arguments[0m of type: int, 'a, 'b
*/

let x = (a, ~b, ~c, ~d) => a + b;
let y = x(2) + 2;
/*
File "/[elided]/normal_moreArguments_case3.re", line 2, characters 8-12:
Error: This expression has type (~b: int, ~c: 'a, ~d: 'b) => int
       but an expression was expected of type int
=====

  We've found a bug for you!
  /[elided]/normal_moreArguments_case3.re 2:9-12

  1 │ let x = (a, ~b, ~c, ~d) => a + b;
  2 │ let y = x(2) + 2;

  [1;33mThis call is missing arguments[0m of type: (~b: int), (~c: 'a), (~d: 'b)
*/

let module Sub = {
  type a = {a: int};
};
let x = (a, b, c, d) => {Sub.a: 2};
let y = x(2).Sub.a;
/*
File "/[elided]/normal_moreArguments_case4.re", line 5, characters 8-12:
Error: This expression has type ('a, 'b, 'c) => Sub.a
       but an expression was expected of type Sub.a
=====

  We've found a bug for you!
  /[elided]/normal_moreArguments_case4.re 5:9-12

  3 │ };
  4 │ let x = (a, b, c, d) => {Sub.a: 2};
  5 │ let y = x(2).Sub.a;

  [1;33mThis call is missing arguments[0m of type: 'a, 'b, 'c
*/
