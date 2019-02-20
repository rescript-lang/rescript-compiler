/* got float, wanted int */
2. + 2;
/*
File "/[elided]/normal_primitives_case0.re", line 2, characters 0-2:
Error: This expression has type float but an expression was expected of type
         int
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case0.re 2:1-2

  1 │ /* got float, wanted int */
  2 │ 2. + 2;

  This has type:
    [1;31mfloat[0m
  But somewhere wanted:
    [1;33mint[0m

You can convert a [1;33mfloat[0m to a [1;33mint[0m with [1;33mint_of_float[0m.If this is a literal, you want a number without a trailing dot (e.g. [1;33m20[0m).
*/

/* got int, wanted string */
2 ++ " things";
/*
File "/[elided]/normal_primitives_case1.re", line 2, characters 0-1:
Error: This expression has type int but an expression was expected of type
         string
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case1.re 2:1

  1 │ /* got int, wanted string */
  2 │ 2 ++ " things";

  This has type:
    [1;31mint[0m
  But somewhere wanted:
    [1;33mstring[0m

You can convert a [1;33mint[0m to a [1;33mstring[0m with [1;33mstring_of_int[0m.
*/

/* Too many arguments */
let x = (a) => a + 2;
x(2, 4);
/*
File "/[elided]/normal_primitives_case2.re", line 3, characters 0-1:
Error: This function has type int => int
       It is applied to too many arguments; maybe you forgot a `;'.
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case2.re 3:1

  1 │ /* Too many arguments */
  2 │ let x = (a) => a + 2;
  3 │ x(2, 4);

  This function has type [1;33mint => int[0m
  It only accepts 1 argument; here, it's called with more.
*/

/* Not a function */
let x = 10;
x(10);
/*
File "/[elided]/normal_primitives_case3.re", line 3, characters 0-1:
Error: This expression has type int
       This is not a function; it cannot be applied.
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case3.re 3:1

  1 │ /* Not a function */
  2 │ let x = 10;
  3 │ x(10);

  This expression has type int
  It is not a function.
*/

/* Not enough arguments */
type x = X(int, float);
X(10);
/*
File "/[elided]/normal_primitives_case4.re", line 3, characters 0-5:
Error: The constructor X expects 2 argument(s),
       but is applied here to 1 argument(s)
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case4.re 3:1-5

  1 │ /* Not enough arguments */
  2 │ type x = X(int, float);
  3 │ X(10);

  This variant constructor, X, expects 2 arguments; here, we've only found 1.
*/

/* Wrong constructor argument */
type x = X(int, float);
X(10, 10);
/*
File "/[elided]/normal_primitives_case5.re", line 3, characters 6-8:
Error: This expression has type int but an expression was expected of type
         float
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case5.re 3:7-8

  1 │ /* Wrong constructor argument */
  2 │ type x = X(int, float);
  3 │ X(10, 10);

  This has type:
    [1;31mint[0m
  But somewhere wanted:
    [1;33mfloat[0m

You can convert a [1;33mint[0m to a [1;33mfloat[0m with [1;33mfloat_of_int[0m.If this is a literal, you want a number with a trailing dot (e.g. [1;33m20.[0m).
*/

/* Wanted list(float), found list(int) */
let a = [1,2,3];
List.map(((n) => n +. 2.), a);
/*
File "/[elided]/normal_primitives_case6.re", line 3, characters 27-28:
Error: This expression has type list(int)
       but an expression was expected of type list(float)
       Type int is not compatible with type float
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case6.re 3:28

  1 │ /* Wanted list(float), found list(int) */
  2 │ let a = [1,2,3];
  3 │ List.map(((n) => n +. 2.), a);

  This has type:
    [1;31mlist(int)[0m
  But somewhere wanted:
    [1;33mlist(float)[0m

  The incompatible parts:
    [1;31mint[0m
    vs
    [1;33mfloat[0m

You can convert a [1;33mint[0m to a [1;33mfloat[0m with [1;33mfloat_of_int[0m.If this is a literal, you want a number with a trailing dot (e.g. [1;33m20.[0m).
*/

let asd = aaa;
/*
File "/[elided]/normal_primitives_case7.re", line 1, characters 10-13:
Error: Unbound value aaa
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case7.re 1:11-13

  1 │ let asd = aaa;

  The value aaa can't be found
*/

let a: int = "hello"
/*
File "/[elided]/normal_primitives_case8.re", line 1, characters 13-20:
Error: This expression has type string but an expression was expected of type
         int
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case8.re 1:14-20

  1 │ let a: int = "hello"

  This has type:
    [1;31mstring[0m
  But somewhere wanted:
    [1;33mint[0m
*/

let aaaaa = 10;
let b = aaaab;
/*
File "/[elided]/normal_primitives_case9.re", line 2, characters 8-13:
Error: Unbound value aaaab
Hint: Did you mean aaaaa?
=====

  We've found a bug for you!
  /[elided]/normal_primitives_case9.re 2:9-13

  1 │ let aaaaa = 10;
  2 │ let b = aaaab;

  The value aaaab can't be found

  [1;33mHint: Did you mean aaaaa?[0m
*/
