let x = 2. + 2;
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_someTests_case0.re", line 1, characters 8-10:
Error: This expression has type float but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_someTests_case0.re[0m [2m1:9-10[0m

  [1;31m1[0m [2m│[0m let x = [1;31m2.[0m + 2;

  This value has type:
    [1;31mfloat[0m
  But was expected to be:
    [1;33mint[0m

You can convert a [1;33mfloat[0m to a [1;33mint[0m with [1;33mint_of_float[0m.
If this is a literal, you want a number without a trailing dot (e.g. [1;33m20[0m).
*/
