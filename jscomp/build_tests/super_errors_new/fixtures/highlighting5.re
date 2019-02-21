/* overflows in the terminal */
let a: int = "helllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll";
/*
File "/[elided]/formatting_highlighting_case5.re", line 2, characters 13-100:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_highlighting_case5.re[0m [2m2:14-100[0m

  1 [2m│[0m /* overflows in the terminal */
  [1;31m2[0m [2m│[0m let a: int = [1;31m"helllllllllllllllllllllllllllllllllllllllllllllllllllllll
      llllllllllllllllllllllllllll"[0m;

  This has type:
    [1;31mstring[0m
  But somewhere wanted:
    [1;33mint[0m
*/
