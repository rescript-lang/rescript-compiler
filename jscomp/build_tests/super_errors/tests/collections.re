/* wrong type in a list */
[1, 2, "Hello"];
/*
File "/[elided]/normal_collections_case0.re", line 2, characters 7-14:
Error: This expression has type string but an expression was expected of type
         int
=====

  We've found a bug for you!
  /[elided]/normal_collections_case0.re 2:8-14

  1 │ /* wrong type in a list */
  2 │ [1, 2, "Hello"];

  This has type:
    [1;31mstring[0m
  But somewhere wanted:
    [1;33mint[0m
*/
