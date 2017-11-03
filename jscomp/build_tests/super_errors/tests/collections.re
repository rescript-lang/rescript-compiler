/* wrong type in a list */
[1, 2, "Hello"];
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_collections_case0.re", line 2, characters 7-14:
Error: This expression has type string but an expression was expected of type
         int
=====

  We've found a bug for you!
  /Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/normal_collections_case0.re 2:8-14

  1 │ /* wrong type in a list */
  2 │ [1, 2, "Hello"];

  This value has type:
    string
  But was expected to be:
    int
*/
