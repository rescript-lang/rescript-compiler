let a: int = "hel

lo"; /**/
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case0.re", line 1, characters 13-22:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case0.re[0m [2m1:14-3:3[0m

  [1;31m1[0m [2m│[0m let a: int = [1;31m"hel[0m
  [1;31m2[0m [2m│[0m
  [1;31m3[0m [2m│[0m [1;31mlo"[0m; /**/

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/

              let a: int = "hel

              lo";
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case1.re", line 1, characters 27-50:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case1.re[0m [2m1:28-3:17[0m

  [1;31m1[0m [2m┆[0m let a: int = [1;31m"hel[0m
  [1;31m2[0m [2m┆[0m
  [1;31m3[0m [2m┆[0m [1;31mlo"[0m;

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/

                      let a: int = "hel

                      lo";
/*  */
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case2.re", line 1, characters 35-66:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case2.re[0m [2m1:36-3:25[0m

  [1;31m1[0m [2m│[0m                       let a: int = [1;31m"hel[0m
  [1;31m2[0m [2m│[0m
  [1;31m3[0m [2m│[0m [1;31m                      lo"[0m;
  4 [2m│[0m /*  */

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/

let a: int = "helllllll

loooooooooooooo";
/*  */
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case3.re", line 1, characters 13-41:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case3.re[0m [2m1:14-3:16[0m

  [1;31m1[0m [2m│[0m let a: int = [1;31m"helllllll[0m
  [1;31m2[0m [2m│[0m
  [1;31m3[0m [2m│[0m [1;31mloooooooooooooo"[0m;
  4 [2m│[0m /*  */

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/

/* single char highlighted */
let asd = {}
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case4.re", line 2, characters 11-11:
Warning 27: unused variable this.
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case4.re", line 1:
Error: CamlinternalOO.cmj not found, cmj format is generated by BuckleScript
=====

  [1;33mWarning number 27[0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case4.re[0m [2m2:12[0m

  1 [2m│[0m /* single char highlighted */
  [1;33m2[0m [2m│[0m let asd = {[1;33m}[0m

  unused variable this.

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case4.re[0m

  CamlinternalOO.cmj not found, cmj format is generated by BuckleScript
*/

/* overflows in the terminal */
let a: int = "helllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll";
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case5.re", line 2, characters 13-100:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case5.re[0m [2m2:14-100[0m

  1 [2m│[0m /* overflows in the terminal */
  [1;31m2[0m [2m│[0m let a: int = [1;31m"helllllllllllllllllllllllllllllllllllllllllllllllllllllll
      llllllllllllllllllllllllllll"[0m;

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/

let aaaaa = 10;
let b = aaaab;
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case6.re", line 2, characters 8-13:
Error: Unbound value aaaab
Hint: Did you mean aaaaa?
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_highlighting_case6.re[0m [2m2:9-13[0m

  1 [2m│[0m let aaaaa = 10;
  [1;31m2[0m [2m│[0m let b = [1;31maaaab[0m;

  The value aaaab can't be found

  [1;33mHint: Did you mean aaaaa?[0m
*/
