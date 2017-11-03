let b = fo;;;;;;;;;;;;;;;;
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re", line 1, characters 10-12:
Error: 869: syntax error, consider adding a `;' before
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re", line 1:
Error: Error while running external preprocessor
Command line: refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re' /var/folders/[elided]
=====
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re", line 1, characters 10-12:
Error: 869: syntax error, consider adding a `;' before

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re[0m

  [1;33mThere's been an error running Reason's refmt parser on a file.[0m
  This was the command:

  refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case0.re' /var/folders/[elided]

  Please file an issue on github.com/facebook/reason. Thanks!
*/

let a = (1, 2
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re", line 1, characters 13-13:
Error: 1387: <UNKNOWN SYNTAX ERROR>
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re", line 1:
Error: Error while running external preprocessor
Command line: refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re' /var/folders/[elided]
=====
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re", line 1, characters 13-13:
Error: 1387: <UNKNOWN SYNTAX ERROR>

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re[0m

  [1;33mThere's been an error running Reason's refmt parser on a file.[0m
  This was the command:

  refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case1.re' /var/folders/[elided]

  Please file an issue on github.com/facebook/reason. Thanks!
*/

let () =
  I'm glad you're looking at this file =)
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re", line 2, characters 6-10:
Error: 1378: syntax error, consider adding a `;' before
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re", line 1:
Error: Error while running external preprocessor
Command line: refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re' /var/folders/[elided]
=====
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re", line 2, characters 6-10:
Error: 1378: syntax error, consider adding a `;' before

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re[0m

  [1;33mThere's been an error running Reason's refmt parser on a file.[0m
  This was the command:

  refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case2.re' /var/folders/[elided]

  Please file an issue on github.com/facebook/reason. Thanks!
*/

let a = print_char('a)
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re", line 1, characters 19-20:
Error: 864: <UNKNOWN SYNTAX ERROR>
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re", line 1:
Error: Error while running external preprocessor
Command line: refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re' /var/folders/[elided]
=====
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re", line 1, characters 19-20:
Error: 864: <UNKNOWN SYNTAX ERROR>

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re[0m

  [1;33mThere's been an error running Reason's refmt parser on a file.[0m
  This was the command:

  refmt3.exe --print binary '/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case3.re' /var/folders/[elided]

  Please file an issue on github.com/facebook/reason. Thanks!
*/

/* */
/* */
/* */
let a:int = "asdaaaaaaaaaaaaaaaaaaaaa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa
aa"
/* */
/* */
/* */
/* */
/*
File "/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case4.re", line 4, characters 12-86:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/Users/jared/clone/fork/bucklescript/jscomp/build_tests/super_errors/tmp/formatting_syntaxErrors_case4.re[0m [2m4:13-20:3[0m

   2 [2m│[0m /* */
   3 [2m│[0m /* */
  [1;31m 4[0m [2m│[0m let a:int = [1;31m"asdaaaaaaaaaaaaaaaaaaaaa[0m
  [1;31m 5[0m [2m│[0m [1;31maa[0m
  [2m .[0m [2m│[0m [2m...[0m
  [1;31m19[0m [2m│[0m [1;31maa[0m
  [1;31m20[0m [2m│[0m [1;31maa"[0m
  21 [2m│[0m /* */
  22 [2m│[0m /* */

  This value has type:
    [1;31mstring[0m
  But was expected to be:
    [1;33mint[0m
*/
