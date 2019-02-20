let b = fo;;;;;;;;;;;;;;;;
/*
File "/[elided]/formatting_syntaxErrors_case0.re", line 1, characters 24-26:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case0.re' ]
File "/[elided]/formatting_syntaxErrors_case0.re", line 1:
Error: Error while running external preprocessor
Command line: lib/bsrefmt --print binary '/[elided]/formatting_syntaxErrors_case0.re' /var/folders/[elided]
=====
File "/[elided]/formatting_syntaxErrors_case0.re", line 1, characters 24-26:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case0.re' ]

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_syntaxErrors_case0.re[0m

  [1;33mThere's been an error running Reason's parser on a file.[0m
  The error location should be slightly above this message.
  Please file an issue on github.com/facebook/reason. Thanks!
*/

let a = (1, 2
/*
File "/[elided]/formatting_syntaxErrors_case1.re", line 1, characters 13-13:
Error: Syntax error: ')' expected
File "/[elided]/formatting_syntaxErrors_case1.re", line 1, characters 8-9:
Error: This '(' might be unmatched
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case1.re' ]
File "/[elided]/formatting_syntaxErrors_case1.re", line 1:
Error: Error while running external preprocessor
Command line: lib/bsrefmt --print binary '/[elided]/formatting_syntaxErrors_case1.re' /var/folders/[elided]
=====
File "/[elided]/formatting_syntaxErrors_case1.re", line 1, characters 13-13:
Error: Syntax error: ')' expected
File "/[elided]/formatting_syntaxErrors_case1.re", line 1, characters 8-9:
Error: This '(' might be unmatched
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case1.re' ]

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_syntaxErrors_case1.re[0m

  [1;33mThere's been an error running Reason's parser on a file.[0m
  The error location should be slightly above this message.
  Please file an issue on github.com/facebook/reason. Thanks!
*/

let () =
  I'm glad you're looking at this file =)
/*
File "/[elided]/formatting_syntaxErrors_case2.re", line 2, characters 34-38:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case2.re' ]
File "/[elided]/formatting_syntaxErrors_case2.re", line 1:
Error: Error while running external preprocessor
Command line: lib/bsrefmt --print binary '/[elided]/formatting_syntaxErrors_case2.re' /var/folders/[elided]
=====
File "/[elided]/formatting_syntaxErrors_case2.re", line 2, characters 34-38:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case2.re' ]

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_syntaxErrors_case2.re[0m

  [1;33mThere's been an error running Reason's parser on a file.[0m
  The error location should be slightly above this message.
  Please file an issue on github.com/facebook/reason. Thanks!
*/

let a = print_char('a)
/*
File "/[elided]/formatting_syntaxErrors_case3.re", line 1, characters 20-21:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case3.re' ]
File "/[elided]/formatting_syntaxErrors_case3.re", line 1:
Error: Error while running external preprocessor
Command line: lib/bsrefmt --print binary '/[elided]/formatting_syntaxErrors_case3.re' /var/folders/[elided]
=====
File "/[elided]/formatting_syntaxErrors_case3.re", line 1, characters 20-21:
Error: Syntax error
Error happened when running command /[elided]/refmt.exe with args [ '--print',
  'binary',
  '/[elided]/formatting_syntaxErrors_case3.re' ]

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_syntaxErrors_case3.re[0m

  [1;33mThere's been an error running Reason's parser on a file.[0m
  The error location should be slightly above this message.
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
File "/[elided]/formatting_syntaxErrors_case4.re", line 4, characters 12-86:
Error: This expression has type string but an expression was expected of type
         int
=====

  [1;31mWe've found a bug for you![0m
  [36m/[elided]/formatting_syntaxErrors_case4.re[0m [2m4:13-20:3[0m

   2 [2m│[0m /* */
   3 [2m│[0m /* */
  [1;31m 4[0m [2m│[0m let a:int = [1;31m"asdaaaaaaaaaaaaaaaaaaaaa[0m
  [1;31m 5[0m [2m│[0m [1;31maa[0m
  [2m .[0m [2m│[0m [2m...[0m
  [1;31m19[0m [2m│[0m [1;31maa[0m
  [1;31m20[0m [2m│[0m [1;31maa"[0m
  21 [2m│[0m /* */
  22 [2m│[0m /* */

  This has type:
    [1;31mstring[0m
  But somewhere wanted:
    [1;33mint[0m
*/
