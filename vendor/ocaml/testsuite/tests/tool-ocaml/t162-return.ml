open Lib;;
let f _ = 0 in f 0;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 14
      11 CONST0
      12 RETURN 1
      14 CLOSURE 0, 11
      17 PUSHCONST0
      18 PUSHACC1
      19 APPLY1
      20 POP 1
      22 ATOM0
      23 SETGLOBAL T162-return
      25 STOP
**)
