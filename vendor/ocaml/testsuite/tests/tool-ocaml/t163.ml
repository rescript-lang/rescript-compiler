open Lib;;
let f _ _ = 0 in f 0;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 17
      11 RESTART
      12 GRAB 1
      14 CONST0
      15 RETURN 2
      17 CLOSURE 0, 12
      20 PUSHCONST0
      21 PUSHACC1
      22 APPLY1
      23 POP 1
      25 ATOM0
      26 SETGLOBAL T163
      28 STOP
**)
