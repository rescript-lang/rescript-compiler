open Lib;;
let f _ _ _ = 0 in f 0 0 0;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 17
      11 RESTART
      12 GRAB 2
      14 CONST0
      15 RETURN 3
      17 CLOSURE 0, 12
      20 PUSHCONST0
      21 PUSHCONST0
      22 PUSHCONST0
      23 PUSHACC3
      24 APPLY3
      25 POP 1
      27 ATOM0
      28 SETGLOBAL T164-apply3
      30 STOP
**)
