open Lib;;
let f () = ();;

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
      17 PUSHACC0
      18 MAKEBLOCK1 0
      20 POP 1
      22 SETGLOBAL T160-closure
      24 STOP
**)
