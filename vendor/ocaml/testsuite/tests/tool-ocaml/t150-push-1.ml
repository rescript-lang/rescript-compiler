open Lib;;
let _ = 0 in
try 0 with _ -> 0
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 PUSH
      11 PUSHTRAP 17
      13 CONST0
      14 POPTRAP
      15 BRANCH 20
      17 PUSHCONST0
      18 POP 1
      20 POP 1
      22 ATOM0
      23 SETGLOBAL T150-push-1
      25 STOP
**)
