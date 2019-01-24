open Lib;;
let x = 1 in
if -x <> -1 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONSTINT -1
      12 PUSHACC1
      13 NEGINT
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 POP 1
      24 ATOM0
      25 SETGLOBAL T110-negint
      27 STOP
**)
