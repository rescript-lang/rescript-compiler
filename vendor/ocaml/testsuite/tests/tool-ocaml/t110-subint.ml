open Lib;;
let x = 1 in
if 1 - x <> 0 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONST0
      11 PUSHACC1
      12 PUSHCONST1
      13 SUBINT
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 POP 1
      24 ATOM0
      25 SETGLOBAL T110-subint
      27 STOP
**)
