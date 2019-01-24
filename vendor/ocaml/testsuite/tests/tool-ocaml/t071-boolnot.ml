open Lib;;
if not true then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 BOOLNOT
      11 BRANCHIFNOT 18
      13 GETGLOBAL Not_found
      15 MAKEBLOCK1 0
      17 RAISE
      18 ATOM0
      19 SETGLOBAL T071-boolnot
      21 STOP
**)
