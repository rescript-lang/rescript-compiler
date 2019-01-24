open Lib;;
if 2 / 2 <> 1 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONST2
      11 PUSHCONST2
      12 DIVINT
      13 NEQ
      14 BRANCHIFNOT 21
      16 GETGLOBAL Not_found
      18 MAKEBLOCK1 0
      20 RAISE
      21 ATOM0
      22 SETGLOBAL T110-divint-1
      24 STOP
**)
