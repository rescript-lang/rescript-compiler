open Lib;;
if false then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 BRANCHIFNOT 17
      12 GETGLOBAL Not_found
      14 MAKEBLOCK1 0
      16 RAISE
      17 ATOM0
      18 SETGLOBAL T070-branchifnot
      20 STOP
**)
