open Lib;;
if not false then 0 else raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 BRANCHIF 15
      12 CONST0
      13 BRANCH 20
      15 GETGLOBAL Not_found
      17 MAKEBLOCK1 0
      19 RAISE
      20 ATOM0
      21 SETGLOBAL T070-branchif
      23 STOP
**)
