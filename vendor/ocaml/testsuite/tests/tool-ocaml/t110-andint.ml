open Lib;;
if (3 land 6) <> 2 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST2
      10 PUSHCONSTINT 6
      12 PUSHCONST3
      13 ANDINT
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 ATOM0
      23 SETGLOBAL T110-andint
      25 STOP
**)
