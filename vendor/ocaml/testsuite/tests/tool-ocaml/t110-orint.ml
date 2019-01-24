open Lib;;
if (3 lor 6) <> 7 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 7
      11 PUSHCONSTINT 6
      13 PUSHCONST3
      14 ORINT
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 ATOM0
      24 SETGLOBAL T110-orint
      26 STOP
**)
