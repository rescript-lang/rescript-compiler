open Lib;;
if (3 lsl 2) <> 12 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 12
      11 PUSHCONST2
      12 PUSHCONST3
      13 LSLINT
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 ATOM0
      23 SETGLOBAL T110-lslint
      25 STOP
**)
