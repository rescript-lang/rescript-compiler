open Lib;;
if 2 * 2 <> 4 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 4
      11 PUSHCONST2 
      12 PUSHCONST2 
      13 MULINT 
      14 NEQ 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 ATOM0 
      23 SETGLOBAL T110-mulint
      25 STOP 
**)
