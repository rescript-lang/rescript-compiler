open Lib;;
if 20 mod 3 <> 2 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST2 
      10 PUSHCONST3 
      11 PUSHCONSTINT 20
      13 MODINT 
      14 NEQ 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 ATOM0 
      23 SETGLOBAL T110-modint-1
      25 STOP 
**)
