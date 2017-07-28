open Lib;;
if [| 1; 2 |].(1) <> 2 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST2 
      10 PUSHCONST1 
      11 PUSHCONST2 
      12 PUSHCONST1 
      13 MAKEBLOCK2 0
      15 GETVECTITEM 
      16 NEQ 
      17 BRANCHIFNOT 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE 
      24 ATOM0 
      25 SETGLOBAL T130-getvectitem
      27 STOP 
**)
