open Lib;;
if 0 > 0 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0 
      10 PUSHCONST0 
      11 GTINT 
      12 BRANCHIFNOT 19
      14 GETGLOBAL Not_found
      16 MAKEBLOCK1 0
      18 RAISE 
      19 ATOM0 
      20 SETGLOBAL T080-gtint
      22 STOP 
**)
