open Lib;;
if Pervasives.compare 1 2 <> -1 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT -1
      11 PUSHCONST2 
      12 PUSHCONST1 
      13 C_CALL2 compare
      15 NEQ 
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE 
      23 ATOM0 
      24 SETGLOBAL T240-c_call2
      26 STOP 
**)
