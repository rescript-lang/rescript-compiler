open Lib;;
try raise Not_found
with _ -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 PUSHTRAP 16
      11 GETGLOBAL Not_found
      13 MAKEBLOCK1 0
      15 RAISE 
      16 PUSHCONST0 
      17 POP 1
      19 ATOM0 
      20 SETGLOBAL T100-pushtrap
      22 STOP 
**)
