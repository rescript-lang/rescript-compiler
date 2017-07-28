open Lib;;
if Hashtbl.hash_param 5 6 [1;2;3] <> 697606130 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 196799
      11 PUSHGETGLOBAL <0>(1, <0>(2, <0>(3, 0)))
      13 PUSHCONSTINT 6
      15 PUSHCONSTINT 5
      17 C_CALL3 hash_univ_param
      19 NEQ 
      20 BRANCHIFNOT 27
      22 GETGLOBAL Not_found
      24 MAKEBLOCK1 0
      26 RAISE 
      27 ATOM0 
      28 SETGLOBAL T240-c_call3
      30 STOP 
**)
