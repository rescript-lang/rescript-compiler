open Lib;;
let rec f _ = g 0
    and g _ = 4
in
if f 5 <> 4 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 19
      11 CONST0 
      12 PUSHOFFSETCLOSURE2 
      13 APPTERM1 2
      15 CONSTINT 4
      17 RETURN 1
      19 CLOSUREREC 0, 11, 15
      24 CONSTINT 4
      26 PUSHCONSTINT 5
      28 PUSHACC3 
      29 APPLY1 
      30 NEQ 
      31 BRANCHIFNOT 38
      33 GETGLOBAL Not_found
      35 MAKEBLOCK1 0
      37 RAISE 
      38 POP 2
      40 ATOM0 
      41 SETGLOBAL T251-pushoffsetclosure2
      43 STOP 
**)
