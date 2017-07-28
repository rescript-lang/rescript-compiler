open Lib;;
let rec f _ = 4
    and g _ = f 2
in
if g 5 <> 4 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 19
      11 CONSTINT 4
      13 RETURN 1
      15 CONST2 
      16 PUSHOFFSETCLOSUREM2 
      17 APPTERM1 2
      19 CLOSUREREC 0, 11, 15
      24 CONSTINT 4
      26 PUSHCONSTINT 5
      28 PUSHACC2 
      29 APPLY1 
      30 NEQ 
      31 BRANCHIFNOT 38
      33 GETGLOBAL Not_found
      35 MAKEBLOCK1 0
      37 RAISE 
      38 POP 2
      40 ATOM0 
      41 SETGLOBAL T251-pushoffsetclosurem2
      43 STOP 
**)
