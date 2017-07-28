open Lib;;
let rec f _ = 11
    and g _ = f
in
if g 3 4 <> 11 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 18
      11 CONSTINT 11
      13 RETURN 1
      15 OFFSETCLOSUREM2 
      16 RETURN 1
      18 CLOSUREREC 0, 11, 15
      23 CONSTINT 11
      25 PUSHCONSTINT 4
      27 PUSHCONST3 
      28 PUSHACC3 
      29 APPLY2 
      30 NEQ 
      31 BRANCHIFNOT 38
      33 GETGLOBAL Not_found
      35 MAKEBLOCK1 0
      37 RAISE 
      38 POP 2
      40 ATOM0 
      41 SETGLOBAL T253-offsetclosurem2
      43 STOP 
**)
