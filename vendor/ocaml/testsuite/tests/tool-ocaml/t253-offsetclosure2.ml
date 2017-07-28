open Lib;;
let rec f _ = g
    and g _ = 10
in
if f 3 4 <> 10 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 18
      11 OFFSETCLOSURE2 
      12 RETURN 1
      14 CONSTINT 10
      16 RETURN 1
      18 CLOSUREREC 0, 11, 14
      23 CONSTINT 10
      25 PUSHCONSTINT 4
      27 PUSHCONST3 
      28 PUSHACC4 
      29 APPLY2 
      30 NEQ 
      31 BRANCHIFNOT 38
      33 GETGLOBAL Not_found
      35 MAKEBLOCK1 0
      37 RAISE 
      38 POP 2
      40 ATOM0 
      41 SETGLOBAL T253-offsetclosure2
      43 STOP 
**)
