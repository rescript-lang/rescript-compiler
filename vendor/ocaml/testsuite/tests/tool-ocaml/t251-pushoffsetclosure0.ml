open Lib;;
let rec f = function
  | 0 -> 13
  | n -> f 0
in
if f 5 <> 13 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 24
      11 CONST0
      12 PUSHACC1
      13 EQ
      14 BRANCHIFNOT 20
      16 CONSTINT 13
      18 RETURN 1
      20 CONST0
      21 PUSHOFFSETCLOSURE0
      22 APPTERM1 2
      24 CLOSUREREC 0, 11
      28 CONSTINT 13
      30 PUSHCONSTINT 5
      32 PUSHACC2
      33 APPLY1
      34 NEQ
      35 BRANCHIFNOT 42
      37 GETGLOBAL Not_found
      39 MAKEBLOCK1 0
      41 RAISE
      42 POP 1
      44 ATOM0
      45 SETGLOBAL T251-pushoffsetclosure0
      47 STOP
**)
