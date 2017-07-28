open Lib;;
let rec f n =
  if n <= 0 then 12
  else 1 + f (n-1)
in
if f 30000 <> 30012 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 29
      11 CONST0 
      12 PUSHACC1 
      13 LEINT 
      14 BRANCHIFNOT 20
      16 CONSTINT 12
      18 RETURN 1
      20 ACC0 
      21 OFFSETINT -1
      23 PUSHOFFSETCLOSURE0 
      24 APPLY1 
      25 PUSHCONST1 
      26 ADDINT 
      27 RETURN 1
      29 CLOSUREREC 0, 11
      33 CONSTINT 30012
      35 PUSHCONSTINT 30000
      37 PUSHACC2 
      38 APPLY1 
      39 NEQ 
      40 BRANCHIFNOT 47
      42 GETGLOBAL Not_found
      44 MAKEBLOCK1 0
      46 RAISE 
      47 POP 1
      49 ATOM0 
      50 SETGLOBAL T360-stacks-1
      52 STOP 
**)
