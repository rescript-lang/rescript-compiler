open Lib;;
let x = 5 in
let f _ = x + x in
if f 0 <> 10 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 16
      11 ENVACC1 
      12 PUSHENVACC1 
      13 ADDINT 
      14 RETURN 1
      16 CONSTINT 5
      18 PUSHACC0 
      19 CLOSURE 1, 11
      22 PUSHCONSTINT 10
      24 PUSHCONST0 
      25 PUSHACC2 
      26 APPLY1 
      27 NEQ 
      28 BRANCHIFNOT 35
      30 GETGLOBAL Not_found
      32 MAKEBLOCK1 0
      34 RAISE 
      35 POP 2
      37 ATOM0 
      38 SETGLOBAL T172-pushenvacc1
      40 STOP 
**)
