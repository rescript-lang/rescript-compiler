open Lib;;
let x = 5 in
let y = 4 in
let f _ = y + x in
if f 0 <> 9 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 16
      11 ENVACC1 
      12 PUSHENVACC2 
      13 ADDINT 
      14 RETURN 1
      16 CONSTINT 5
      18 PUSHCONSTINT 4
      20 PUSHACC0 
      21 PUSHACC2 
      22 CLOSURE 2, 11
      25 PUSHCONSTINT 9
      27 PUSHCONST0 
      28 PUSHACC2 
      29 APPLY1 
      30 NEQ 
      31 BRANCHIFNOT 38
      33 GETGLOBAL Not_found
      35 MAKEBLOCK1 0
      37 RAISE 
      38 POP 3
      40 ATOM0 
      41 SETGLOBAL T172-pushenvacc2
      43 STOP 
**)
