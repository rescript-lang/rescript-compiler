open Lib;;
let x = 5 in
let y = 4 in
let z = 3 in
let f _ = z + y + x in
if f 0 <> 12 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 18
      11 ENVACC1 
      12 PUSHENVACC2 
      13 PUSHENVACC3 
      14 ADDINT 
      15 ADDINT 
      16 RETURN 1
      18 CONSTINT 5
      20 PUSHCONSTINT 4
      22 PUSHCONST3 
      23 PUSHACC0 
      24 PUSHACC2 
      25 PUSHACC4 
      26 CLOSURE 3, 11
      29 PUSHCONSTINT 12
      31 PUSHCONST0 
      32 PUSHACC2 
      33 APPLY1 
      34 NEQ 
      35 BRANCHIFNOT 42
      37 GETGLOBAL Not_found
      39 MAKEBLOCK1 0
      41 RAISE 
      42 POP 4
      44 ATOM0 
      45 SETGLOBAL T172-pushenvacc3
      47 STOP 
**)
