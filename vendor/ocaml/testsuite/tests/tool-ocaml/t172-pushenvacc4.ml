open Lib;;
let x = 5 in
let y = 4 in
let z = 3 in
let a = 2 in
let f _ = a + z + y + x in
if f 0 <> 14 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 21
      11 ENVACC1 
      12 PUSHENVACC2 
      13 PUSHENVACC3 
      14 PUSHENVACC 4
      16 ADDINT 
      17 ADDINT 
      18 ADDINT 
      19 RETURN 1
      21 CONSTINT 5
      23 PUSHCONSTINT 4
      25 PUSHCONST3 
      26 PUSHCONST2 
      27 PUSHACC0 
      28 PUSHACC2 
      29 PUSHACC4 
      30 PUSHACC6 
      31 CLOSURE 4, 11
      34 PUSHCONSTINT 14
      36 PUSHCONST0 
      37 PUSHACC2 
      38 APPLY1 
      39 NEQ 
      40 BRANCHIFNOT 47
      42 GETGLOBAL Not_found
      44 MAKEBLOCK1 0
      46 RAISE 
      47 POP 5
      49 ATOM0 
      50 SETGLOBAL T172-pushenvacc4
      52 STOP 
**)
