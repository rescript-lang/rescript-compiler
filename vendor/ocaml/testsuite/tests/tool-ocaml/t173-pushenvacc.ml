open Lib;;
let x = 5 in
let y = 4 in
let z = 3 in
let a = 2 in
let b = 1 in
let f _ = b + a + z + y + x in
if f 0 <> 15 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 24
      11 ENVACC1 
      12 PUSHENVACC2 
      13 PUSHENVACC3 
      14 PUSHENVACC 4
      16 PUSHENVACC 5
      18 ADDINT 
      19 ADDINT 
      20 ADDINT 
      21 ADDINT 
      22 RETURN 1
      24 CONSTINT 5
      26 PUSHCONSTINT 4
      28 PUSHCONST3 
      29 PUSHCONST2 
      30 PUSHCONST1 
      31 PUSHACC0 
      32 PUSHACC2 
      33 PUSHACC4 
      34 PUSHACC6 
      35 PUSHACC 8
      37 CLOSURE 5, 11
      40 PUSHCONSTINT 15
      42 PUSHCONST0 
      43 PUSHACC2 
      44 APPLY1 
      45 NEQ 
      46 BRANCHIFNOT 53
      48 GETGLOBAL Not_found
      50 MAKEBLOCK1 0
      52 RAISE 
      53 POP 6
      55 ATOM0 
      56 SETGLOBAL T173-pushenvacc
      58 STOP 
**)
