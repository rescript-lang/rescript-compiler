open Lib;;
let x = 5 in
let y = 2 in
let z = 1 in
let a = 4 in
let b = 3 in
let f _ = ignore x; ignore y; ignore z; ignore a; b in
if f 0 <> 3 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 23
      11 ENVACC1 
      12 CONST0 
      13 ENVACC2 
      14 CONST0 
      15 ENVACC3 
      16 CONST0 
      17 ENVACC4 
      18 CONST0 
      19 ENVACC 5
      21 RETURN 1
      23 CONSTINT 5
      25 PUSHCONST2 
      26 PUSHCONST1 
      27 PUSHCONSTINT 4
      29 PUSHCONST3 
      30 PUSHACC0 
      31 PUSHACC2 
      32 PUSHACC4 
      33 PUSHACC6 
      34 PUSHACC 8
      36 CLOSURE 5, 11
      39 PUSHCONST3 
      40 PUSHCONST0 
      41 PUSHACC2 
      42 APPLY1 
      43 NEQ 
      44 BRANCHIFNOT 51
      46 GETGLOBAL Not_found
      48 MAKEBLOCK1 0
      50 RAISE 
      51 POP 6
      53 ATOM0 
      54 SETGLOBAL T171-envacc
      56 STOP 
**)
