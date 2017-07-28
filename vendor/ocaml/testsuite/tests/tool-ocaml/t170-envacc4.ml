open Lib;;
let x = 5 in
let y = 2 in
let z = 1 in
let a = 4 in
let f _ = ignore x; ignore y; ignore z; a in
if f 0 <> 4 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 20
      11 ENVACC1 
      12 CONST0 
      13 ENVACC2 
      14 CONST0 
      15 ENVACC3 
      16 CONST0 
      17 ENVACC4 
      18 RETURN 1
      20 CONSTINT 5
      22 PUSHCONST2 
      23 PUSHCONST1 
      24 PUSHCONSTINT 4
      26 PUSHACC0 
      27 PUSHACC2 
      28 PUSHACC4 
      29 PUSHACC6 
      30 CLOSURE 4, 11
      33 PUSHCONSTINT 4
      35 PUSHCONST0 
      36 PUSHACC2 
      37 APPLY1 
      38 NEQ 
      39 BRANCHIFNOT 46
      41 GETGLOBAL Not_found
      43 MAKEBLOCK1 0
      45 RAISE 
      46 POP 5
      48 ATOM0 
      49 SETGLOBAL T170-envacc4
      51 STOP 
**)
