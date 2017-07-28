open Lib;;
let x = 5 in
let y = 2 in
let f _ = ignore x; y in
if f 0 <> 2 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 16
      11 ENVACC1 
      12 CONST0 
      13 ENVACC2 
      14 RETURN 1
      16 CONSTINT 5
      18 PUSHCONST2 
      19 PUSHACC0 
      20 PUSHACC2 
      21 CLOSURE 2, 11
      24 PUSHCONST2 
      25 PUSHCONST0 
      26 PUSHACC2 
      27 APPLY1 
      28 NEQ 
      29 BRANCHIFNOT 36
      31 GETGLOBAL Not_found
      33 MAKEBLOCK1 0
      35 RAISE 
      36 POP 3
      38 ATOM0 
      39 SETGLOBAL T170-envacc2
      41 STOP 
**)
