open Lib;;
let x = 5 in
let y = 2 in
let z = 1 in
let f _ = ignore x; ignore y; z in
if f 0 <> 1 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 18
      11 ENVACC1
      12 CONST0
      13 ENVACC2
      14 CONST0
      15 ENVACC3
      16 RETURN 1
      18 CONSTINT 5
      20 PUSHCONST2
      21 PUSHCONST1
      22 PUSHACC0
      23 PUSHACC2
      24 PUSHACC4
      25 CLOSURE 3, 11
      28 PUSHCONST1
      29 PUSHCONST0
      30 PUSHACC2
      31 APPLY1
      32 NEQ
      33 BRANCHIFNOT 40
      35 GETGLOBAL Not_found
      37 MAKEBLOCK1 0
      39 RAISE
      40 POP 4
      42 ATOM0
      43 SETGLOBAL T170-envacc3
      45 STOP
**)
