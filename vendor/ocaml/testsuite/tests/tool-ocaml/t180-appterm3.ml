open Lib;;
let f _ _ _ = 13 in
let g _ = f 0 0 0 in
if g 0 <> 13 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 24
      11 CONST0 
      12 PUSHCONST0 
      13 PUSHCONST0 
      14 PUSHENVACC1 
      15 APPTERM3 4
      17 RESTART 
      18 GRAB 2
      20 CONSTINT 13
      22 RETURN 3
      24 CLOSURE 0, 18
      27 PUSHACC0 
      28 CLOSURE 1, 11
      31 PUSHCONSTINT 13
      33 PUSHCONST0 
      34 PUSHACC2 
      35 APPLY1 
      36 NEQ 
      37 BRANCHIFNOT 44
      39 GETGLOBAL Not_found
      41 MAKEBLOCK1 0
      43 RAISE 
      44 POP 2
      46 ATOM0 
      47 SETGLOBAL T180-appterm3
      49 STOP 
**)
