open Lib;;
let f _ = 12 in
let g _ = f 0 in
if g 0 <> 12 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 19
      11 CONST0 
      12 PUSHENVACC1 
      13 APPTERM1 2
      15 CONSTINT 12
      17 RETURN 1
      19 CLOSURE 0, 15
      22 PUSHACC0 
      23 CLOSURE 1, 11
      26 PUSHCONSTINT 12
      28 PUSHCONST0 
      29 PUSHACC2 
      30 APPLY1 
      31 NEQ 
      32 BRANCHIFNOT 39
      34 GETGLOBAL Not_found
      36 MAKEBLOCK1 0
      38 RAISE 
      39 POP 2
      41 ATOM0 
      42 SETGLOBAL T180-appterm1
      44 STOP 
**)
