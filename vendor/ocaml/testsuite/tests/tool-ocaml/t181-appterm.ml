open Lib;;
let f _ _ _ _ = -10 in
let g _ = f 0 0 0 0 in
if g 0 <> -10 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 26
      11 CONST0 
      12 PUSHCONST0 
      13 PUSHCONST0 
      14 PUSHCONST0 
      15 PUSHENVACC1 
      16 APPTERM 4, 5
      19 RESTART 
      20 GRAB 3
      22 CONSTINT -10
      24 RETURN 4
      26 CLOSURE 0, 20
      29 PUSHACC0 
      30 CLOSURE 1, 11
      33 PUSHCONSTINT -10
      35 PUSHCONST0 
      36 PUSHACC2 
      37 APPLY1 
      38 NEQ 
      39 BRANCHIFNOT 46
      41 GETGLOBAL Not_found
      43 MAKEBLOCK1 0
      45 RAISE 
      46 POP 2
      48 ATOM0 
      49 SETGLOBAL T181-appterm
      51 STOP 
**)
