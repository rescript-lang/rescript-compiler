open Lib;;
let f _ _ = 12 in
let g _ = f 0 0 in
if g 0 <> 12 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 23
      11 CONST0 
      12 PUSHCONST0 
      13 PUSHENVACC1 
      14 APPTERM2 3
      16 RESTART 
      17 GRAB 1
      19 CONSTINT 12
      21 RETURN 2
      23 CLOSURE 0, 17
      26 PUSHACC0 
      27 CLOSURE 1, 11
      30 PUSHCONSTINT 12
      32 PUSHCONST0 
      33 PUSHACC2 
      34 APPLY1 
      35 NEQ 
      36 BRANCHIFNOT 43
      38 GETGLOBAL Not_found
      40 MAKEBLOCK1 0
      42 RAISE 
      43 POP 2
      45 ATOM0 
      46 SETGLOBAL T180-appterm2
      48 STOP 
**)
