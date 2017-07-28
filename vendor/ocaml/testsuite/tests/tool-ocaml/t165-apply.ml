open Lib;;
let f _ _ _ _ = 0 in f 0 0 0 0;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 17
      11 RESTART 
      12 GRAB 3
      14 CONST0 
      15 RETURN 4
      17 CLOSURE 0, 12
      20 PUSH 
      21 PUSH_RETADDR 30
      23 CONST0 
      24 PUSHCONST0 
      25 PUSHCONST0 
      26 PUSHCONST0 
      27 PUSHACC7 
      28 APPLY 4
      30 POP 1
      32 ATOM0 
      33 SETGLOBAL T165-apply
      35 STOP 
**)
