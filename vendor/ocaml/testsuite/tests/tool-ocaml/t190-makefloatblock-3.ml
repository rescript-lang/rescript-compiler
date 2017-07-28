open Lib;;
let x = 0.0 in [| x; x; x |];;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0
      11 PUSHACC0 
      12 PUSHACC1 
      13 PUSHACC2 
      14 MAKEFLOATBLOCK 3
      16 POP 1
      18 ATOM0 
      19 SETGLOBAL T190-makefloatblock-3
      21 STOP 
**)
