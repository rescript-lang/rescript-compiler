open Lib;;
let x = 0.0 in [| x; x |];;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0
      11 PUSHACC0 
      12 PUSHACC1 
      13 MAKEFLOATBLOCK 2
      15 POP 1
      17 ATOM0 
      18 SETGLOBAL T190-makefloatblock-2
      20 STOP 
**)
