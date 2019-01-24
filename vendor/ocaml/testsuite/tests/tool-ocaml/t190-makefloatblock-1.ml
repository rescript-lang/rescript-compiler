open Lib;;
let x = 0.0 in [| x |];;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0
      11 PUSHACC0
      12 MAKEFLOATBLOCK 1
      14 POP 1
      16 ATOM0
      17 SETGLOBAL T190-makefloatblock-1
      19 STOP
**)
