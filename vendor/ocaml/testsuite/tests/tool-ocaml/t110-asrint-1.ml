open Lib;;
if (-2 asr 1) <> -1 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT -1
      11 PUSHCONST1
      12 PUSHCONSTINT -2
      14 ASRINT
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 ATOM0
      24 SETGLOBAL T110-asrint-1
      26 STOP
**)
