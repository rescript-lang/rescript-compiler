open Lib;;
if Array.length [| 1; 2 |] <> 2 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST2
      10 PUSHCONST2
      11 PUSHCONST1
      12 MAKEBLOCK2 0
      14 VECTLENGTH
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 ATOM0
      24 SETGLOBAL T130-vectlength
      26 STOP
**)
