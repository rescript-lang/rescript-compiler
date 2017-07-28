open Lib;;
let x = 0.0 in
if Array.length [| x |] <> 1 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0
      11 PUSHCONST1 
      12 PUSHACC1 
      13 MAKEFLOATBLOCK 1
      15 VECTLENGTH 
      16 NEQ 
      17 BRANCHIFNOT 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE 
      24 POP 1
      26 ATOM0 
      27 SETGLOBAL T191-vectlength
      29 STOP 
**)
