open Lib;;
let x = false in
let y = true in
if x then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 PUSHCONST1
      11 PUSHACC1
      12 BRANCHIFNOT 19
      14 GETGLOBAL Not_found
      16 MAKEBLOCK1 0
      18 RAISE
      19 POP 2
      21 ATOM0
      22 SETGLOBAL T092-pushacc1
      24 STOP
**)
