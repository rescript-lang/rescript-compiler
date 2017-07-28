open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
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
      11 PUSHCONST1 
      12 PUSHCONST1 
      13 PUSHACC3 
      14 BRANCHIFNOT 21
      16 GETGLOBAL Not_found
      18 MAKEBLOCK1 0
      20 RAISE 
      21 POP 4
      23 ATOM0 
      24 SETGLOBAL T092-pushacc3
      26 STOP 
**)
