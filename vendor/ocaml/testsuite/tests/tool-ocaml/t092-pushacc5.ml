open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
let b = true in
let c = true in
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
      13 PUSHCONST1 
      14 PUSHCONST1 
      15 PUSHACC5 
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE 
      23 POP 6
      25 ATOM0 
      26 SETGLOBAL T092-pushacc5
      28 STOP 
**)
