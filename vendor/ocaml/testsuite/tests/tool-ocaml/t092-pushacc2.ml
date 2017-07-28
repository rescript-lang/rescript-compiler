open Lib;;
let x = false in
let y = true in
let z = true in
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
      12 PUSHACC2 
      13 BRANCHIFNOT 20
      15 GETGLOBAL Not_found
      17 MAKEBLOCK1 0
      19 RAISE 
      20 POP 3
      22 ATOM0 
      23 SETGLOBAL T092-pushacc2
      25 STOP 
**)
