open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
let b = true in
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
      14 PUSHACC4 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 POP 5
      24 ATOM0 
      25 SETGLOBAL T092-pushacc4
      27 STOP 
**)
