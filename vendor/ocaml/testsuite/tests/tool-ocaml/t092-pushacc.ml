open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
let b = true in
let c = true in
let d = true in
let e = true in
let f = true in
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
      15 PUSHCONST1 
      16 PUSHCONST1 
      17 PUSHCONST1 
      18 PUSHACC 8
      20 BRANCHIFNOT 27
      22 GETGLOBAL Not_found
      24 MAKEBLOCK1 0
      26 RAISE 
      27 POP 9
      29 ATOM0 
      30 SETGLOBAL T092-pushacc
      32 STOP 
**)
