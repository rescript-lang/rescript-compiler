open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
let b = true in
let c = true in
let d = true in
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
      16 PUSHACC6
      17 BRANCHIFNOT 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE
      24 POP 7
      26 ATOM0
      27 SETGLOBAL T092-pushacc6
      29 STOP
**)
