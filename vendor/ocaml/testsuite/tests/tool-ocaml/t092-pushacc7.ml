open Lib;;
let x = false in
let y = true in
let z = true in
let a = true in
let b = true in
let c = true in
let d = true in
let e = true in
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
      17 PUSHACC7
      18 BRANCHIFNOT 25
      20 GETGLOBAL Not_found
      22 MAKEBLOCK1 0
      24 RAISE
      25 POP 8
      27 ATOM0
      28 SETGLOBAL T092-pushacc7
      30 STOP
**)
