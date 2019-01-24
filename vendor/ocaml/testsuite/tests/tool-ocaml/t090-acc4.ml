open Lib;;
let x = true in
let y = false in
let z = false in
let a = false in
let b = false in
();
if not x then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONST0
      11 PUSHCONST0
      12 PUSHCONST0
      13 PUSHCONST0
      14 PUSHCONST0
      15 ACC4
      16 BOOLNOT
      17 BRANCHIFNOT 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE
      24 POP 5
      26 ATOM0
      27 SETGLOBAL T090-acc4
      29 STOP
**)
