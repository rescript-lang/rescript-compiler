open Lib;;
let x = true in
let y = false in
let z = false in
let a = false in
let b = false in
let c = false in
let d = false in
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
      15 PUSHCONST0 
      16 PUSHCONST0 
      17 ACC6 
      18 BOOLNOT 
      19 BRANCHIFNOT 26
      21 GETGLOBAL Not_found
      23 MAKEBLOCK1 0
      25 RAISE 
      26 POP 7
      28 ATOM0 
      29 SETGLOBAL T090-acc6
      31 STOP 
**)
