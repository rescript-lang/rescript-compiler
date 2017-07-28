open Lib;;
let x = true in
let y = false in
let z = false in
let a = false in
let b = false in
let c = false in
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
      16 ACC5 
      17 BOOLNOT 
      18 BRANCHIFNOT 25
      20 GETGLOBAL Not_found
      22 MAKEBLOCK1 0
      24 RAISE 
      25 POP 6
      27 ATOM0 
      28 SETGLOBAL T090-acc5
      30 STOP 
**)
