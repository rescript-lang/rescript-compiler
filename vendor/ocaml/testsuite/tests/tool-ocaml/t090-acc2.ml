open Lib;;
let x = true in
let y = false in
let z = false in
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
      13 ACC2 
      14 BOOLNOT 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 POP 3
      24 ATOM0 
      25 SETGLOBAL T090-acc2
      27 STOP 
**)
