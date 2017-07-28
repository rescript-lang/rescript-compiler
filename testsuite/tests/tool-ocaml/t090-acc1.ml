open Lib;;
let x = true in
let y = false in
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
      12 ACC1 
      13 BOOLNOT 
      14 BRANCHIFNOT 21
      16 GETGLOBAL Not_found
      18 MAKEBLOCK1 0
      20 RAISE 
      21 POP 2
      23 ATOM0 
      24 SETGLOBAL T090-acc1
      26 STOP 
**)
