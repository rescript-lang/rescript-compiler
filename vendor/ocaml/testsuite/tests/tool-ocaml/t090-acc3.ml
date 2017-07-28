open Lib;;
let x = true in
let y = false in
let z = false in
let a = false in
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
      14 ACC3 
      15 BOOLNOT 
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE 
      23 POP 4
      25 ATOM0 
      26 SETGLOBAL T090-acc3
      28 STOP 
**)
