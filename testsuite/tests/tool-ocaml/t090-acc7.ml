open Lib;;
let x = true in
let y = false in
let z = false in
let a = false in
let b = false in
let c = false in
let d = false in
let e = false in
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
      17 PUSHCONST0 
      18 ACC7 
      19 BOOLNOT 
      20 BRANCHIFNOT 27
      22 GETGLOBAL Not_found
      24 MAKEBLOCK1 0
      26 RAISE 
      27 POP 8
      29 ATOM0 
      30 SETGLOBAL T090-acc7
      32 STOP 
**)
