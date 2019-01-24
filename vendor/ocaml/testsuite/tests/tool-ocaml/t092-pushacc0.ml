open Lib;;
let x = false in
if x then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 PUSHACC0
      11 BRANCHIFNOT 18
      13 GETGLOBAL Not_found
      15 MAKEBLOCK1 0
      17 RAISE
      18 POP 1
      20 ATOM0
      21 SETGLOBAL T092-pushacc0
      23 STOP
**)
