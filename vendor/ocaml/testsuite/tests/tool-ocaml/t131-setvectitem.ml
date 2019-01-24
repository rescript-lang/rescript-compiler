open Lib;;
let x = [| 1; 2 |] in
x.(0) <- 3;
if x.(0) <> 3 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST2
      10 PUSHCONST1
      11 MAKEBLOCK2 0
      13 PUSHCONST3
      14 PUSHCONST0
      15 PUSHACC2
      16 SETVECTITEM
      17 CONST3
      18 PUSHCONST0
      19 PUSHACC2
      20 GETVECTITEM
      21 NEQ
      22 BRANCHIFNOT 29
      24 GETGLOBAL Not_found
      26 MAKEBLOCK1 0
      28 RAISE
      29 POP 1
      31 ATOM0
      32 SETGLOBAL T131-setvectitem
      34 STOP
**)
