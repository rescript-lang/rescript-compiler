open Lib;;
let x = ref 1 in
x := 3;
if !x <> 3 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSHCONST3
      11 ASSIGN 0
      13 CONST3
      14 PUSHACC1
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 POP 1
      25 ATOM0
      26 SETGLOBAL T220-assign
      28 STOP
**)
