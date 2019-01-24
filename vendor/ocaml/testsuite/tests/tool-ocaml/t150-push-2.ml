open Lib;;
let x = 1 in
try if x <> 1 then raise Not_found
with End_of_file -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1
      10 PUSH
      11 PUSHTRAP 26
      13 CONST1
      14 PUSHACC5
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 POPTRAP
      24 BRANCH 40
      26 PUSHGETGLOBAL End_of_file
      28 PUSHACC1
      29 GETFIELD0
      30 EQ
      31 BRANCHIFNOT 36
      33 CONST0
      34 BRANCH 38
      36 ACC0
      37 RAISE
      38 POP 1
      40 POP 1
      42 ATOM0
      43 SETGLOBAL T150-push-2
      45 STOP
**)
