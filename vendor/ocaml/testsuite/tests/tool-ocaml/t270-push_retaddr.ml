open Lib;;
let f a b c d = 123 in
if f 0 1 2 3 <> 123 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 18
      11 RESTART
      12 GRAB 3
      14 CONSTINT 123
      16 RETURN 4
      18 CLOSURE 0, 12
      21 PUSHCONSTINT 123
      23 PUSH
      24 PUSH_RETADDR 34
      26 CONST3
      27 PUSHCONST2
      28 PUSHCONST1
      29 PUSHCONST0
      30 PUSHACC 8
      32 APPLY 4
      34 NEQ
      35 BRANCHIFNOT 42
      37 GETGLOBAL Not_found
      39 MAKEBLOCK1 0
      41 RAISE
      42 POP 1
      44 ATOM0
      45 SETGLOBAL T270-push_retaddr
      47 STOP
**)
