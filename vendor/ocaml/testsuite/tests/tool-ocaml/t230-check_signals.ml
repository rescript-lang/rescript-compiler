open Lib;;
for i = 0 to 0 do () done;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 PUSHCONST0
      11 PUSH
      12 BRANCH 21
      14 CHECK_SIGNALS
      15 CONST0
      16 ACC1
      17 OFFSETINT 1
      19 ASSIGN 1
      21 ACC0
      22 PUSHACC2
      23 LEINT
      24 BRANCHIF 14
      26 CONST0
      27 POP 2
      29 ATOM0
      30 SETGLOBAL T230-check_signals
      32 STOP
**)
