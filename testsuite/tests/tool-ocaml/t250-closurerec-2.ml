open Lib;;
let rec f _ = 23 in
if f 0 <> 23 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 15
      11 CONSTINT 23
      13 RETURN 1
      15 CLOSUREREC 0, 11
      19 CONSTINT 23
      21 PUSHCONST0 
      22 PUSHACC2 
      23 APPLY1 
      24 NEQ 
      25 BRANCHIFNOT 32
      27 GETGLOBAL Not_found
      29 MAKEBLOCK1 0
      31 RAISE 
      32 POP 1
      34 ATOM0 
      35 SETGLOBAL T250-closurerec-2
      37 STOP 
**)
