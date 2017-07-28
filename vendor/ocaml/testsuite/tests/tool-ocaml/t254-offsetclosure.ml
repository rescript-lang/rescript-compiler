open Lib;;
let rec f _ = 11
    and g _ = 0
    and h _ = f
in
if h 3 4 <> 11 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 22
      11 CONSTINT 11
      13 RETURN 1
      15 CONST0 
      16 RETURN 1
      18 OFFSETCLOSURE -4
      20 RETURN 1
      22 CLOSUREREC 0, 11, 15, 18
      28 CONSTINT 11
      30 PUSHCONSTINT 4
      32 PUSHCONST3 
      33 PUSHACC3 
      34 APPLY2 
      35 NEQ 
      36 BRANCHIFNOT 43
      38 GETGLOBAL Not_found
      40 MAKEBLOCK1 0
      42 RAISE 
      43 POP 3
      45 ATOM0 
      46 SETGLOBAL T254-offsetclosure
      48 STOP 
**)
