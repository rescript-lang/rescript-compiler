open Lib;;
let rec f _ = g f
    and g _ = 10
in
if f 3 <> 10 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 19
      11 OFFSETCLOSURE0 
      12 PUSHOFFSETCLOSURE2 
      13 APPTERM1 2
      15 CONSTINT 10
      17 RETURN 1
      19 CLOSUREREC 0, 11, 15
      24 CONSTINT 10
      26 PUSHCONST3 
      27 PUSHACC3 
      28 APPLY1 
      29 NEQ 
      30 BRANCHIFNOT 37
      32 GETGLOBAL Not_found
      34 MAKEBLOCK1 0
      36 RAISE 
      37 POP 2
      39 ATOM0 
      40 SETGLOBAL T253-offsetclosure0
      42 STOP 
**)
