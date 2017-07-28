open Lib;;
let rec f x = x
    and g _ = f 4
    and h _ = f 6
in
if h 1 <> 6 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 25
      11 ACC0 
      12 RETURN 1
      14 CONSTINT 4
      16 PUSHOFFSETCLOSUREM2 
      17 APPTERM1 2
      19 CONSTINT 6
      21 PUSHOFFSETCLOSURE -4
      23 APPTERM1 2
      25 CLOSUREREC 0, 11, 14, 19
      31 CONSTINT 6
      33 PUSHCONST1 
      34 PUSHACC2 
      35 APPLY1 
      36 NEQ 
      37 BRANCHIFNOT 44
      39 GETGLOBAL Not_found
      41 MAKEBLOCK1 0
      43 RAISE 
      44 POP 3
      46 ATOM0 
      47 SETGLOBAL T252-pushoffsetclosure
      49 STOP 
**)
