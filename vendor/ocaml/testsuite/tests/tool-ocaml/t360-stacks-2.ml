open Lib;;
let rec f n =
  if n <= 0 then 12
  else 1 + f (n-1)
in
try
  ignore (f 3000000);
  raise Not_found
with Stack_overflow -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 29
      11 CONST0
      12 PUSHACC1
      13 LEINT
      14 BRANCHIFNOT 20
      16 CONSTINT 12
      18 RETURN 1
      20 ACC0
      21 OFFSETINT -1
      23 PUSHOFFSETCLOSURE0
      24 APPLY1
      25 PUSHCONST1
      26 ADDINT
      27 RETURN 1
      29 CLOSUREREC 0, 11
      33 PUSHTRAP 44
      35 CONSTINT 3000000
      37 PUSHACC5
      38 APPLY1
      39 GETGLOBAL Not_found
      41 MAKEBLOCK1 0
      43 RAISE
      44 PUSHGETGLOBAL Stack_overflow
      46 PUSHACC1
      47 GETFIELD0
      48 EQ
      49 BRANCHIFNOT 54
      51 CONST0
      52 BRANCH 56
      54 ACC0
      55 RAISE
      56 POP 1
      58 POP 1
      60 ATOM0
      61 SETGLOBAL T360-stacks-2
      63 STOP
**)
