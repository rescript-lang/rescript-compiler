open Lib;;
let f _ = raise End_of_file in
try
  f 0;
  raise Not_found;
with End_of_file -> 0
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 BRANCH 16
      11 GETGLOBAL End_of_file
      13 MAKEBLOCK1 0
      15 RAISE 
      16 CLOSURE 0, 11
      19 PUSH 
      20 PUSHTRAP 30
      22 CONST0 
      23 PUSHACC5 
      24 APPLY1 
      25 GETGLOBAL Not_found
      27 MAKEBLOCK1 0
      29 RAISE 
      30 PUSHGETGLOBAL End_of_file
      32 PUSHACC1 
      33 GETFIELD0 
      34 EQ 
      35 BRANCHIFNOT 40
      37 CONST0 
      38 BRANCH 42
      40 ACC0 
      41 RAISE 
      42 POP 1
      44 POP 1
      46 ATOM0 
      47 SETGLOBAL T161-apply1
      49 STOP 
**)
