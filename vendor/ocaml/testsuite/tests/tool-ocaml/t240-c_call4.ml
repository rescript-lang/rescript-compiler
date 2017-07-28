open Lib;;
let s = "abcdefgh" in
String.unsafe_fill s 0 6 'x';
if s.[5] <> 'x' then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL "abcdefgh"
      11 PUSHCONSTINT 120
      13 PUSHCONSTINT 6
      15 PUSHCONST0 
      16 PUSHACC3 
      17 C_CALL4 fill_string
      19 CONSTINT 120
      21 PUSHCONSTINT 5
      23 PUSHACC2 
      24 GETSTRINGCHAR 
      25 NEQ 
      26 BRANCHIFNOT 33
      28 GETGLOBAL Not_found
      30 MAKEBLOCK1 0
      32 RAISE 
      33 POP 1
      35 ATOM0 
      36 SETGLOBAL T240-c_call4
      38 STOP 
**)
