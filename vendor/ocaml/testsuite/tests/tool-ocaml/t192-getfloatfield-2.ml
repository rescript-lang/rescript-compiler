open Lib;;
type t = { a : float; b : float };;

if { a = 0.1; b = 0.2 }.b <> 0.2 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0.2
      11 PUSHGETGLOBAL [|0.1, 0.2|]
      13 GETFLOATFIELD 1
      15 C_CALL2 neq_float
      17 BRANCHIFNOT 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE
      24 ATOM0
      25 SETGLOBAL T192-getfloatfield-2
      27 STOP
**)
