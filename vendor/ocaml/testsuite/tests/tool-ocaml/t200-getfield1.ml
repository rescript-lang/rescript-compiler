open Lib;;
type t = {
  a : int;
  b : int;
};;

if { a = 7; b = 6 }.b <> 6 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 6
      11 PUSHGETGLOBAL <0>(7, 6)
      13 GETFIELD1
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 ATOM0
      23 SETGLOBAL T200-getfield1
      25 STOP
**)
