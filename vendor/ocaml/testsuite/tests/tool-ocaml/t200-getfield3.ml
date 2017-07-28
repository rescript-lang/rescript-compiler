open Lib;;
type t = {
  a : int;
  b : int;
  c : int;
  d : int;
};;

if { a = 7; b = 6; c = 5; d = 4 }.d <> 4 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 4
      11 PUSHGETGLOBAL <0>(7, 6, 5, 4)
      13 GETFIELD3 
      14 NEQ 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 ATOM0 
      23 SETGLOBAL T200-getfield3
      25 STOP 
**)
