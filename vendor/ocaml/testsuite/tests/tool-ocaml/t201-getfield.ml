open Lib;;
type t = {
  a : int;
  b : int;
  c : int;
  d : int;
  e : int;
};;

if { a = 7; b = 6; c = 5; d = 4; e = 3 }.e <> 3 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST3
      10 PUSHGETGLOBAL <0>(7, 6, 5, 4, 3)
      12 GETFIELD 4
      14 NEQ
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 ATOM0
      23 SETGLOBAL T201-getfield
      25 STOP
**)
