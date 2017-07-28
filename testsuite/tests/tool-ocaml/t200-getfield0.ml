open Lib;;
type t = {
  a : int;
};;

if { a = 7 }.a <> 7 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 7
      11 PUSHGETGLOBAL <0>(7)
      13 GETFIELD0 
      14 NEQ 
      15 BRANCHIFNOT 22
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE 
      22 ATOM0 
      23 SETGLOBAL T200-getfield0
      25 STOP 
**)
