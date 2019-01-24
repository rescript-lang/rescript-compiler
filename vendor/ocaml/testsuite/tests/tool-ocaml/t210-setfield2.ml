open Lib;;
type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
};;

let x = {a = 7; b = 6; c = 5} in
x.c <- 11;
if x.c <> 11 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 5
      11 PUSHCONSTINT 6
      13 PUSHCONSTINT 7
      15 MAKEBLOCK3 0
      17 PUSHCONSTINT 11
      19 PUSHACC1
      20 SETFIELD2
      21 CONSTINT 11
      23 PUSHACC1
      24 GETFIELD2
      25 NEQ
      26 BRANCHIFNOT 33
      28 GETGLOBAL Not_found
      30 MAKEBLOCK1 0
      32 RAISE
      33 ACC0
      34 POP 1
      36 ATOM0
      37 SETGLOBAL T210-setfield2
      39 STOP
**)
