open Lib;;
type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable d : int;
  mutable e : int;
};;

let x = {a = 7; b = 6; c = 5; d = 4; e = 5} in
x.e <- 11;
if x.e <> 11 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 5
      11 PUSHCONSTINT 4
      13 PUSHCONSTINT 5
      15 PUSHCONSTINT 6
      17 PUSHCONSTINT 7
      19 MAKEBLOCK 5, 0
      22 PUSHCONSTINT 11
      24 PUSHACC1
      25 SETFIELD 4
      27 CONSTINT 11
      29 PUSHACC1
      30 GETFIELD 4
      32 NEQ
      33 BRANCHIFNOT 40
      35 GETGLOBAL Not_found
      37 MAKEBLOCK1 0
      39 RAISE
      40 ACC0
      41 POP 1
      43 ATOM0
      44 SETGLOBAL T211-setfield
      46 STOP
**)
