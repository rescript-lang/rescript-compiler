open Lib;;
type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable d : int;
};;

let x = {a = 7; b = 6; c = 5; d = 4} in
x.d <- 11;
if x.d <> 11 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 4
      11 PUSHCONSTINT 5
      13 PUSHCONSTINT 6
      15 PUSHCONSTINT 7
      17 MAKEBLOCK 4, 0
      20 PUSHCONSTINT 11
      22 PUSHACC1 
      23 SETFIELD3 
      24 CONSTINT 11
      26 PUSHACC1 
      27 GETFIELD3 
      28 NEQ 
      29 BRANCHIFNOT 36
      31 GETGLOBAL Not_found
      33 MAKEBLOCK1 0
      35 RAISE 
      36 ACC0 
      37 POP 1
      39 ATOM0 
      40 SETGLOBAL T210-setfield3
      42 STOP 
**)
