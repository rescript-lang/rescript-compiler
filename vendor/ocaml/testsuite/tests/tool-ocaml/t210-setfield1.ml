open Lib;;
type t = {
  mutable a : int;
  mutable b : int;
};;

let x = {a = 7; b = 6} in
x.b <- 11;
if x.b <> 11 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 6
      11 PUSHCONSTINT 7
      13 MAKEBLOCK2 0
      15 PUSHCONSTINT 11
      17 PUSHACC1
      18 SETFIELD1
      19 CONSTINT 11
      21 PUSHACC1
      22 GETFIELD1
      23 NEQ
      24 BRANCHIFNOT 31
      26 GETGLOBAL Not_found
      28 MAKEBLOCK1 0
      30 RAISE
      31 ACC0
      32 POP 1
      34 ATOM0
      35 SETGLOBAL T210-setfield1
      37 STOP
**)
