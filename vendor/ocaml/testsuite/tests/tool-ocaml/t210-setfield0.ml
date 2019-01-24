open Lib;;
type t = {
  mutable a : int;
};;

let x = {a = 7} in
x.a <- 11;
if x.a <> 11 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 7
      11 MAKEBLOCK1 0
      13 PUSHCONSTINT 11
      15 PUSHACC1
      16 SETFIELD0
      17 CONSTINT 11
      19 PUSHACC1
      20 GETFIELD0
      21 NEQ
      22 BRANCHIFNOT 29
      24 GETGLOBAL Not_found
      26 MAKEBLOCK1 0
      28 RAISE
      29 ACC0
      30 POP 1
      32 ATOM0
      33 SETGLOBAL T210-setfield0
      35 STOP
**)
