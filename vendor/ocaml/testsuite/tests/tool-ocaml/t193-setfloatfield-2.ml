open Lib;;
type t = {
  mutable a : float;
  mutable b : float;
};;

let x = { a = 0.1; b = 0.2 } in
x.b <- 0.3;
if x.b <> 0.3 then raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL 0.2
      11 PUSHGETGLOBAL 0.1
      13 MAKEFLOATBLOCK 2
      15 PUSHGETGLOBAL 0.3
      17 PUSHACC1
      18 SETFLOATFIELD 1
      20 GETGLOBAL 0.3
      22 PUSHACC1
      23 GETFLOATFIELD 1
      25 C_CALL2 neq_float
      27 BRANCHIFNOT 34
      29 GETGLOBAL Not_found
      31 MAKEBLOCK1 0
      33 RAISE
      34 POP 1
      36 ATOM0
      37 SETGLOBAL T193-setfloatfield-2
      39 STOP
**)
