open Lib;;
let x = ref 32 in
incr x;
if !x <> 33 then raise Not_found;
x
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 32
      11 MAKEBLOCK1 0
      13 PUSHACC0
      14 OFFSETREF 1
      16 CONSTINT 33
      18 PUSHACC1
      19 GETFIELD0
      20 NEQ
      21 BRANCHIFNOT 28
      23 GETGLOBAL Not_found
      25 MAKEBLOCK1 0
      27 RAISE
      28 ACC0
      29 POP 1
      31 ATOM0
      32 SETGLOBAL T260-offsetref
      34 STOP
**)
