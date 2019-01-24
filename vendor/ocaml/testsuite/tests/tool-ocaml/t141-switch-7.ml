open Lib;;
type t =
 | A of int
 | B of int
 | C of int
;;

match C 0 with
| A _ -> raise Not_found
| B _ -> raise Not_found
| _ -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL <2>(0)
      11 PUSHACC0
      12 SWITCH
        tag 0 -> 17
        tag 1 -> 22
        tag 2 -> 27
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 GETGLOBAL Not_found
      24 MAKEBLOCK1 0
      26 RAISE
      27 CONST0
      28 POP 1
      30 ATOM0
      31 SETGLOBAL T141-switch-7
      33 STOP
**)
