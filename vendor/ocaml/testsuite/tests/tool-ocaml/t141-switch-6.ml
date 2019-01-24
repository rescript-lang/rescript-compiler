open Lib;;
type t =
 | A of int
 | B of int
 | C of int
;;

match B 0 with
| A _ -> raise Not_found
| B _ -> ()
| _ -> raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL <1>(0)
      11 PUSHACC0
      12 SWITCH
        tag 0 -> 17
        tag 1 -> 22
        tag 2 -> 25
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 CONST0
      23 BRANCH 30
      25 GETGLOBAL Not_found
      27 MAKEBLOCK1 0
      29 RAISE
      30 POP 1
      32 ATOM0
      33 SETGLOBAL T141-switch-6
      35 STOP
**)
