open Lib;;
type t =
 | A of int
 | B of int
 | C of int
;;

match A 0 with
| A _ -> ()
| B _ -> raise Not_found
| _ -> raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 GETGLOBAL <0>(0)
      11 PUSHACC0
      12 SWITCH
        tag 0 -> 17
        tag 1 -> 20
        tag 2 -> 25
      17 CONST0
      18 BRANCH 30
      20 GETGLOBAL Not_found
      22 MAKEBLOCK1 0
      24 RAISE
      25 GETGLOBAL Not_found
      27 MAKEBLOCK1 0
      29 RAISE
      30 POP 1
      32 ATOM0
      33 SETGLOBAL T141-switch-5
      35 STOP
**)
