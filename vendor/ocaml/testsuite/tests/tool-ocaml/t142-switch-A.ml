open Lib;;
type t =
 | A
 | B of int
 | C of int
;;

match C 0 with
| C _ -> ()
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
        int 0 -> 20
        tag 0 -> 20
        tag 1 -> 17
      17 CONST0 
      18 BRANCH 25
      20 GETGLOBAL Not_found
      22 MAKEBLOCK1 0
      24 RAISE 
      25 POP 1
      27 ATOM0 
      28 SETGLOBAL T142-switch-A
      30 STOP 
**)
