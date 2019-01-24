open Lib;;
type t =
 | A
 | B of int
 | C of int
;;

match A with
| A -> ()
| _ -> raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST0
      10 PUSHACC0
      11 SWITCH
        int 0 -> 16
        tag 0 -> 19
        tag 1 -> 19
      16 CONST0
      17 BRANCH 24
      19 GETGLOBAL Not_found
      21 MAKEBLOCK1 0
      23 RAISE
      24 POP 1
      26 ATOM0
      27 SETGLOBAL T142-switch-8
      29 STOP
**)
