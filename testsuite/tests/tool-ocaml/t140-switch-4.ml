open Lib;;
match -1 with
| 0 -> raise Not_found
| 1 -> raise Not_found
| _ -> ()
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT -1
      11 PUSHACC0 
      12 SWITCH 
        int 0 -> 18
        int 1 -> 23
      16 BRANCH 28
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE 
      23 GETGLOBAL Not_found
      25 MAKEBLOCK1 0
      27 RAISE 
      28 CONST0 
      29 POP 1
      31 ATOM0 
      32 SETGLOBAL T140-switch-4
      34 STOP 
**)
