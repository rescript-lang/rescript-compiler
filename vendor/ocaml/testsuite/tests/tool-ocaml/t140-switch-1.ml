open Lib;;
match 0 with
| 0 -> ()
| 1 -> raise Not_found
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
        int 0 -> 17
        int 1 -> 20
      15 BRANCH 25
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
      33 SETGLOBAL T140-switch-1
      35 STOP 
**)
