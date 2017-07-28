open Lib;;
match 1 with
| 0 -> raise Not_found
| 1 -> ()
| _ -> raise Not_found
;;

(**
       0 CONSTINT 42
       2 PUSHACC0 
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONST1 
      10 PUSHACC0 
      11 SWITCH 
        int 0 -> 17
        int 1 -> 22
      15 BRANCH 25
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
      33 SETGLOBAL T140-switch-2
      35 STOP 
**)
