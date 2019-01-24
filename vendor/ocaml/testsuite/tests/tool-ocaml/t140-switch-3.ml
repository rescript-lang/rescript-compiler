open Lib;;
match 2 with
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
       9 CONST2
      10 PUSHACC0
      11 SWITCH
        int 0 -> 17
        int 1 -> 22
      15 BRANCH 27
      17 GETGLOBAL Not_found
      19 MAKEBLOCK1 0
      21 RAISE
      22 GETGLOBAL Not_found
      24 MAKEBLOCK1 0
      26 RAISE
      27 CONST0
      28 POP 1
      30 ATOM0
      31 SETGLOBAL T140-switch-3
      33 STOP
**)
