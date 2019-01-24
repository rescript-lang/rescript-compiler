open Lib;;
if "foo".[2] <> 'o' then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 111
      11 PUSHCONST2
      12 PUSHGETGLOBAL "foo"
      14 GETSTRINGCHAR
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 ATOM0
      24 SETGLOBAL T120-getstringchar
      26 STOP
**)
