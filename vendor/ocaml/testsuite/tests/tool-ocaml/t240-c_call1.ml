open Lib;;
if Pervasives.int_of_string "123" <> 123 then raise Not_found;;
(** test for fix of bug 6649: http://caml.inria.fr/mantis/view.php?id=6649 *)
if Pervasives.int_of_string "+123" <> 123 then raise Not_found;;

if Int32.of_string "+123" <> Int32.of_int 123 then raise Not_found;;
if Int64.of_string "+123" <> Int64.of_int 123 then raise Not_found;;
if Nativeint.of_string "+123" <> Nativeint.of_int 123 then raise Not_found;;

(**
       0 CONSTINT 42
       2 PUSHACC0
       3 MAKEBLOCK1 0
       5 POP 1
       7 SETGLOBAL Lib
       9 CONSTINT 123
      11 PUSHGETGLOBAL "123"
      13 C_CALL1 int_of_string
      15 NEQ
      16 BRANCHIFNOT 23
      18 GETGLOBAL Not_found
      20 MAKEBLOCK1 0
      22 RAISE
      23 ATOM0
      24 SETGLOBAL T240-c_call1
      26 STOP
**)
