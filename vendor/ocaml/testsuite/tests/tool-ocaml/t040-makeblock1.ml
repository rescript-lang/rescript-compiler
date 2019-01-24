type t = {
  mutable a : int;
};;

{ a = 0 };;

(**
       0 CONST0
       1 MAKEBLOCK1 0
       3 ATOM0
       4 SETGLOBAL T040-makeblock1
       6 STOP
**)
