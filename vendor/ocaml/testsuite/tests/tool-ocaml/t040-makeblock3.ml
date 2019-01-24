type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
};;

{ a = 0; b = 0; c = 0 };;

(**
       0 CONST0
       1 PUSHCONST0
       2 PUSHCONST0
       3 MAKEBLOCK3 0
       5 ATOM0
       6 SETGLOBAL T040-makeblock3
       8 STOP
**)
