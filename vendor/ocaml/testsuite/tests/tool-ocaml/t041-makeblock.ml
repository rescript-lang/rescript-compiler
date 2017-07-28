type t = {
  mutable a : int;
  mutable b : int;
  mutable c : int;
  mutable d : int;
};;

{ a = 0; b = 0; c = 0; d = 0 };;

(**
       0 CONST0 
       1 PUSHCONST0 
       2 PUSHCONST0 
       3 PUSHCONST0 
       4 MAKEBLOCK 4, 0
       7 ATOM0 
       8 SETGLOBAL T041-makeblock
      10 STOP 
**)
