

val f : < length : float; width : float; .. > Js.t -> float

val h : < height__set : int -> unit [@uncurry];
          width__set : int -> unit [@uncurry];
          .. > Js.t -> unit

val chain : < element : < length : int; .. > Js.t; .. > Js.t -> int

val g :
  < method1 : 
      ((int * __) -> unit [@uncurry][@hey]); 

    method2 : 
      (int * int -> 'a [@uncurry]);
    .. > Js.t ->
  'a

(** Another proposal :

{[
  val g :
    [%bs.obj: < 
           method1 : int -> unit ; 
           method2 : int * int -> 'a ;
           ..
                    >
    ] -> 'a 
]}
*)

