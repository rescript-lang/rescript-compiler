

val f : < length : float; width : float; .. > Js.t -> float

val h : < height_set : int -> unit [@fn];
          width_set : int -> unit [@fn];
          .. > Js.t -> unit

val chain : < element : < length : int; .. > Js.t; .. > Js.t -> int

val g :
  < method1 : 
      (int -> unit [@fn][@hey]); 

    method2 : 
      (int ->  int -> 'a [@fn]);
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

