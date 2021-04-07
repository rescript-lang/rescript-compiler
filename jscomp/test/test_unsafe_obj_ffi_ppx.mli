

class type v = object 
  method height : int  [@@set]
  method width  : int [@@set]
end


class type ['a] g = object 
  method method1 : int -> unit 
  method method2 : int -> int -> 'a
end


val f : < length : float; width : float; .. >  -> float

val h : v -> unit

val chain : < element : < length : int; .. > ; .. >  -> int

val g : 'a g -> 'a

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

