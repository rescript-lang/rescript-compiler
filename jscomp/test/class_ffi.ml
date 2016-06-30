

class type x = 
  object
    inherit y
    val u : int 
    val v : int 
    method hh : int -> int -> int 
    method cmp : x -> bool 
  end [@@bs] 
and y = object 
end [@@y]

class type __x = object 
  inherit __y
  method u : int 
  method v : int 
  method hh : (int * int * int ) Js.meth
  method cmp : (__x Js.t * bool ) Js.meth
end
type _x = __x Js.t 
