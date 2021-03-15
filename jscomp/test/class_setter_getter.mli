(* class type _y = *)
(*   object method height#= : ([ `Arity_1 of int ], unit) Js.meth end *)
(* type y = _y  *)
(* class type _y0 = *)
(*   object *)
(*     method height : int Js.null *)
(*     method height#= : ([ `Arity_1 of int ], unit) Js.meth *)
(*   end *)
(* type y0 = _y0  *)
(* class type _y1 = *)
(*   object *)
(*     method height : int Js.undefined *)
(*     method height#= : ([ `Arity_1 of int ], unit) Js.meth *)
(*   end *)
(* type y1 = _y1  *)
(* class type _y2 = *)
(*   object *)
(*     method height : int Js.null_undefined *)
(*     method height#= : ([ `Arity_1 of int ], unit) Js.meth *)
(*   end *)
(* type y2 = _y2  *)
(* class type _y3 = object method height : int Js.null_undefined end *)
(* type y3 = _y3  *)


class type _y = object 
  method height : int [@@bs.set no_get]
end [@bs]
type y = _y  
class type _y0 = object 
  method height : int [@@bs.set] [@@bs.get null]
end [@bs]
type y0 = _y0  

class type _y1 = object 
  method height : int [@@bs.set] [@@bs.get undefined]
end[@bs]
type y1 = _y1  

class type _y2 = object 
  method height : int [@@bs.set] [@@bs.get nullable]
end [@bs]
type y2 = _y2  

class type _y3 = object 
  method height : int  [@@bs.get nullable]
end[@bs]
type y3 = _y3 


type yy2 = 
  < height : int 
             [@bs.get nullable]
             [@bs.set] > 


val fff : 
  yy2 -> unit 

val ff :     
  y2 ->
  yy2 -> 
  int Js.nullable list 
