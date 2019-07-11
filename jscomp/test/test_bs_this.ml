let uux_this : ([%bs.obj: < length: int > ] -> int -> int -> int[@bs.this]) =
 fun [@bs.this] o x y -> o##length + x + y

let even = fun [@bs.this] o x -> x + o

let bark () =
 fun [@bs.this] (o : 'self) x y ->
  Js.log (o##length, o##x, o##y, x, y) ;
  x + y

let js_obj : 'self =
  [%bs.obj {bark= (fun [@bs.this] (o : 'self) x y -> Js.log o ; x + y)}]

class type _x =
  object
    method onload : (_x Js.t -> unit[@bs.this]) [@@bs.set]

    method addEventListener : string -> ((_x Js.t -> unit)[@bs.this]) -> unit

    method response : string
  end[@bs]

type x = _x Js.t

let f (x : x) =
  x ## onload #= (fun [@bs.this] o -> Js.log o) ;
  x##addEventListener "onload" (fun [@bs.this] o -> Js.log o##response)

let u = fun [@bs.this] (_ : int) (x : int) -> x
