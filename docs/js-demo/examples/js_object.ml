
let type_safe_json_data = [%bs.obj [|
    { x = 3 ; y = "hey" };
    { x =  32; y = "xx" }
|] ]

external optional_json_data : hi:int -> ?lo:int -> unit -> _ =
    "" [@@bs.obj]

let data =  [|
  optional_json_data ~hi:3 ();
  optional_json_data ~hi:32 ~lo:3 ()
|]

let obj =
  object(self)
    val mutable count = 0
    method say x y = self##hi x +. y
    method private hi x = x *. x
    method incrCount () =
      self##count #= (self##count + 1)
    method getCount () =
      self##count
  end [@bs]

let () = Js.log (obj##say 1. 2.)
