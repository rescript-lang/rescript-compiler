module Mod = struct
  type t = int
end
module Aliased = Mod

let tuple = ("string", 1, 2.0, true, false)

let aliased: Aliased.t = 1

let js_obj = [%bs.obj { name = "nam"; id = 2 }]
  
type rec_t = { name: string; id: int; }

let record = { name = "rec"; id = 1; }

class type obj_t = object
  method name : string
  method id : int
end [@bs]

let obj_arg (o : obj_t) = ()

let arr = [|1; 2; 3|]

let from_util (a: Util.t) = ()

type tt = int
let tt: tt = 0

let iter items fn = Array.iter fn items

type 'a pair = 'a * 'a

let pair (a : 'a pair) = ()

let tuple_with_fn = (fun a -> pair a), 1
