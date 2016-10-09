module M = struct
  type t = int
end
module M1 = M

let do_if b = if b then true else false

let mk_tuple (int1: M1.t) int2 () = (int1 + 1, int2 + 2, "3", '4')

type rec_t = { name: string; id: int; }

let mk_rec ~name () = { name; id = 1; }

class type obj_t = object
  method name : string
  method id : int
end [@bs]

let mk_rec (o : obj_t) =
  {
    name = o#name;
    id = o#id;
  }

let mk_obj r = [%bs.obj { name = "nam"; id = 2 }]
  
let mk_arr () = [|1; 2; 3|]

let from_util (a: Util.t) = ()

let float_ = 1.0

type tt = int
let tt: tt = 0

let iter items1 items2 fn1 fn2 = Array.iter fn1 items1; Array.iter fn2 items2

type 'a pair = 'a * 'a

let pair (a : 'a pair) = (a, a)

let fn_in_obj = (
  (fun a -> pair a),
  1
)
