class type ['a] case =
  object
    method case : int -> 'a

    method case__set : int -> 'a -> unit
  end[@bs]

(* let f (x : [%bs.obj: < case : int -> 'a ; *)
(* case__set : int -> int -> unit ; *)
(* .. > [@bs] ] ) *)
(* = *)
(* x ## case__set 3 2 ; *)
(* x ## case 3 *)

let ff (x : int case Js.t) =
  x##case__set 3 2 ;
  x##case 3

type 'a return = (int -> 'a[@bs])

let h (x : [%bs.obj: < cse: int -> 'a return [@bs] ; .. > ]) =
  ((x#@cse 3) 2 [@bs])

type x_obj =
  [%bs.obj: < cse: int -> int [@bs] ; cse__st: int -> int -> unit [@bs] > ]

let f_ext (x : x_obj) =
  x#@cse__st 3 2 ;
  x#@cse 3

type 'a h_obj = [%bs.obj: < cse: int -> 'a return [@bs] > ]

let h_ext (x : 'a h_obj) = ((x#@cse 3) 2 [@bs])
