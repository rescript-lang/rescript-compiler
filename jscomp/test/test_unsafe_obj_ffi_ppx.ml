class type _v =
  object
    method height : int [@@bs.set]

    method width : int [@@bs.set]
  end[@bs]

type v = _v Js.t

class type ['a] _g =
  object
    method method1 : int -> unit

    method method2 : int -> int -> 'a
  end[@bs]

type 'a g = 'a _g Js.t

let f x = x##length +. x##width
let i () = ()

let h x : unit =
  x ## height #= 3 ;
  i @@ (x ## width #= 3)

let chain x = x##element##length + x##element##length

(* current error message : Error: '##' is not a valid value identifier. *)
(* let syntax_error x = *)
(* x ## _set_height 3 3 *)

let g x =
  let () = x##method1 3 in
  x##method2 3 3
