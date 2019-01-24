(* Test construction of cyclic values where the cycles pass through references *)

type t = { mutable next : t; mutable inst : n ref }
and n = T of t

let rec d = { next = d; inst = ref (T d) }

let f t1 t2 =
  let rec self = ref init
  and init () = t1 (function () -> self := t2; t2 ())
  in fun () -> !self ()
;;
