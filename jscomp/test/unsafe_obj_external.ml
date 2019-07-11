external config :
     x:(('self_type -> 'x)[@bs.this])
  -> say:(('self_type -> 'x -> 'say)[@bs.this])
  -> (< x: unit -> 'x [@bs.meth] ; say: 'x -> 'say [@bs.meth] > Js.t
      as
      'self_type) = ""
  [@@bs.obj]

let v =
  let x = 3 in
  config
    ~x:(fun [@bs.this] _ -> x)
    ~say:(fun [@bs.this] self x -> self##x () + x)

(** let x = 3 in object (self : 'self_type) method x () = x method say x =
    self##x + x end [@bs] *)
let u = v##x () + v##say 3

(* local variables: *)
(* compile-command: "bsc.exe -I ../runtime -drawlambda unsafe_obj_external.ml" *)
(* end: *)
