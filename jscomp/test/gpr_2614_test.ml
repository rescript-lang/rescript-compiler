
[@@@ocaml.warning "-a"]

type t = {

  hi : int ;
    (* [@bs.as "hi"] *)
  low : int
} [@@bs.deriving abstract]
  (* [@@bs.x] *)
