module Gen_spec = struct type 't extra = unit end

module type S = sig
  module Spec : sig type 't extra = unit end

  type t
  val make : unit -> t Spec.extra
end (* S *)

module Make () : S with module Spec := Gen_spec = struct
  type t = int
  let make () = ()
end (* Make *)

let () =
  let module M = Make () in
  M.make ()
  (* (M.make () : unit) *)
