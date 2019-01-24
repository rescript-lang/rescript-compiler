module A : module type of Array = ArrayLabels
module B : module type of Bytes = BytesLabels
module L : module type of List = ListLabels
module S : module type of String = StringLabels

module M : module type of Map = MoreLabels.Map
module Se : module type of Set = MoreLabels.Set


(* For  *)
(* module H : module type of Hashtbl = MoreLabels.Hashtbl *)
(* we will have following error: *)
(* Error: Signature mismatch: *)
(*        ... *)
(*        Type declarations do not match: *)
(*          type statistics = Hashtbl.statistics *)
(*        is not included in *)
(*          type statistics = { *)
(*            num_bindings : int; *)
(*            num_buckets : int; *)
(*            max_bucket_length : int; *)
(*            bucket_histogram : int array; *)
(*          } *)
(*        Their kinds differ. *)
(* This is workaround:*)
module Indirection = struct
  type t = Hashtbl.statistics = {  num_bindings: int;
                                   num_buckets: int;
                                   max_bucket_length: int;
                                   bucket_histogram: int array}
end
module type HS = sig
  type statistics = Indirection.t
  include module type of Hashtbl
                         with type statistics := Indirection.t
end
module H : HS = MoreLabels.Hashtbl

let ()  =
  ()
