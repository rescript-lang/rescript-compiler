type ident = Ident.t
(** Based on [hash_set] specialized for mask operations *)

type t

val create : int -> t

(* add one ident *)
val add_unmask : t -> ident -> unit

val mask_check_all_hit : ident -> t -> bool
(** [check_mask h key] if [key] exists mask it otherwise nothing return true if
    all keys are masked otherwise false *)

val iter_and_unmask : (ident -> bool -> unit) -> t -> unit
(** [iter_and_unmask f h] iterating the collection and mask all idents, dont
    consul the collection in function [f] TODO: what happens if an exception
    raised in the callback, would the hashtbl still be in consistent state? *)
