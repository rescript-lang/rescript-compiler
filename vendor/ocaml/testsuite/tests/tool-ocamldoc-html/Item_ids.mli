(** Check that all toplevel items are given a unique id. *)

exception Ex
type t
val x: t
type ext = ..
type ext += A
class c: object end
class type ct= object end
[@@@attribute]
module M: sig end
module type s = sig end

