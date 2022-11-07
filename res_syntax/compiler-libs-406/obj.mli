(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on internal representations of values.

   Not for the casual user.
*)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
val [@inline always] is_block : t -> bool
external is_int : t -> bool = "%obj_is_int"
external tag : t -> int = "caml_obj_tag"
external size : t -> int = "%obj_size"
external reachable_words : t -> int = "caml_obj_reachable_words"
  (**
     Computes the total size (in words, including the headers) of all
     heap blocks accessible from the argument.  Statically
     allocated blocks are excluded.

     @Since 4.04
  *)

external field : t -> int -> t = "%obj_field"

(** When using flambda:

    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)

    The same goes for [set_double_field] and [set_tag].  However, for
    [set_tag], in the case of immutable blocks where the middle-end optimizers
    never see code that discriminates on their tag (for example records), the
    operation should be safe.  Such uses are nonetheless discouraged.

    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    {!Sys.opaque_identity}, so any information about its contents will not
    be propagated.
*)
external set_field : t -> int -> t -> unit = "%obj_set_field"
external set_tag : t -> int -> unit = "caml_obj_set_tag"

val [@inline always] double_field : t -> int -> float  (* @since 3.11.2 *)
val [@inline always] set_double_field : t -> int -> float -> unit
  (* @since 3.11.2 *)
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
external add_offset : t -> Int32.t -> t = "caml_obj_add_offset"
         (* @since 3.12.0 *)

val first_non_constant_constructor_tag : int
val last_non_constant_constructor_tag : int

val lazy_tag : int
val closure_tag : int
val object_tag : int
val infix_tag : int
val forward_tag : int
val no_scan_tag : int
val abstract_tag : int
val string_tag : int   (* both [string] and [bytes] *)
val double_tag : int
val double_array_tag : int
val custom_tag : int
val final_tag : int
  [@@ocaml.deprecated "Replaced by custom_tag."]

val int_tag : int
val out_of_heap_tag : int
val unaligned_tag : int   (* should never happen @since 3.11.0 *)

val extension_constructor : 'a -> extension_constructor
val [@inline always] extension_name : extension_constructor -> string
val [@inline always] extension_id : extension_constructor -> int

(** The following two functions are deprecated.  Use module {!Marshal}
    instead. *)

val marshal : t -> bytes
  [@@ocaml.deprecated "Use Marshal.to_bytes instead."]
val unmarshal : bytes -> int -> t * int
  [@@ocaml.deprecated "Use Marshal.from_bytes and Marshal.total_size instead."]

module Ephemeron: sig
  (** Ephemeron with arbitrary arity and untyped *)

  type obj_t = t
  (** alias for {!Obj.t} *)

  type t
  (** an ephemeron cf {!Ephemeron} *)

  val create: int -> t
  (** [create n] returns an ephemeron with [n] keys.
      All the keys and the data are initially empty *)

  val length: t -> int
  (** return the number of keys *)

  val get_key: t -> int -> obj_t option
  (** Same as {!Ephemeron.K1.get_key} *)

  val get_key_copy: t -> int -> obj_t option
  (** Same as {!Ephemeron.K1.get_key_copy} *)

  val set_key: t -> int -> obj_t -> unit
  (** Same as {!Ephemeron.K1.set_key} *)

  val unset_key: t -> int -> unit
  (** Same as {!Ephemeron.K1.unset_key} *)

  val check_key: t -> int -> bool
  (** Same as {!Ephemeron.K1.check_key} *)

  val blit_key : t -> int -> t -> int -> int -> unit
  (** Same as {!Ephemeron.K1.blit_key} *)

  val get_data: t -> obj_t option
  (** Same as {!Ephemeron.K1.get_data} *)

  val get_data_copy: t -> obj_t option
  (** Same as {!Ephemeron.K1.get_data_copy} *)

  val set_data: t -> obj_t -> unit
  (** Same as {!Ephemeron.K1.set_data} *)

  val unset_data: t -> unit
  (** Same as {!Ephemeron.K1.unset_data} *)

  val check_data: t -> bool
  (** Same as {!Ephemeron.K1.check_data} *)

  val blit_data : t -> t -> unit
  (** Same as {!Ephemeron.K1.blit_data} *)
end
