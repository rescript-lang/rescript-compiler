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

external tag : t -> int = "caml_obj_tag"
external size : t -> int = "%obj_size"

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
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"


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


