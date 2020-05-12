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

(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_int : t -> bool = "%obj_is_int"
let [@inline always] is_block a = not (is_int a)
external tag : t -> int = "caml_obj_tag"
(* external set_tag : t -> int -> unit = "caml_obj_set_tag" *)
external size : t -> int = "%obj_size"

external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
 
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"






let lazy_tag = 246
let object_tag = 248
let forward_tag = 250


module Ephemeron = struct
  type obj_t = t

  type t (** ephemeron *)

  external create: int -> t = "caml_ephe_create"

  let length x = size(repr x) - 2 (*-FIXME*)

  external get_key: t -> int -> obj_t option = "caml_ephe_get_key"
  external get_key_copy: t -> int -> obj_t option = "caml_ephe_get_key_copy"
  external set_key: t -> int -> obj_t -> unit = "caml_ephe_set_key"
  external unset_key: t -> int -> unit = "caml_ephe_unset_key"
  external check_key: t -> int -> bool = "caml_ephe_check_key"
  external blit_key : t -> int -> t -> int -> int -> unit
    = "caml_ephe_blit_key"

  external get_data: t -> obj_t option = "caml_ephe_get_data"
  external get_data_copy: t -> obj_t option = "caml_ephe_get_data_copy"
  external set_data: t -> obj_t -> unit = "caml_ephe_set_data"
  external unset_data: t -> unit = "caml_ephe_unset_data"
  external check_data: t -> bool = "caml_ephe_check_data"
  external blit_data : t -> t -> unit = "caml_ephe_blit_data"


end

