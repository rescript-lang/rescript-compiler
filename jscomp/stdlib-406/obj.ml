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
 
external new_block : int -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"






let object_tag = 248

let extension_constructor x =
  let x = repr x in
  let slot =
    if (is_block x) && (tag x) <> object_tag && (size x) >= 1 then field x 0
    else x
  in
  let name =
    if (is_block slot) && (tag slot) = object_tag then field slot 0
    else invalid_arg "Obj.extension_constructor"
  in
    if Js.typeof name = "string" then (obj slot : extension_constructor)
    else invalid_arg "Obj.extension_constructor"

let [@inline always] extension_name (slot : extension_constructor) =
  (obj (field (repr slot) 0) : string)

let [@inline always] extension_id (slot : extension_constructor) =
  (obj (field (repr slot) 1) : int)

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

let new_object_tag_block size = 
  new_block object_tag size  

