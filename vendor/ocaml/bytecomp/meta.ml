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

external global_data : unit -> Obj.t array = "caml_get_global_data"
external realloc_global_data : int -> unit = "caml_realloc_global"
external static_alloc : int -> bytes = "caml_static_alloc"
external static_free : bytes -> unit = "caml_static_free"
external static_resize : bytes -> int -> bytes = "caml_static_resize"
external static_release_bytecode : bytes -> int -> unit
                                 = "caml_static_release_bytecode"
type closure = unit -> Obj.t
external reify_bytecode : bytes -> int -> closure = "caml_reify_bytecode"
external invoke_traced_function : Obj.t -> Obj.t -> Obj.t -> Obj.t
                                = "caml_invoke_traced_function"
external get_section_table : unit -> (string * Obj.t) list
                           = "caml_get_section_table"
external add_debug_info :
  bytes -> int -> Instruct.debug_event list array -> unit
                        = "caml_add_debug_info"
external remove_debug_info : bytes -> unit
                           = "caml_remove_debug_info"
