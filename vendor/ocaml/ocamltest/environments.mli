(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of environments, used to pass parameters to tests and actions *)

exception Variable_already_defined of Variables.t

type t

val empty : t

val from_bindings : (Variables.t * string) list -> t
val to_bindings : t -> (Variables.t * string) list

val lookup : Variables.t -> t -> string option
val safe_lookup : Variables.t -> t -> string
val is_variable_defined : Variables.t -> t -> bool

val add : Variables.t -> string -> t -> t
val add_bindings : (Variables.t * string) list -> t -> t

val dump : out_channel -> t -> unit

(* Environment modifiers *)

type modifier =
  | Include of string
  | Add of Variables.t * string
  | Replace of Variables.t * string
  | Append of Variables.t * string

type modifiers = modifier list

val apply_modifier : t -> modifier -> t
val apply_modifiers : t -> modifiers -> t

exception Empty_modifiers_name
exception Modifiers_name_already_registered of string
exception Modifiers_name_not_found of string

val register : modifiers -> string -> unit
