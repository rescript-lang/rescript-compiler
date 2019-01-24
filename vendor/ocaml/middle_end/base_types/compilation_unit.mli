(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

include Identifiable.S

(* The [Ident.t] must be persistent.  This function raises an exception
   if that is not the case. *)
val create : Ident.t -> Linkage_name.t -> t

val get_persistent_ident : t -> Ident.t
val get_linkage_name : t -> Linkage_name.t

val set_current : t -> unit
val get_current : unit -> t option
val get_current_exn : unit -> t
val get_current_id_exn : unit -> Ident.t

val string_for_printing : t -> string
