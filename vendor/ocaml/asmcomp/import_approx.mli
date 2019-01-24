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

(** Create simple value approximations from the export information in
    .cmx files. *)

(** Given an approximation description, load .cmx files (possibly more
    than one) until the description is fully resolved.  If a necessary .cmx
    file cannot be found, "unresolved" will be returned. *)
val really_import : Simple_value_approx.descr -> Simple_value_approx.descr

(** Maps the description of the given approximation through [really_import]. *)
val really_import_approx : Simple_value_approx.t -> Simple_value_approx.t

(** Read and convert the approximation of a given symbol from the
    relevant .cmx file.  Unlike the "really_" functions, this does not
    continue to load .cmx files until the approximation is fully
    resolved. *)
val import_symbol : Symbol.t -> Simple_value_approx.t
