(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

(* Convert environment summaries to environments *)

val env_from_summary : Env.summary -> Subst.t -> Env.t

(* Empty the environment caches. To be called when load_path changes. *)

val reset_cache: unit -> unit

val env_of_only_summary : Env.t -> Env.t

(* Error report *)

type error =
    Module_not_found of Path.t

exception Error of error

val report_error: formatter -> error -> unit
