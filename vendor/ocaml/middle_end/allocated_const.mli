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

(** Constants that are always allocated (possibly statically).  Blocks
    are not included here since they are always encoded using
    [Prim (Pmakeblock, ...)]. *)

type t =
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  (* CR-someday mshinwell: consider using "float array" *)
  | Float_array of float list
  | Immutable_float_array of float list
  | String of string
  | Immutable_string of string

val compare : t -> t -> int

val print : Format.formatter -> t -> unit
