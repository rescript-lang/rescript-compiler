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

(* CR-soon mshinwell: This module should be removed. *)

(** Generic identifier type *)
module type BaseId =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Id =
sig
  include BaseId
  val create : ?name:string -> unit -> t
end

(** Fully qualified identifiers *)
module type UnitId =
sig
  module Compilation_unit : Identifiable.Thing
  include BaseId
  val create : ?name:string -> Compilation_unit.t -> t
  val unit : t -> Compilation_unit.t
end

(** If applied generatively, i.e. [Id(struct end)], creates a new type
    of identifiers. *)
module Id : functor (E : sig end) -> Id

module UnitId :
  functor (Id : Id) ->
  functor (Compilation_unit : Identifiable.Thing) ->
    UnitId with module Compilation_unit := Compilation_unit
