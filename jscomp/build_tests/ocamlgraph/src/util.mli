(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Some useful operations. *)

open Sig

(** Cartesian product of two ordered types. *)
module OTProduct(X: ORDERED_TYPE)(Y: ORDERED_TYPE) :
  ORDERED_TYPE with type t = X.t * Y.t

(** Cartesian product of two hashable types. *)
module HTProduct(X: HASHABLE)(Y: HASHABLE) :
  HASHABLE with type t = X.t * Y.t

(** Cartesian product of two comparable types. *)
module CMPProduct(X: COMPARABLE)(Y: COMPARABLE) :
  COMPARABLE with type t = X.t * Y.t

(** Create a vertex type with some data attached to it *)
module DataV(L : sig type t end)(V : Sig.COMPARABLE) : sig
  type data = L.t
  and label = V.t
  and t = data ref * V.t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val create : data -> V.t -> t
  val label : t -> V.t
  val data : t -> data
  val set_data : t -> data -> unit
end

