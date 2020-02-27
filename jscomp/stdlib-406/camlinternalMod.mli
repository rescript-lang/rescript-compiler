(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Xavier Leroy, projet Cristal, INRIA Rocquencourt              *)
(*                                                                        *)
(*   Copyright 2004 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Run-time support for recursive modules.
    All functions in this module are for system use only, not for the
    casual user. *)

type shape =
  | Function (* [@@dead "shape.Function"] *)
  | Lazy (* [@@dead "shape.Lazy"] *)
  | Class (* [@@dead "shape.Class"] *)
  | Module of shape array (* [@@dead "shape.Module"] *)
  | Value of Obj.t (* [@@dead "shape.Value"] *)
#if BS then 
#else
val init_mod: string * int * int -> shape -> Obj.t
val update_mod: shape -> Obj.t -> Obj.t -> unit
#end