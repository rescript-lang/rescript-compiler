(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jeremie Dimino, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for attributes *)

open Asttypes
open Parsetree

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

(** The [string list] argument of the following functions is a list of
    alternative names for the attribute we are looking for. For instance:

    {[
      ["foo"; "ocaml.foo"]
    ]} *)
val get_no_payload_attribute : string list -> attributes -> string loc option
val has_no_payload_attribute : string list -> attributes -> bool

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit
