(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Types
open Parser_aux
open Format

val expression :
    Instruct.debug_event option -> Env.t -> expression ->
    Debugcom.Remote_value.t * type_expr

type error =
  | Unbound_identifier of Ident.t
  | Not_initialized_yet of Path.t
  | Unbound_long_identifier of Longident.t
  | Unknown_name of int
  | Tuple_index of type_expr * int * int
  | Array_index of int * int
  | List_index of int * int
  | String_index of string * int * int
  | Wrong_item_type of type_expr * int
  | Wrong_label of type_expr * string
  | Not_a_record of type_expr
  | No_result

exception Error of error

val report_error: formatter -> error -> unit
