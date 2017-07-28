(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** A module with a function to parse strings to obtain a [Odoc_types.text] value. *)

(** Syntax error in a text. *)
exception Text_syntax of int * int * string (* line, char, string *)

(** Transformation of strings to text structures. *)
module Texter :
    sig
      val text_of_string : string -> Odoc_types.text
      val string_of_text : Odoc_types.text -> string
    end
