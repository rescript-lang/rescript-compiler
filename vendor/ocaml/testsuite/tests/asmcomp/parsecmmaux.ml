(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliary functions for parsing *)

type error =
    Unbound of string

exception Error of error

let tbl_ident = (Hashtbl.create 57 : (string, Ident.t) Hashtbl.t)

let bind_ident s =
  let id = Ident.create s in
  Hashtbl.add tbl_ident s id;
  id

let find_ident s =
  try
    Hashtbl.find tbl_ident s
  with Not_found ->
    raise(Error(Unbound s))

let unbind_ident id =
  Hashtbl.remove tbl_ident (Ident.name id)

let report_error = function
    Unbound s ->
      prerr_string "Unbound identifier "; prerr_string s; prerr_endline "."
