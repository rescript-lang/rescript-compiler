(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
(* Tools *)

open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Rule

let pp_l = List.print String.print

let tags_of_pathname p =
  Configuration.tags_of_filename (Pathname.to_string p)
  ++("file:"^p)
  ++("extension:"^Pathname.get_extension p)

let opt_print elt ppf =
  function
  | Some x -> fprintf ppf "@[<2>Some@ %a@]" elt x
  | None -> pp_print_string ppf "None"

let path_and_context_of_string s =
  if Pathname.is_implicit s then
    let b = Pathname.basename s in
    let d = Pathname.dirname s in
    if d <> Pathname.current_dir_name then
      let () = Pathname.define_context d [d] in
      [s]
    else
      let include_dirs = Pathname.include_dirs_of d in
      List.map (fun include_dir -> include_dir/b) include_dirs
  else [s]
