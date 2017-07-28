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

open My_std
open Pathname.Operators

let digests = Hashtbl.create 103

let get = Hashtbl.find digests

let put = Hashtbl.replace digests

let _digests = lazy (!Options.build_dir / (Pathname.mk "_digests"))

let finalize () =
  with_output_file !*_digests begin fun oc ->
    Hashtbl.iter begin fun name digest ->
      Printf.fprintf oc "%S: %S\n" name digest
    end digests
  end

let init () =
  Shell.chdir !Options.build_dir;
  if Pathname.exists !*_digests then
    with_input_file !*_digests begin fun ic ->
      try while true do
        let l = input_line ic in
        Scanf.sscanf l "%S: %S" put
      done with End_of_file -> ()
    end;
  My_unix.at_exit_once finalize
