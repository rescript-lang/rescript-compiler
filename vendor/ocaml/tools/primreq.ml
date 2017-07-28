(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Determine the set of C primitives required by the given .cmo and .cma
   files *)

open Config
open Cmo_format

module StringSet = Set.Make(struct type t = string let compare = compare end)

let defined = ref true
let used = ref false
let exclude_file = ref ""

let primitives = ref StringSet.empty

let scan_reloc = function
    (Reloc_primitive s, _) -> primitives := StringSet.add s !primitives
  | _ -> ()

let scan_prim s =
  primitives := StringSet.add s !primitives

let scan_info cu =
  if !used then List.iter scan_reloc cu.cu_reloc;
  if !defined then List.iter scan_prim cu.cu_primitives

let scan_obj filename =
  let ic = open_in_bin filename in
  let buffer = String.create (String.length cmo_magic_number) in
  really_input ic buffer 0 (String.length cmo_magic_number);
  if buffer = cmo_magic_number then begin
    let cu_pos = input_binary_int ic in
    seek_in ic cu_pos;
    let cu = (input_value ic : compilation_unit) in
    close_in ic;
    scan_info cu
  end else
  if buffer = cma_magic_number then begin
    let toc_pos = input_binary_int ic in
    seek_in ic toc_pos;
    let toc = (input_value ic : library) in
    close_in ic;
    List.iter scan_info toc.lib_units
  end else begin
    prerr_endline "Not an object file"; exit 2
  end

let exclude filename =
  let ic = open_in filename in
  try
    while true do
      let s = input_line ic in
      primitives := StringSet.remove s !primitives
    done
  with End_of_file -> close_in ic
     | x -> close_in ic; raise x

let main() =
  Arg.parse
    ["-used", Arg.Unit(fun () -> used := true; defined := false),
        "show primitives referenced in the object files";
     "-defined", Arg.Unit(fun () -> defined := true; used := false),
        "show primitives defined in the object files (default)";
     "-all", Arg.Unit(fun () -> defined := true; used := true),
        "show primitives defined or referenced in the object files";
     "-exclude", Arg.String(fun s -> exclude_file := s),
        "<file> don't print the primitives mentioned in <file>"]
    scan_obj
    "Usage: primreq [options] <.cmo and .cma files>\nOptions are:";
  if String.length !exclude_file > 0 then exclude !exclude_file;
  StringSet.iter
    (fun s ->
      if s.[0] <> '%' then begin print_string s; print_newline() end)
    !primitives;
  exit 0

let _ = main ()
