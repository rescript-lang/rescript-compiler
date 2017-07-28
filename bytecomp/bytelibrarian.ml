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

(* Build libraries of .cmo files *)

open Misc
open Config
open Cmo_format

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

(* Copy a compilation unit from a .cmo or .cma into the archive *)
let copy_compunit ic oc compunit =
  seek_in ic compunit.cu_pos;
  compunit.cu_pos <- pos_out oc;
  compunit.cu_force_link <- !Clflags.link_everything;
  copy_file_chunk ic oc compunit.cu_codesize;
  if compunit.cu_debug > 0 then begin
    seek_in ic compunit.cu_debug;
    compunit.cu_debug <- pos_out oc;
    copy_file_chunk ic oc compunit.cu_debugsize
  end

(* Add C objects and options and "custom" info from a library descriptor *)

let lib_ccobjs = ref []
let lib_ccopts = ref []
let lib_dllibs = ref []

(* See Bytelink.add_ccobjs for explanations on how options are ordered.
   Notice that here we scan .cma files given on the command line from
   left to right, hence options must be added after. *)

let add_ccobjs l =
  if not !Clflags.no_auto_link then begin
    if l.lib_custom then Clflags.custom_runtime := true;
    lib_ccobjs := !lib_ccobjs @ l.lib_ccobjs;
    lib_ccopts := !lib_ccopts @ l.lib_ccopts;
    lib_dllibs := !lib_dllibs @ l.lib_dllibs
  end

let copy_object_file ppf oc name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = really_input_string ic (String.length cmo_magic_number) in
    if buffer = cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      Bytelink.check_consistency ppf file_name compunit;
      copy_compunit ic oc compunit;
      close_in ic;
      [compunit]
    end else
    if buffer = cma_magic_number then begin
      let toc_pos = input_binary_int ic in
      seek_in ic toc_pos;
      let toc = (input_value ic : library) in
      List.iter (Bytelink.check_consistency ppf file_name) toc.lib_units;
      add_ccobjs toc;
      List.iter (copy_compunit ic oc) toc.lib_units;
      close_in ic;
      toc.lib_units
    end else
      raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

let create_archive ppf file_list lib_name =
  let outchan = open_out_bin lib_name in
  try
    output_string outchan cma_magic_number;
    let ofs_pos_toc = pos_out outchan in
    output_binary_int outchan 0;
    let units =
      List.flatten(List.map (copy_object_file ppf outchan) file_list) in
    let toc =
      { lib_units = units;
        lib_custom = !Clflags.custom_runtime;
        lib_ccobjs = !Clflags.ccobjs @ !lib_ccobjs;
        lib_ccopts = !Clflags.all_ccopts @ !lib_ccopts;
        lib_dllibs = !Clflags.dllibs @ !lib_dllibs } in
    let pos_toc = pos_out outchan in
    output_value outchan toc;
    seek_out outchan ofs_pos_toc;
    output_binary_int outchan pos_toc;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file lib_name;
    raise x

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a bytecode object file"
        Location.print_filename name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := []
