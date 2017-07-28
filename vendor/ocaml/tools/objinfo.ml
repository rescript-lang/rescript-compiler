(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*        Mehdi Dogguy, PPS laboratory, University Paris Diderot       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.   Modifications Copyright 2010 Mehdi Dogguy,       *)
(*  used and distributed as part of OCaml by permission from           *)
(*  the author.   This file is distributed under the terms of the      *)
(*  Q Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

(* Dump info on .cmi, .cmo, .cmx, .cma, .cmxa, .cmxs files
   and on bytecode executables. *)

open Printf
open Misc
open Config
open Cmo_format

let input_stringlist ic len =
  let get_string_list sect len =
    let rec fold s e acc =
      if e != len then
        if sect.[e] = '\000' then
          fold (e+1) (e+1) (String.sub sect s (e-s) :: acc)
        else fold s (e+1) acc
      else acc
    in fold 0 0 []
  in
  let sect = really_input_string ic len in
  get_string_list sect len

let dummy_crc = String.make 32 '-'

let print_name_crc (name, crco) =
  let crc =
    match crco with
      None -> dummy_crc
    | Some crc -> Digest.to_hex crc
  in
    printf "\t%s\t%s\n" crc name

let print_line name =
  printf "\t%s\n" name

let print_cmo_infos cu =
  printf "Unit name: %s\n" cu.cu_name;
  print_string "Interfaces imported:\n";
  List.iter print_name_crc cu.cu_imports;
  printf "Uses unsafe features: ";
  (match cu.cu_primitives with
    | [] -> printf "no\n"
    | l  ->
        printf "YES\n";
        printf "Primitives declared in this module:\n";
        List.iter print_line l);
  printf "Force link: %s\n" (if cu.cu_force_link then "YES" else "no")

let print_spaced_string s =
  printf " %s" s

let print_cma_infos (lib : Cmo_format.library) =
  printf "Force custom: %s\n" (if lib.lib_custom then "YES" else "no");
  printf "Extra C object files:";
  (* PR#4949: print in linking order *)
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string lib.lib_ccopts;
  printf "\n";
  print_string "Extra dynamically-loaded libraries:";
  List.iter print_spaced_string lib.lib_dllibs;
  printf "\n";
  List.iter print_cmo_infos lib.lib_units

let print_cmi_infos name crcs =
  printf "Unit name: %s\n" name;
  printf "Interfaces imported:\n";
  List.iter print_name_crc crcs

let print_cmt_infos cmt =
  let open Cmt_format in
  printf "Cmt unit name: %s\n" cmt.cmt_modname;
  print_string "Cmt interfaces imported:\n";
  List.iter print_name_crc cmt.cmt_imports;
  printf "Source file: %s\n"
         (match cmt.cmt_sourcefile with None -> "(none)" | Some f -> f);
  printf "Compilation flags:";
  Array.iter print_spaced_string cmt.cmt_args;
  printf "\nLoad path:";
  List.iter print_spaced_string cmt.cmt_loadpath;
  printf "\n";
  printf "cmt interface digest: %s\n"
    (match cmt.cmt_interface_digest with
     | None -> ""
     | Some crc -> Digest.to_hex crc)

let print_general_infos name crc defines cmi cmx =
  printf "Name: %s\n" name;
  printf "CRC of implementation: %s\n" (Digest.to_hex crc);
  printf "Globals defined:\n";
  List.iter print_line defines;
  printf "Interfaces imported:\n";
  List.iter print_name_crc cmi;
  printf "Implementations imported:\n";
  List.iter print_name_crc cmx

open Cmx_format

let print_cmx_infos (ui, crc) =
  print_general_infos
    ui.ui_name crc ui.ui_defines ui.ui_imports_cmi ui.ui_imports_cmx;
  printf "Approximation:\n";
  Format.fprintf Format.std_formatter "  %a@." Printclambda.approx ui.ui_approx;
  let pr_funs _ fns =
    List.iter (fun arity -> printf " %d" arity) fns in
  printf "Currying functions:%a\n" pr_funs ui.ui_curry_fun;
  printf "Apply functions:%a\n" pr_funs ui.ui_apply_fun;
  printf "Send functions:%a\n" pr_funs ui.ui_send_fun;
  printf "Force link: %s\n" (if ui.ui_force_link then "YES" else "no")

let print_cmxa_infos (lib : Cmx_format.library_infos) =
  printf "Extra C object files:";
  List.iter print_spaced_string (List.rev lib.lib_ccobjs);
  printf "\nExtra C options:";
  List.iter print_spaced_string lib.lib_ccopts;
  printf "\n";
  List.iter print_cmx_infos lib.lib_units

let print_cmxs_infos header =
  List.iter
    (fun ui ->
       print_general_infos
         ui.dynu_name
         ui.dynu_crc
         ui.dynu_defines
         ui.dynu_imports_cmi
         ui.dynu_imports_cmx)
    header.dynu_units

let p_title title = printf "%s:\n" title

let p_section title = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print_name_crc l

let p_list title print = function
  | [] -> ()
  | l ->
      p_title title;
      List.iter print l

let dump_byte ic =
  Bytesections.read_toc ic;
  let toc = Bytesections.toc () in
  let toc = List.sort Pervasives.compare toc in
  List.iter
    (fun (section, _) ->
       try
         let len = Bytesections.seek_section ic section in
         if len > 0 then match section with
           | "CRCS" ->
               p_section
                 "Imported units"
                 (input_value ic : (string * Digest.t option) list)
           | "DLLS" ->
               p_list
                 "Used DLLs"
                 print_line
                 (input_stringlist ic len)
           | "DLPT" ->
               p_list
                 "Additional DLL paths"
                 print_line
                 (input_stringlist ic len)
           | "PRIM" ->
               p_list
                 "Primitives used"
                 print_line
                 (input_stringlist ic len)
           | _ -> ()
       with _ -> ()
    )
    toc

let read_dyn_header filename ic =
  let tempfile = Filename.temp_file "objinfo" ".out" in
  let helper = Filename.concat Config.standard_library "objinfo_helper" in
  try
    try_finally
      (fun () ->
        let rc = Sys.command (sprintf "%s %s > %s"
                                (Filename.quote helper)
                                (Filename.quote filename)
                                tempfile) in
        if rc <> 0 then failwith "cannot read";
        let tc = open_in tempfile in
        try_finally
          (fun () ->
            let ofs = Scanf.fscanf tc "%Ld" (fun x -> x) in
            LargeFile.seek_in ic ofs;
            Some(input_value ic : dynheader))
          (fun () -> close_in tc))
      (fun () -> remove_file tempfile)
  with Failure _ | Sys_error _ -> None

let dump_obj filename =
  printf "File %s\n" filename;
  let ic = open_in_bin filename in
  let len_magic_number = String.length cmo_magic_number in
  let magic_number = really_input_string ic len_magic_number in
  if magic_number = cmo_magic_number then begin
    let cu_pos = input_binary_int ic in
    seek_in ic cu_pos;
    let cu = (input_value ic : compilation_unit) in
    close_in ic;
    print_cmo_infos cu
  end else if magic_number = cma_magic_number then begin
    let toc_pos = input_binary_int ic in
    seek_in ic toc_pos;
    let toc = (input_value ic : library) in
    close_in ic;
    print_cma_infos toc
  end else if magic_number = cmi_magic_number ||
              magic_number = cmt_magic_number then begin
    close_in ic;
    let cmi, cmt = Cmt_format.read filename in
    begin match cmi with
     | None -> ()
     | Some cmi ->
         print_cmi_infos cmi.Cmi_format.cmi_name cmi.Cmi_format.cmi_crcs
    end;
    begin match cmt with
     | None -> ()
     | Some cmt -> print_cmt_infos cmt
    end
  end else if magic_number = cmx_magic_number then begin
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    print_cmx_infos (ui, crc)
  end else if magic_number = cmxa_magic_number then begin
    let li = (input_value ic : library_infos) in
    close_in ic;
    print_cmxa_infos li
  end else begin
    let pos_trailer = in_channel_length ic - len_magic_number in
    let _ = seek_in ic pos_trailer in
    let magic_number = really_input_string ic len_magic_number in
    if magic_number = Config.exec_magic_number then begin
      dump_byte ic;
      close_in ic
    end else if Filename.check_suffix filename ".cmxs" then begin
      flush stdout;
      match read_dyn_header filename ic with
      | None ->
          printf "Unable to read info on file %s\n" filename;
          exit 2
      | Some header ->
          if header.dynu_magic = Config.cmxs_magic_number then
            print_cmxs_infos header
          else begin
            printf "Wrong magic number\n"; exit 2
          end;
          close_in ic
    end else begin
      printf "Not an OCaml object file\n"; exit 2
    end
  end

let arg_list = []
let arg_usage =
   Printf.sprintf "%s [OPTIONS] FILES : give information on files" Sys.argv.(0)

let main() =
  Arg.parse arg_list dump_obj arg_usage;
  exit 0

let _ = main ()
