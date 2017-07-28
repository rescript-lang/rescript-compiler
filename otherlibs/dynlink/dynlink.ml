(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Dynamic loading of .cmo files *)

open Dynlinkaux  (* REMOVE_ME for ../../debugger/dynlink.ml *)
open Cmo_format

type linking_error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | File_not_found of string
  | Cannot_open_dll of string
  | Inconsistent_implementation of string

exception Error of error

let () =
  Printexc.register_printer
    (function
      | Error err ->
          let msg = match err with
          | Not_a_bytecode_file s ->
              Printf.sprintf "Not_a_bytecode_file %S" s
          | Inconsistent_import s ->
              Printf.sprintf "Inconsistent_import %S" s
          | Unavailable_unit s ->
              Printf.sprintf "Unavailable_unit %S" s
          | Unsafe_file ->
              "Unsafe_file"
          | Linking_error (s, Undefined_global s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Undefined_global %S)"
                             s s'
          | Linking_error (s, Unavailable_primitive s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Unavailable_primitive \
                              %S)" s s'
          | Linking_error (s, Uninitialized_global s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Uninitialized_global \
                              %S)" s s'
          | Corrupted_interface s ->
              Printf.sprintf "Corrupted_interface %S" s
          | File_not_found s ->
              Printf.sprintf "File_not_found %S" s
          | Cannot_open_dll s ->
              Printf.sprintf "Cannot_open_dll %S" s
          | Inconsistent_implementation s ->
              Printf.sprintf "Inconsistent_implementation %S" s in
          Some (Printf.sprintf "Dynlink.Error(Dynlink.%s)" msg)
      | _ -> None)

(* Management of interface CRCs *)

let crc_interfaces = ref (Consistbl.create ())
let allow_extension = ref true

(* Check that the object file being loaded has been compiled against
   the same interfaces as the program itself. In addition, check that
   only authorized compilation units are referenced. *)

let check_consistency file_name cu =
  try
    List.iter
      (fun (name, crco) ->
         match crco with
           None -> ()
         | Some crc ->
             if name = cu.cu_name then
               Consistbl.set !crc_interfaces name crc file_name
             else if !allow_extension then
               Consistbl.check !crc_interfaces name crc file_name
             else
               Consistbl.check_noadd !crc_interfaces name crc file_name)
      cu.cu_imports
  with Consistbl.Inconsistency(name, user, auth) ->
         raise(Error(Inconsistent_import name))
     | Consistbl.Not_available(name) ->
         raise(Error(Unavailable_unit name))

(* Empty the crc_interfaces table *)

let clear_available_units () =
  Consistbl.clear !crc_interfaces;
  allow_extension := false

(* Allow only access to the units with the given names *)

let allow_only names =
  Consistbl.filter (fun name -> List.mem name names) !crc_interfaces;
  allow_extension := false

(* Prohibit access to the units with the given names *)

let prohibit names =
  Consistbl.filter (fun name -> not (List.mem name names)) !crc_interfaces;
  allow_extension := false

(* Initialize the crc_interfaces table with a list of units with fixed CRCs *)

let add_available_units units =
  List.iter
    (fun (unit, crc) -> Consistbl.set !crc_interfaces unit crc "")
    units

(* Default interface CRCs: those found in the current executable *)
let default_crcs = ref []

let default_available_units () =
  clear_available_units();
  List.iter
    (fun (unit, crco) ->
       match crco with
         None -> ()
       | Some crc -> Consistbl.set !crc_interfaces unit crc "")
    !default_crcs;
  allow_extension := true

(* Initialize the linker tables and everything *)

let inited = ref false

let init () =
  if not !inited then begin
    default_crcs := Symtable.init_toplevel();
    default_available_units ();
    inited := true;
  end

let clear_available_units () = init(); clear_available_units ()
let allow_only l = init(); allow_only l
let prohibit l = init(); prohibit l
let add_available_units l = init(); add_available_units l
let default_available_units () = init(); default_available_units ()

(* Read the CRC of an interface from its .cmi file *)

let digest_interface unit loadpath =
  let filename =
    let shortname = unit ^ ".cmi" in
    try
      Misc.find_in_path_uncap loadpath shortname
    with Not_found ->
      raise (Error(File_not_found shortname)) in
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      raise(Error(Corrupted_interface filename))
    end;
    let cmi = Cmi_format.input_cmi ic in
    close_in ic;
    let crc =
      match cmi.Cmi_format.cmi_crcs with
        (_, Some crc) :: _ -> crc
      | _             -> raise(Error(Corrupted_interface filename))
    in
    crc
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_interface filename))

(* Initialize the crc_interfaces table with a list of units.
   Their CRCs are read from their interfaces. *)

let add_interfaces units loadpath =
  add_available_units
    (List.map (fun unit -> (unit, digest_interface unit loadpath)) units)

(* Check whether the object file being loaded was compiled in unsafe mode *)

let unsafe_allowed = ref false

let allow_unsafe_modules b =
  unsafe_allowed := b

let check_unsafe_module cu =
  if (not !unsafe_allowed) && cu.cu_primitives <> []
  then raise(Error(Unsafe_file))

(* Load in-core and execute a bytecode object file *)

external register_code_fragment: bytes -> int -> string -> unit
                               = "caml_register_code_fragment"

let load_compunit ic file_name file_digest compunit =
  check_consistency file_name compunit;
  check_unsafe_module compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  Bytes.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  Bytes.unsafe_set code (compunit.cu_codesize + 1) '\000';
  Bytes.unsafe_set code (compunit.cu_codesize + 2) '\000';
  Bytes.unsafe_set code (compunit.cu_codesize + 3) '\000';
  Bytes.unsafe_set code (compunit.cu_codesize + 4) '\001';
  Bytes.unsafe_set code (compunit.cu_codesize + 5) '\000';
  Bytes.unsafe_set code (compunit.cu_codesize + 6) '\000';
  Bytes.unsafe_set code (compunit.cu_codesize + 7) '\000';
  let initial_symtable = Symtable.current_state() in
  begin try
    Symtable.patch_object code compunit.cu_reloc;
    Symtable.check_global_initialized compunit.cu_reloc;
    Symtable.update_global_table()
  with Symtable.Error error ->
    let new_error =
      match error with
        Symtable.Undefined_global s -> Undefined_global s
      | Symtable.Unavailable_primitive s -> Unavailable_primitive s
      | Symtable.Uninitialized_global s -> Uninitialized_global s
      | _ -> assert false in
    raise(Error(Linking_error (file_name, new_error)))
  end;
  (* PR#5215: identify this code fragment by
     digest of file contents + unit name.
     Unit name is needed for .cma files, which produce several code fragments.*)
  let digest = Digest.string (file_digest ^ compunit.cu_name) in
  register_code_fragment code code_size digest;
  begin try
    ignore((Meta.reify_bytecode code code_size) ())
  with exn ->
    Symtable.restore_state initial_symtable;
    raise exn
  end

let loadfile file_name =
  init();
  if not (Sys.file_exists file_name)
    then raise (Error (File_not_found file_name));
  let ic = open_in_bin file_name in
  let file_digest = Digest.channel ic (-1) in
  seek_in ic 0;
  try
    let buffer =
      try really_input_string ic (String.length Config.cmo_magic_number)
      with End_of_file -> raise (Error (Not_a_bytecode_file file_name))
    in
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu = (input_value ic : compilation_unit) in
      load_compunit ic file_name file_digest cu
    end else
    if buffer = Config.cma_magic_number then begin
      let toc_pos = input_binary_int ic in  (* Go to table of contents *)
      seek_in ic toc_pos;
      let lib = (input_value ic : library) in
      begin try
        Dll.open_dlls Dll.For_execution
                      (List.map Dll.extract_dll_name lib.lib_dllibs)
      with Failure reason ->
        raise(Error(Cannot_open_dll reason))
      end;
      List.iter (load_compunit ic file_name file_digest) lib.lib_units
    end else
      raise(Error(Not_a_bytecode_file file_name));
    close_in ic
  with exc ->
    close_in ic; raise exc

let loadfile_private file_name =
  init();
  let initial_symtable = Symtable.current_state()
  and initial_crc = !crc_interfaces in
  try
    loadfile file_name;
    Symtable.hide_additions initial_symtable;
    crc_interfaces := initial_crc
  with exn ->
    Symtable.hide_additions initial_symtable;
    crc_interfaces := initial_crc;
    raise exn

(* Error report *)

let error_message = function
    Not_a_bytecode_file name ->
      name ^ " is not a bytecode object file"
  | Inconsistent_import name ->
      "interface mismatch on " ^ name
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
      "corrupted interface file " ^ name
  | File_not_found name ->
      "cannot find file " ^ name ^ " in search path"
  | Cannot_open_dll reason ->
      "error loading shared library: " ^ reason
  | Inconsistent_implementation name ->
      "implementation mismatch on " ^ name

let is_native = false
let adapt_filename f = f
