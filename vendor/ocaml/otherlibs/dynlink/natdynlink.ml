(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* Dynamic loading of .cmx files *)

type handle

external ndl_open: string -> bool -> handle * bytes = "caml_natdynlink_open"
external ndl_run: handle -> string -> unit = "caml_natdynlink_run"
external ndl_getmap: unit -> bytes = "caml_natdynlink_getmap"
external ndl_globals_inited: unit -> int = "caml_natdynlink_globals_inited"

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

open Cmx_format

(* Copied from config.ml to avoid dependencies *)
let cmxs_magic_number = "Caml2007D002"

let dll_filename fname =
  if Filename.is_implicit fname then Filename.concat (Sys.getcwd ()) fname
  else fname

let read_file filename priv =
  let dll = dll_filename filename in
  if not (Sys.file_exists dll) then raise (Error (File_not_found dll));

  let (handle,data) as res = ndl_open dll (not priv) in
  if Obj.tag (Obj.repr res) = Obj.string_tag
  then raise (Error (Cannot_open_dll (Obj.magic res)));

  let header : dynheader = Marshal.from_bytes data 0 in
  if header.dynu_magic <> cmxs_magic_number
  then raise(Error(Not_a_bytecode_file dll));
  (dll, handle, header.dynu_units)



(* Management of interface and implementation CRCs *)

module StrMap = Map.Make(String)

type implem_state =
  | Loaded
  | Check_inited of int

type state = {
  ifaces: (string*string) StrMap.t;
  implems: (string*string*implem_state) StrMap.t;
}

let empty_state = {
  ifaces = StrMap.empty;
  implems = StrMap.empty;
}

let global_state = ref empty_state

let allow_extension = ref true

let inited = ref false

let default_available_units () =
  let map : (string*Digest.t*Digest.t*string list) list =
    Marshal.from_bytes (ndl_getmap ()) 0 in
  let exe = Sys.executable_name in
  let rank = ref 0 in
  global_state :=
    List.fold_left
      (fun st (name,crc_intf,crc_impl,syms) ->
        rank := !rank + List.length syms;
        {
         ifaces = StrMap.add name (crc_intf,exe) st.ifaces;
         implems = StrMap.add name (crc_impl,exe,Check_inited !rank) st.implems;
        }
      )
      empty_state
      map;
  allow_extension := true;
  inited := true

let init () =
  if not !inited then default_available_units ()

let add_check_ifaces allow_ext filename ui ifaces =
  List.fold_left
    (fun ifaces (name, crco) ->
       match crco with
         None -> ifaces
       | Some crc ->
           if name = ui.dynu_name
           then StrMap.add name (crc,filename) ifaces
           else
             try
               let (old_crc,old_src) = StrMap.find name ifaces in
                 if old_crc <> crc
                 then raise(Error(Inconsistent_import(name)))
                 else ifaces
             with Not_found ->
               if allow_ext then StrMap.add name (crc,filename) ifaces
               else raise (Error(Unavailable_unit name))
    ) ifaces ui.dynu_imports_cmi

let check_implems filename ui implems =
  List.iter
    (fun (name, crco) ->
       match name with
         |"Out_of_memory"
         |"Sys_error"
         |"Failure"
         |"Invalid_argument"
         |"End_of_file"
         |"Division_by_zero"
         |"Not_found"
         |"Match_failure"
         |"Stack_overflow"
         |"Sys_blocked_io"
         |"Assert_failure"
         |"Undefined_recursive_module" -> ()
         | _ ->
       try
         let (old_crc,old_src,state) = StrMap.find name implems in
           match crco with
             Some crc when old_crc <> crc ->
               raise(Error(Inconsistent_implementation(name)))
           | _ ->
               match state with
               | Check_inited i ->
                   if ndl_globals_inited() < i
                   then raise(Error(Unavailable_unit name))
               | Loaded -> ()
       with Not_found ->
         raise (Error(Unavailable_unit name))
    ) ui.dynu_imports_cmx

let loadunits filename handle units state =
  let new_ifaces =
    List.fold_left
      (fun accu ui -> add_check_ifaces !allow_extension filename ui accu)
      state.ifaces units in
  let new_implems =
    List.fold_left
      (fun accu ui ->
         check_implems filename ui accu;
         StrMap.add ui.dynu_name (ui.dynu_crc,filename,Loaded) accu)
      state.implems units in

  let defines = List.flatten (List.map (fun ui -> ui.dynu_defines) units) in

  ndl_run handle "_shared_startup";
  List.iter (ndl_run handle) defines;
  { implems = new_implems; ifaces = new_ifaces }

let load priv filename =
  init();
  let (filename,handle,units) = read_file filename priv in
  let nstate = loadunits filename handle units !global_state in
  if not priv then global_state := nstate

let loadfile filename = load false filename
let loadfile_private filename = load true filename

let allow_only names =
  init();
  let old = !global_state.ifaces in
  let ifaces =
    List.fold_left
      (fun ifaces name ->
         try StrMap.add name (StrMap.find name old) ifaces
         with Not_found -> ifaces)
      StrMap.empty names in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let prohibit names =
  init();
  let ifaces = List.fold_right StrMap.remove names !global_state.ifaces in
  global_state := { !global_state with ifaces = ifaces };
  allow_extension := false

let digest_interface _ _ =
  failwith "Dynlink.digest_interface: not implemented in native code"
let add_interfaces _ _ =
  failwith "Dynlink.add_interfaces: not implemented in native code"
let add_available_units _ =
  failwith "Dynlink.add_available_units: not implemented in native code"
let clear_available_units _ =
  failwith "Dynlink.clear_available_units: not implemented in native code"
let allow_unsafe_modules _ =
  ()

(* Error report *)

let error_message = function
    Not_a_bytecode_file name ->
      name ^ " is not an object file"
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

let is_native = true
let adapt_filename f = Filename.chop_extension f ^ ".cmxs"
