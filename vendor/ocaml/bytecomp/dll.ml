(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Handling of dynamically-linked libraries *)

type dll_handle
type dll_address
type dll_mode = For_checking | For_execution

external dll_open: dll_mode -> string -> dll_handle = "caml_dynlink_open_lib"
external dll_close: dll_handle -> unit = "caml_dynlink_close_lib"
external dll_sym: dll_handle -> string -> dll_address
                = "caml_dynlink_lookup_symbol"
         (* returned dll_address may be Val_unit *)
external add_primitive: dll_address -> int = "caml_dynlink_add_primitive"
external get_current_dlls: unit -> dll_handle array
                                           = "caml_dynlink_get_current_libs"

(* Current search path for DLLs *)
let search_path = ref ([] : string list)

(* DLLs currently opened *)
let opened_dlls = ref ([] : dll_handle list)

(* File names for those DLLs *)
let names_of_opened_dlls = ref ([] : string list)

(* Add the given directories to the search path for DLLs. *)
let add_path dirs =
  search_path := dirs @ !search_path

let remove_path dirs =
  search_path := List.filter (fun d -> not (List.mem d dirs)) !search_path

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)

let extract_dll_name file =
  if Filename.check_suffix file Config.ext_dll then
    Filename.chop_suffix file Config.ext_dll
  else if String.length file >= 2 && String.sub file 0 2 = "-l" then
    "dll" ^ String.sub file 2 (String.length file - 2)
  else
    file (* will cause error later *)

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)

let open_dll mode name =
  let name = name ^ Config.ext_dll in
  let fullname =
    try
      let fullname = Misc.find_in_path !search_path name in
      if Filename.is_implicit fullname then
        Filename.concat Filename.current_dir_name fullname
      else fullname
    with Not_found -> name in
  if not (List.mem fullname !names_of_opened_dlls) then begin
    try
      let dll = dll_open mode fullname in
      names_of_opened_dlls := fullname :: !names_of_opened_dlls;
      opened_dlls := dll :: !opened_dlls
    with Failure msg ->
      failwith (fullname ^ ": " ^ msg)
  end

let open_dlls mode names =
  List.iter (open_dll mode) names

(* Close all DLLs *)

let close_all_dlls () =
  List.iter dll_close !opened_dlls;
  opened_dlls := [];
  names_of_opened_dlls := []

(* Find a primitive in the currently opened DLLs.
   Raise [Not_found] if not found. *)

let find_primitive prim_name =
  let rec find seen = function
    [] ->
      raise Not_found
  | dll :: rem ->
      let addr = dll_sym dll prim_name in
      if addr == Obj.magic () then find (dll :: seen) rem else begin
        if seen <> [] then opened_dlls := dll :: List.rev_append seen rem;
        addr
      end in
  find [] !opened_dlls

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)

let linking_in_core = ref false

let synchronize_primitive num symb =
  if !linking_in_core then begin
    let actual_num = add_primitive symb in
    assert (actual_num = num)
  end

(* Read the [ld.conf] file and return the corresponding list of directories *)

let ld_conf_contents () =
  let path = ref [] in
  begin try
    let ic = open_in (Filename.concat Config.standard_library "ld.conf") in
    begin try
      while true do
        path := input_line ic :: !path
      done
    with End_of_file -> ()
    end;
    close_in ic
  with Sys_error _ -> ()
  end;
  List.rev !path

(* Split the CAML_LD_LIBRARY_PATH environment variable and return
   the corresponding list of directories.  *)

let split str sep =
  let rec split_rec pos =
    if pos >= String.length str then [] else begin
      try
        let newpos = String.index_from str pos sep in
        String.sub str pos (newpos - pos) ::
        split_rec (newpos + 1)
      with Not_found ->
        [String.sub str pos (String.length str - pos)]
    end in
  split_rec 0

let ld_library_path_contents () =
  let path_separator =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> ':'
    | "Win32" -> ';'
    | _ -> assert false in
  try
    split (Sys.getenv "CAML_LD_LIBRARY_PATH") path_separator
  with Not_found ->
    []

let split_dll_path path =
  split path '\000'

(* Initialization for separate compilation *)

let init_compile nostdlib =
  search_path :=
    ld_library_path_contents() @
    (if nostdlib then [] else ld_conf_contents())

(* Initialization for linking in core (dynlink or toplevel) *)

let init_toplevel dllpath =
  search_path :=
    ld_library_path_contents() @
    split_dll_path dllpath @
    ld_conf_contents();
  opened_dlls := Array.to_list (get_current_dlls());
  names_of_opened_dlls := [];
  linking_in_core := true

let reset () =
  search_path := [];
  opened_dlls :=[];
  names_of_opened_dlls := [];
  linking_in_core := false
