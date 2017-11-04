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

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)


(* The main OCaml version string has moved to ../VERSION *)
let version = "4.02.3+BS"
let standard_library =
  Filename.concat (Filename.dirname Sys.executable_name)  "ocaml"
let standard_library_default = standard_library

let standard_runtime = "ocamlrun" (*dont care:path to ocamlrun*)
let ccomp_type = "cc"(*dont care: cc or msvc*)
let bytecomp_c_compiler = "gcc -O  -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT -O" (*dont care*)
let bytecomp_c_libraries = "-lcurses -lpthread" (*dont care*)
let native_c_compiler = "gcc -O  -D_FILE_OFFSET_BITS=64 -D_REENTRANT" (*dont care*)
let native_c_libraries = ""(*dont care*)
let native_pack_linker = "ld -r -arch x86_64  -o"(*dont care*) 
let ranlib = "ranlib"(*dont care*)
let ar = ""(*dont care*)
let cc_profile = "-pg"(*dont care*)
let mkdll = ""(*dont care*)
let mkexe = ""(*dont care*)
let mkmaindll = ""(*dont care*)

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I017"
and cmo_magic_number = "Caml1999O010"
and cma_magic_number = "Caml1999A011"
and cmx_magic_number = "Caml1999Y014"
and cmxa_magic_number = "Caml1999Z013"
and ast_impl_magic_number = "Caml1999M016"
and ast_intf_magic_number = "Caml1999N015"
and cmxs_magic_number = "Caml2007D002"
and cmt_magic_number = "Caml2012T004"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)

let architecture = "amd64" (*dont care*)
let model = "default"(*dont care*)
let system = "macosx"

let asm = "clang -arch x86_64 -c"
let asm_cfi_supported = false (*dont care*)
let with_frame_pointers = false (*dontcare*)

let ext_obj = ".o" (*dont care*)
let ext_asm = ".s" (*dont care*)
let ext_lib = ".a" (*dont caer*)
let ext_dll = ".a" (*dont care*)

let host = "%%HOST%%"
let target = "%%TARGET%%"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = false (*dontcare*);;

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  let p_bool name valu = Printf.fprintf oc "%s: %B\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "standard_runtime" standard_runtime;
  p "ccomp_type" ccomp_type;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_compiler" native_c_compiler;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p "ranlib" ranlib;
  p "cc_profile" cc_profile;
  p "architecture" architecture;
  p "model" model;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;

  (* print the magic number *)
  p "exec_magic_number" exec_magic_number;
  p "cmi_magic_number" cmi_magic_number;
  p "cmo_magic_number" cmo_magic_number;
  p "cma_magic_number" cma_magic_number;
  p "cmx_magic_number" cmx_magic_number;
  p "cmxa_magic_number" cmxa_magic_number;
  p "ast_impl_magic_number" ast_impl_magic_number;
  p "ast_intf_magic_number" ast_intf_magic_number;
  p "cmxs_magic_number" cmxs_magic_number;
  p "cmt_magic_number" cmt_magic_number;

  flush oc;
;;

