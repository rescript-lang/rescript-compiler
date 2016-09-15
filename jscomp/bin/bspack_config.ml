module Config : sig 
#1 "config.mli"
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

(* System configuration *)

val version: string
        (* The current version number of the system *)

val standard_library: string
        (* The directory containing the standard libraries *)
val standard_runtime: string
        (* The full path to the standard bytecode interpreter ocamlrun *)
val ccomp_type: string
        (* The "kind" of the C compiler, assembler and linker used: one of
               "cc" (for Unix-style C compilers)
               "msvc" (for Microsoft Visual C++ and MASM) *)
val bytecomp_c_compiler: string
        (* The C compiler to use for compiling C files
           with the bytecode compiler *)
val bytecomp_c_libraries: string
        (* The C libraries to link with custom runtimes *)
val native_c_compiler: string
        (* The C compiler to use for compiling C files
           with the native-code compiler *)
val native_c_libraries: string
        (* The C libraries to link with native-code programs *)
val native_pack_linker: string
        (* The linker to use for packaging (ocamlopt -pack) and for partial
           links (ocamlopt -output-obj). *)
val mkdll: string
        (* The linker command line to build dynamic libraries. *)
val mkexe: string
        (* The linker command line to build executables. *)
val mkmaindll: string
        (* The linker command line to build main programs as dlls. *)
val ranlib: string
        (* Command to randomize a library, or "" if not needed *)
val ar: string
        (* Name of the ar command, or "" if not needed  (MSVC) *)
val cc_profile : string
        (* The command line option to the C compiler to enable profiling. *)

val load_path: string list ref
        (* Directories in the search path for .cmi and .cmo files *)

val interface_suffix: string ref
        (* Suffix for interface file names *)

val exec_magic_number: string
        (* Magic number for bytecode executable files *)
val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val cmo_magic_number: string
        (* Magic number for object bytecode files *)
val cma_magic_number: string
        (* Magic number for archive files *)
val cmx_magic_number: string
        (* Magic number for compilation unit descriptions *)
val cmxa_magic_number: string
        (* Magic number for libraries of compilation unit descriptions *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmxs_magic_number: string
        (* Magic number for dynamically-loadable plugins *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a regular block. *)
val lazy_tag : int
        (* Normally the same as Obj.lazy_tag.  Separate definition because
           of technical reasons for bootstrapping. *)
val max_young_wosize: int
        (* Maximal size of arrays that are directly allocated in the
           minor heap *)
val stack_threshold: int
        (* Size in words of safe area at bottom of VM stack,
           see byterun/config.h *)

val architecture: string
        (* Name of processor type for the native-code compiler *)
val model: string
        (* Name of processor submodel for the native-code compiler *)
val system: string
        (* Name of operating system for the native-code compiler *)

val asm: string
        (* The assembler (and flags) to use for assembling
           ocamlopt-generated code. *)

val asm_cfi_supported: bool
        (* Whether assembler understands CFI directives *)
val with_frame_pointers : bool
        (* Whether assembler should maintain frame pointers *)

val ext_obj: string
        (* Extension for object files, e.g. [.o] under Unix. *)
val ext_asm: string
        (* Extension for assembler files, e.g. [.s] under Unix. *)
val ext_lib: string
        (* Extension for library files, e.g. [.a] under Unix. *)
val ext_dll: string
        (* Extension for dynamically-loaded libraries, e.g. [.so] under Unix.*)

val default_executable_name: string
        (* Name of executable produced by linking if none is given with -o,
           e.g. [a.out] under Unix. *)

val systhread_supported : bool
        (* Whether the system thread library is implemented *)

val host : string
        (* Whether the compiler is a cross-compiler *)

val target : string
        (* Whether the compiler is a cross-compiler *)

val print_config : out_channel -> unit;;

end = struct
#1 "config.ml"
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
let version = Sys.ocaml_version

let standard_library_default = "/usr/local/lib/ocaml/lib/ocaml" (* does not matter *)

let standard_library =
  try
    Sys.getenv "OCAMLLIB"
  with Not_found ->
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    standard_library_default

let standard_runtime = "/usr/local/bin/ocaml/bin/ocamlrun"
let ccomp_type = "cc"
let bytecomp_c_compiler = "gcc -O  -Wall -D_FILE_OFFSET_BITS=64 -D_REENTRANT -O "
let bytecomp_c_libraries = "-lcurses -lpthread"
let native_c_compiler = "gcc -O  -D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let native_c_libraries = ""
let native_pack_linker = "ld -r -arch x86_64  -o "
let ranlib = "ranlib"
let ar = "ar"
let cc_profile = "-pg"
let mkdll = "gcc -bundle -flat_namespace -undefined suppress -Wl,-no_compact_unwind"
let mkexe = "gcc -Wl,-no_compact_unwind"
let mkmaindll = "gcc -bundle -flat_namespace -undefined suppress -Wl,-no_compact_unwind"

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

let architecture = "amd64"
let model = "default"
let system = "macosx"

let asm = "clang -arch x86_64 -c"
let asm_cfi_supported = true
let with_frame_pointers = false

let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"

let host = "x86_64-apple-darwin15.6.0"
let target = "x86_64-apple-darwin15.6.0"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = true;;

let print_config oc = ()
;;

end
module Clflags : sig 
#1 "clflags.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val objfiles : string list ref
val ccobjs : string list ref
val dllibs : string list ref
val compile_only : bool ref
val output_name : string option ref
val include_dirs : string list ref
val no_std_include : bool ref
val print_types : bool ref
val make_archive : bool ref
val debug : bool ref
val fast : bool ref
val link_everything : bool ref
val custom_runtime : bool ref
val no_check_prims : bool ref
val bytecode_compatible_32 : bool ref
val output_c_object : bool ref
val output_complete_object : bool ref
val all_ccopts : string list ref
val classic : bool ref
val nopervasives : bool ref
val open_modules : string list ref
val preprocessor : string option ref
val all_ppx : string list ref
val annotations : bool ref
val binary_annotations : bool ref
val use_threads : bool ref
val use_vmthreads : bool ref
val noassert : bool ref
val verbose : bool ref
val noprompt : bool ref
val nopromptcont : bool ref
val init_file : string option ref
val noinit : bool ref
val use_prims : string ref
val use_runtime : string ref
val principal : bool ref
val real_paths : bool ref
val recursive_types : bool ref
val strict_sequence : bool ref
val strict_formats : bool ref
val applicative_functors : bool ref
val make_runtime : bool ref
val gprofile : bool ref
val c_compiler : string option ref
val no_auto_link : bool ref
val dllpaths : string list ref
val make_package : bool ref
val for_package : string option ref
val error_size : int ref
val float_const_prop : bool ref
val transparent_modules : bool ref
val dump_source : bool ref
val dump_parsetree : bool ref
val dump_typedtree : bool ref
val dump_rawlambda : bool ref
val dump_lambda : bool ref
val dump_clambda : bool ref
val dump_instr : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_cse : bool ref
val dump_live : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val inline_threshold : int ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val runtime_variant : string ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val unsafe_string : bool ref
val opaque : bool ref

end = struct
#1 "clflags.ml"
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

(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)

let compile_only = ref false            (* -c *)
and output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and no_std_include = ref false          (* -nostdlib *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and no_check_prims = ref false          (* -no-check-prims *)
and bytecode_compatible_32 = ref false  (* -compat-32 *)
and output_c_object = ref false         (* -output-obj *)
and output_complete_object = ref false  (* -output-complete-obj *)
and all_ccopts = ref ([] : string list)     (* -ccopt *)
and classic = ref false                 (* -nolabels *)
and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
and all_ppx = ref ([] : string list)        (* -ppx *)
let annotations = ref false             (* -annot *)
let binary_annotations = ref false      (* -annot *)
and use_threads = ref false             (* -thread *)
and use_vmthreads = ref false           (* -vmthread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and noprompt = ref false                (* -noprompt *)
and nopromptcont = ref false            (* -nopromptcont *)
and init_file = ref (None : string option)   (* -init *)
and noinit = ref false                  (* -noinit *)
and open_modules = ref []               (* -open *)
and use_prims = ref ""                  (* -use-prims ... *)
and use_runtime = ref ""                (* -use-runtime ... *)
and principal = ref false               (* -principal *)
and real_paths = ref true               (* -short-paths *)
and recursive_types = ref false         (* -rectypes *)
and strict_sequence = ref false         (* -strict-sequence *)
and strict_formats = ref false          (* -strict-formats *)
and applicative_functors = ref true     (* -no-app-funct *)
and make_runtime = ref false            (* -make-runtime *)
and gprofile = ref false                (* -p *)
and c_compiler = ref (None: string option) (* -cc *)
and no_auto_link = ref false            (* -noautolink *)
and dllpaths = ref ([] : string list)   (* -dllpath *)
and make_package = ref false            (* -pack *)
and for_package = ref (None: string option) (* -for-pack *)
and error_size = ref 500                (* -error-size *)
and float_const_prop = ref true         (* -no-float-const-prop *)
and transparent_modules = ref false     (* -trans-mod *)
let dump_source = ref false             (* -dsource *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_typedtree = ref false          (* -dtypedtree *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_clambda = ref false            (* -dclambda *)
and dump_instr = ref false              (* -dinstr *)

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
and opaque = ref false                  (* -opaque *)

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let keep_startup_file = ref false       (* -dstartup *)
let dump_combine = ref false            (* -dcombine *)
let native_code = ref false             (* set to true under ocamlopt *)
let inline_threshold = ref 10
let force_slash = ref false             (* for ocamldep *)

let dont_write_files = ref false        (* set to true under ocamldoc *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))
;;

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]
;;

let shared = ref false (* -shared *)
let dlcode = ref true (* not -nodynlink *)

let runtime_variant = ref "";;      (* -runtime-variant *)

let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref false              (* -keep-locs *)
let unsafe_string = ref true;;         (* -safe-string / -unsafe-string *)

end
