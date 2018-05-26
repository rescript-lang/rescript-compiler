module Config_whole_compiler : sig 
#1 "config_whole_compiler.mli"
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
(* val system: string *)
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

(* val host : string *)
        (* Whether the compiler is a cross-compiler *)

(* val target : string *)
        (* Whether the compiler is a cross-compiler *)

val print_config : out_channel -> unit;;


end = struct
#1 "config_whole_compiler.ml"
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


end
module Config = Config_whole_compiler 
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


 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
val no_implicit_current_dir : bool ref
val assume_no_mli : mli_status ref 
val record_event_when_debug : bool ref 
val bs_vscode : bool
val dont_record_crc_unit : string option ref
val bs_only : bool ref (* set true on bs top*)
val no_assert_false : bool ref


type color_setting = Auto | Always | Never
val parse_color_setting : string -> color_setting option
val color : color_setting ref


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


 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
let no_implicit_current_dir = ref false
let assume_no_mli = ref Mli_na
let record_event_when_debug = ref true (* turned off in BuckleScript*)
let bs_vscode = 
    try ignore @@ Sys.getenv "BS_VSCODE" ; true with _ -> false
    (* We get it from environment variable mostly due to 
       we don't want to rebuild when flip on or off
    *)
let dont_record_crc_unit : string option ref = ref None
let bs_only = ref false
let no_assert_false = ref false


type color_setting = Auto | Always | Never
let parse_color_setting = function
  | "auto" -> Some Auto
  | "always" -> Some Always
  | "never" -> Some Never
  | _ -> None
let color = ref Auto ;; (* -color *)


end
module Misc : sig 
#1 "misc.mli"
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

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a;;

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
        (* [map_end f l t] is [map f l @ t], just more efficient. *)
val map_left_right: ('a -> 'b) -> 'a list -> 'b list
        (* Like [List.map], with guaranteed left-to-right evaluation order *)
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [List.for_all] but for a binary predicate.
           In addition, this [for_all2] never fails: given two lists
           with different lengths, it returns false. *)
val replicate_list: 'a -> int -> 'a list
        (* [replicate_list elem n] is the list with [n] elements
           all identical to [elem]. *)
val list_remove: 'a -> 'a list -> 'a list
        (* [list_remove x l] returns a copy of [l] with the first
           element equal to [x] removed. *)
val split_last: 'a list -> 'a list * 'a
        (* Return the last element and the other elements of the given list. *)
val samelist: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
        (* Like [List.for_all2] but returns [false] if the two
           lists have different length. *)

val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
val find_in_path_rel: string list -> string -> string
        (* Search a relative file in a list of directories. *)
val find_in_path_uncap: string list -> string -> string
        (* Same, but search also for uncapitalized name, i.e.
           if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
           to match. *)
val remove_file: string -> unit
        (* Delete the given file if it exists. Never raise an error. *)
val expand_directory: string -> string -> string
        (* [expand_directory alt file] eventually expands a [+] at the
           beginning of file into [alt] (an alternate root directory) *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
val string_of_file: in_channel -> string
        (* [string_of_file ic] reads the contents of file [ic] and copies
           them to a string. It stops when encountering EOF on [ic]. *)
val log2: int -> int
        (* [log2 n] returns [s] such that [n = 1 lsl s]
           if [n] is a power of 2*)
val align: int -> int -> int
        (* [align n a] rounds [n] upwards to a multiple of [a]
           (a power of 2). *)
val no_overflow_add: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 + n2] does not overflow. *)
val no_overflow_sub: int -> int -> bool
        (* [no_overflow_add n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_lsl: int -> bool
        (* [no_overflow_add n] returns [true] if the computation of
           [n lsl 1] does not overflow. *)

val chop_extension_if_any: string -> string
        (* Like Filename.chop_extension but returns the initial file
           name if it has no extension *)

val chop_extensions: string -> string
        (* Return the given file name without its extensions. The extensions
           is the longest suffix starting with a period and not including
           a directory separator, [.xyz.uvw] for instance.

           Return the given name if it does not contain an extension. *)

val search_substring: string -> string -> int -> int
        (* [search_substring pat str start] returns the position of the first
           occurrence of string [pat] in string [str].  Search starts
           at offset [start] in [str].  Raise [Not_found] if [pat]
           does not occur. *)

val replace_substring: before:string -> after:string -> string -> string
        (* [search_substring ~before ~after str] replaces all occurences
           of [before] with [after] in [str] and returns the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and return
           the list of words in reverse order. *)

val get_ref: 'a list ref -> 'a list
        (* [get_ref lr] returns the content of the list reference [lr] and reset
           its content to the empty list. *)


val fst3: 'a * 'b * 'c -> 'a
val snd3: 'a * 'b * 'c -> 'b
val thd3: 'a * 'b * 'c -> 'c

val fst4: 'a * 'b * 'c * 'd -> 'a
val snd4: 'a * 'b * 'c * 'd -> 'b
val thd4: 'a * 'b * 'c * 'd -> 'c
val for4: 'a * 'b * 'c * 'd -> 'd

module LongString :
  sig
    type t = bytes array
    val create : int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val output : out_channel -> t -> int -> int -> unit
    val unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
    val input_bytes : in_channel -> int -> t
  end

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val split : string -> char -> string list
(** [String.split string char] splits the string [string] at every char
    [char], and returns the list of sub-strings between the chars.
    [String.concat (String.make 1 c) (String.split s c)] is the identity.
    @since 4.01
 *)

val cut_at : string -> char -> string * string
(** [String.cut_at s c] returns a pair containing the sub-string before
   the first occurrence of [c] in [s], and the sub-string after the
   first occurrence of [c] in [s].
   [let (before, after) = String.cut_at s c in
    before ^ String.make 1 c ^ after] is the identity if [s] contains [c].

   Raise [Not_found] if the character does not appear in the string
   @since 4.01
*)





(* Color handling *)
module Color : sig
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

    | Dim


  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  val default_styles: styles
  val get_styles: unit -> styles
  val set_styles: styles -> unit

  val setup : Clflags.color_setting -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end


end = struct
#1 "misc.ml"
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

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
    [] -> l2
  | hd::tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
    [] -> []
  | hd::tl -> let res = f hd in res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (hd1::tl1, hd2::tl2) -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | (_, _) -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n-1)

let rec list_remove x = function
    [] -> []
  | hd :: tl ->
      if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
    [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
      let (lst, last) = split_last tl in
      (hd :: lst, last)

let rec samelist pred l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (hd1 :: tl1, hd2 :: tl2) -> pred hd1 hd2 && samelist pred tl1 tl2
  | (_, _) -> false

(* Options *)

let may f = function
    Some x -> f x
  | None -> ()

let may_map f = function
    Some x -> Some (f x)
  | None -> None

(* File functions *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let find_in_path_rel path name =
  let rec simplify s =
    let open Filename in
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then simplify dir
    else concat (simplify dir) base
  in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = simplify (Filename.concat dir name) in
      if Sys.file_exists fullname then fullname else try_dir rem
  in try_dir path

let find_in_path_uncap path name =
  let uname = String.uncapitalize name in
  let rec try_dir = function
    [] -> raise Not_found
  | dir::rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in try_dir path

let remove_file filename =
  try
    Sys.remove filename
  with Sys_error msg ->
    ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+'
  then Filename.concat alt
                       (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let copy_file ic oc =
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then () else (output oc buff 0 n; copy())
  in copy()

let copy_file_chunk ic oc len =
  let buff = Bytes.create 0x1000 in
  let rec copy n =
    if n <= 0 then () else begin
      let r = input ic buff 0 (min n 0x1000) in
      if r = 0 then raise End_of_file else (output oc buff 0 r; copy(n-r))
    end
  in copy len

let string_of_file ic =
  let b = Buffer.create 0x10000 in
  let buff = Bytes.create 0x1000 in
  let rec copy () =
    let n = input ic buff 0 0x1000 in
    if n = 0 then Buffer.contents b else
      (Buffer.add_subbytes b buff 0 n; copy())
  in copy()

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_lsl a = min_int asr 1 <= a && a <= max_int asr 1

(* String operations *)

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else
      Filename.concat dirname basename
  with Not_found -> file

let search_substring pat str start =
  let rec search i j =
    if j >= String.length pat then i
    else if i + j >= String.length str then raise Not_found
    else if str.[i + j] = pat.[j] then search i (j+1)
    else search (i+1) 0
  in search start 0

let replace_substring ~before ~after str =
  let rec search acc curr =
    match search_substring before str curr with
      | next ->
         let prefix = String.sub str curr (next - curr) in
         search (prefix :: acc) (next + String.length before)
      | exception Not_found ->
        let suffix = String.sub str curr (String.length str - curr) in
        List.rev (suffix :: acc)
  in String.concat after (search [] 0)

let rev_split_words s =
  let rec split1 res i =
    if i >= String.length s then res else begin
      match s.[i] with
        ' ' | '\t' | '\r' | '\n' -> split1 res (i+1)
      | _ -> split2 res i (i+1)
    end
  and split2 res i j =
    if j >= String.length s then String.sub s i (j-i) :: res else begin
      match s.[j] with
        ' ' | '\t' | '\r' | '\n' -> split1 (String.sub s i (j-i) :: res) (j+1)
      | _ -> split2 res i (j+1)
    end
  in split1 [] 0

let get_ref r =
  let v = !r in
  r := []; v

let fst3 (x, _, _) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let fst4 (x, _, _, _) = x
let snd4 (_,x,_, _) = x
let thd4 (_,_,x,_) = x
let for4 (_,_,_,x) = x


module LongString = struct
  type t = bytes array

  let create str_size =
    let tbl_size = str_size / Sys.max_string_length + 1 in
    let tbl = Array.make tbl_size Bytes.empty in
    for i = 0 to tbl_size - 2 do
      tbl.(i) <- Bytes.create Sys.max_string_length;
    done;
    tbl.(tbl_size - 1) <- Bytes.create (str_size mod Sys.max_string_length);
    tbl

  let length tbl =
    let tbl_size = Array.length tbl in
    Sys.max_string_length * (tbl_size - 1) + Bytes.length tbl.(tbl_size - 1)

  let get tbl ind =
    Bytes.get tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)

  let set tbl ind c =
    Bytes.set tbl.(ind / Sys.max_string_length) (ind mod Sys.max_string_length)
              c

  let blit src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      set dst (dstoff + i) (get src (srcoff + i))
    done

  let output oc tbl pos len =
    for i = pos to pos + len - 1 do
      output_char oc (get tbl i)
    done

  let unsafe_blit_to_bytes src srcoff dst dstoff len =
    for i = 0 to len - 1 do
      Bytes.unsafe_set dst (dstoff + i) (get src (srcoff + i))
    done

  let input_bytes ic len =
    let tbl = create len in
    Array.iter (fun str -> really_input ic str 0 (Bytes.length str)) tbl;
    tbl
end


let edit_distance a b cutoff =
  let la, lb = String.length a, String.length b in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff in
  if abs (la - lb) > cutoff then None
  else begin
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i;
    done;
    for j = 1 to lb do
      m.(0).(j) <- j;
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i-1] = b.[j-1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i-1).(j) m.(i).(j-1)) (m.(i-1).(j-1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if not (i > 1 && j > 1 && a.[i-1] = b.[j-2] && a.[i-2] = b.[j-1])
          then best
          else min best (m.(i-2).(j-2) + cost)
        in
        m.(i).(j) <- best
      done;
    done;
    let result = m.(la).(lb) in
    if result > cutoff
    then None
    else Some result
  end


(* split a string [s] at every char [c], and return the list of sub-strings *)
let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)





(* Color handling *)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  ;;

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset

    | Dim


  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"

    | Dim -> "2"


  let ansi_of_style_l l =
    let s = match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {
    error: style list;
    warning: style list;
    loc: style list;
  }

  let default_styles = {
    warning = [Bold; FG Magenta];
    error = [Bold; FG Red];
    loc = [Bold];
  }

  let cur_styles = ref default_styles
  let get_styles () = !cur_styles
  let set_styles s = cur_styles := s

  (* map a tag to a style, if the tag is known.
     @raise Not_found otherwise *)
  let style_of_tag s = match s with
    | "error" -> (!cur_styles).error
    | "warning" -> (!cur_styles).warning
    | "loc" -> (!cur_styles).loc

    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]

    | _ -> raise Not_found

  let color_enabled = ref true

  (* either prints the tag of [s] or delegate to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_tag_functions ppf () in
    let functions' = {functions with
      mark_open_tag=(mark_open_tag ~or_else:functions.mark_open_tag);
      mark_close_tag=(mark_close_tag ~or_else:functions.mark_close_tag);
    } in
    pp_set_mark_tags ppf true; (* enable tags *)
    pp_set_formatter_tag_functions ppf functions'

  (* external isatty : out_channel -> bool = "caml_sys_isatty" *)

  (* reasonable heuristic on whether colors should be enabled *)
   let should_enable_color () = false  
(*    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> "" *)
(*    && isatty stderr *)

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l = [Format.std_formatter; Format.err_formatter; Format.str_formatter] in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled := (match o with
          | Clflags.Always -> true
          | Clflags.Auto -> should_enable_color ()
          | Clflags.Never -> false
        )
      );
      ()
end


end
module Terminfo : sig 
#1 "terminfo.mli"
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

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int  (* number of lines of the terminal *)
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end = struct
#1 "terminfo.ml"
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

(* Basic interface to the terminfo database *)

type status =
  | Uninitialised
  | Bad_term
  | Good_term of int
;;
external setup : out_channel -> status = "caml_terminfo_setup";;
external backup : int -> unit = "caml_terminfo_backup";;
external standout : bool -> unit = "caml_terminfo_standout";;
external resume : int -> unit = "caml_terminfo_resume";;

end
module Warnings : sig 
#1 "warnings.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string                    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted                          (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Without_principality of string          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (* 30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool (* 37 *)
  | Unused_extension of string * bool * bool   (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool   (* 40 *)
  | Ambiguous_name of string list * string list * bool (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
;;

val parse_options : bool -> string -> unit;;

val is_active : t -> bool;;
val is_error : t -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

val print : formatter -> t -> unit;;

exception Errors of int;;

val check_fatal : unit -> unit;;

val help_warnings: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit


val message : t -> string 
val number: t -> int
val super_print : (t -> string) -> formatter -> t -> unit;;


end = struct
#1 "warnings.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m   in ocaml
   - man/ocamlopt.m in ocaml
   - manual/cmds/comp.etex   in the doc sources
   - manual/cmds/native.etex in the doc sources
*)

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string                    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted                          (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Non_closed_record_pattern of string     (*  9 *)
  | Statement_type                          (* 10 *)
  | Unused_match                            (* 11 *)
  | Unused_pat                              (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Without_principality of string          (* 19 *)
  | Unused_argument                         (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Multiple_definition of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * bool * bool  (* 37 *)
  | Unused_extension of string * bool * bool    (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool    (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string                   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
;;

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Deprecated _ -> 3
  | Fragile_match _ -> 4
  | Partial_application -> 5
  | Labels_omitted -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Non_closed_record_pattern _ -> 9
  | Statement_type -> 10
  | Unused_match -> 11
  | Unused_pat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Without_principality _ -> 19
  | Unused_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 25
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Multiple_definition _ -> 31
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Bad_docstring _ -> 50

  | Bs_unused_attribute _ -> 101
  | Bs_polymorphic_comparison -> 102
  | Bs_ffi_warning _ -> 103
  | Bs_derive_warning _ -> 104
;;

let last_warning_number = 104
(* Must be the max number returned by the [number] function. *)
let letter_all = 
  let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
  loop last_warning_number

let letter = function
  | 'a' ->
    letter_all
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false
;;

type state =
  {
    active: bool array;
    error: bool array;
  }

let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
    }

let backup () = !current

let restore x = current := x

let is_active x = (!current).active.(number x);;
let is_error x = (!current).error.(number x);;

let parse_opt error active flags s =
  let set i = flags.(i) <- true in
  let clear i = flags.(i) <- false in
  let set_all i = active.(i) <- true; error.(i) <- true in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop i =
    if i >= String.length s then () else
    match s.[i] with
    | 'A' .. 'Z' ->
       List.iter set (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter clear (letter s.[i]);
       loop (i+1)
    | '+' -> loop_letter_num set (i+1)
    | '-' -> loop_letter_num clear (i+1)
    | '@' -> loop_letter_num set_all (i+1)
    | c -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 last_warning_number do myset n done;
        loop i
    | 'A' .. 'Z' ->
       List.iter myset (letter (Char.lowercase s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter myset (letter s.[i]);
       loop (i+1)
    | _ -> error ()
  in
  loop 0
;;

let parse_options errflag s =
  let error = Array.copy (!current).error in
  let active = Array.copy (!current).active in
  parse_opt error active (if errflag then error else active) s;
  current := {error; active}

(* If you change these, don't forget to change them in man/ocamlc.m *)
let defaults_w = "+a-4-6-7-9-27-29-32..39-41..42-44-45-48-50-102";;
let defaults_warn_error = "-a";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

let message = function
  | Comment_start -> "this is the start of a comment."
  | Comment_not_end -> "this is not the end of a comment."
  | Deprecated s -> "deprecated: " ^ s
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted ->
      "labels were omitted in the application of this function."
  | Method_override [lab] ->
      "the method " ^ lab ^ " is overridden."
  | Method_override (cname :: slist) ->
      String.concat " "
        ("the following methods are overridden by the class"
         :: cname  :: ":\n " :: slist)
  | Method_override [] -> assert false
  | Partial_match "" -> "this pattern-matching is not exhaustive."
  | Partial_match s ->
      "this pattern-matching is not exhaustive.\n\
       Here is an example of a value that is not matched:\n" ^ s
  | Non_closed_record_pattern s ->
      "the following labels are not bound in this record pattern:\n" ^ s ^
      "\nEither bind these labels explicitly or add '; _' to the pattern."
  | Statement_type ->
      "this expression should have type unit."
  | Unused_match -> "this match case is unused."
  | Unused_pat   -> "this sub-pattern is unused."
  | Instance_variable_override [lab] ->
      "the instance variable " ^ lab ^ " is overridden.\n" ^
      "The behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override (cname :: slist) ->
      String.concat " "
        ("the following instance variables are overridden by the class"
         :: cname  :: ":\n " :: slist) ^
      "\nThe behaviour changed in ocaml 3.10 (previous behaviour was hiding.)"
  | Instance_variable_override [] -> assert false
  | Illegal_backslash -> "illegal backslash escape in string."
  | Implicit_public_methods l ->
      "the following private methods were made public implicitly:\n "
      ^ String.concat " " l ^ "."
  | Unerasable_optional_argument -> "this optional argument cannot be erased."
  | Undeclared_virtual_method m -> "the virtual method "^m^" is not declared."
  | Not_principal s -> s^" is not principal."
  | Without_principality s -> s^" without principality."
  | Unused_argument -> "this argument will not be used by the function."
  | Nonreturning_statement ->
      "this statement never returns (or has an unsound type.)"
  | Preprocessor s -> s
  | Useless_record_with ->
      "all the fields are explicitly listed in this record:\n\
       the 'with' clause is useless."
  | Bad_module_name (modname) ->
      "bad source file name: \"" ^ modname ^ "\" is not a valid module name."
  | All_clauses_guarded ->
      "bad style, all clauses in this pattern-matching are guarded."
  | Unused_var v | Unused_var_strict v -> "unused variable " ^ v ^ "."
  | Wildcard_arg_to_constant_constr ->
     "wildcard pattern given as argument to a constant constructor"
  | Eol_in_string ->
     "unescaped end-of-line in a string constant (non-portable code)"
  | Duplicate_definitions (kind, cname, tc1, tc2) ->
      Printf.sprintf "the %s %s is defined in both types %s and %s."
        kind cname tc1 tc2
  | Multiple_definition(modname, file1, file2) ->
      Printf.sprintf
        "files %s and %s both define a module named %s"
        file1 file2 modname
  | Unused_value_declaration v -> "unused value " ^ v ^ "."
  | Unused_open s -> "unused open " ^ s ^ "."
  | Unused_type_declaration s -> "unused type " ^ s ^ "."
  | Unused_for_index s -> "unused for-loop index " ^ s ^ "."
  | Unused_ancestor s -> "unused ancestor variable " ^ s ^ "."
  | Unused_constructor (s, false, false) -> "unused constructor " ^ s ^ "."
  | Unused_constructor (s, true, _) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_constructor (s, false, true) ->
      "constructor " ^ s ^
      " is never used to build values.\n\
        Its type is exported as a private type."
  | Unused_extension (s, false, false) ->
      "unused extension constructor " ^ s ^ "."
  | Unused_extension (s, true, _) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        (However, this constructor appears in patterns.)"
  | Unused_extension (s, false, true) ->
      "extension constructor " ^ s ^
      " is never used to build values.\n\
        It is exported or rebound as a private extension."
  | Unused_rec_flag ->
      "unused rec flag."
  | Name_out_of_scope (ty, [nm], false) ->
      nm ^ " was selected from type " ^ ty ^
      ".\nIt is not visible in the current scope, and will not \n\
       be selected if the type becomes unknown."
  | Name_out_of_scope (_, _, false) -> assert false
  | Name_out_of_scope (ty, slist, true) ->
      "this record of type "^ ty ^" contains fields that are \n\
       not visible in the current scope: "
      ^ String.concat " " slist ^ ".\n\
       They will not be selected if the type becomes unknown."
  | Ambiguous_name ([s], tl, false) ->
      s ^ " belongs to several types: " ^ String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Ambiguous_name (_, _, false) -> assert false
  | Ambiguous_name (slist, tl, true) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Disambiguated_name s ->
      "this use of " ^ s ^ " required disambiguation."
  | Nonoptional_label s ->
      "the label " ^ s ^ " is not optional."
  | Open_shadow_identifier (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s identifier %s (which is later used)"
        kind s
  | Open_shadow_label_constructor (kind, s) ->
      Printf.sprintf
        "this open statement shadows the %s %s (which is later used)"
        kind s
  | Bad_env_variable (var, s) ->
      Printf.sprintf "illegal environment variable %s : %s" var s
  | Attribute_payload (a, s) ->
      Printf.sprintf "illegal payload for attribute '%s'.\n%s" a s
  | Eliminated_optional_arguments sl ->
      Printf.sprintf "implicit elimination of optional argument%s %s"
        (if List.length sl = 1 then "" else "s")
        (String.concat ", " sl)
  | No_cmi_file s ->
      "no cmi file was found in path for module " ^ s
  | Bad_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Bs_unused_attribute s ->
      "Unused BuckleScript attribute: " ^ s
  | Bs_polymorphic_comparison ->
      "polymorphic comparison introduced (maybe unsafe)"
  | Bs_ffi_warning s ->
      "BuckleScript FFI warning: " ^ s
  | Bs_derive_warning s ->
      "BuckleScript bs.deriving warning: " ^ s 
;;

let nerrors = ref 0;;

let print ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%d: %s" num msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;


(* used by super-errors. Copied from the `print` above *)
let super_print message ppf w =
  let msg = message w in
  let num = number w in
  Format.fprintf ppf "%s" msg;
  Format.pp_print_flush ppf ();
  if (!current).error.(num) then incr nerrors
;;


exception Errors of int;;

let check_fatal () =
  if !nerrors > 0 then begin
    let e = Errors !nerrors in
    nerrors := 0;
    raise e;
  end;
;;

let descriptions =
  [
    1, "Suspicious-looking start-of-comment mark.";
    2, "Suspicious-looking end-of-comment mark.";
    3, "Deprecated feature.";
    4, "Fragile pattern matching: matching that will remain complete even\n\
   \    if additional constructors are added to one of the variant types\n\
   \    matched.";
    5, "Partially applied function: expression whose result has function\n\
   \    type and is ignored.";
    6, "Label omitted in function application.";
    7, "Method overridden.";
    8, "Partial match: missing cases in pattern-matching.";
    9, "Missing fields in a record pattern.";
   10, "Expression on the left-hand side of a sequence that doesn't have type\n\
   \    \"unit\" (and that is not a function, see warning number 5).";
   11, "Redundant case in a pattern matching (unused match case).";
   12, "Redundant sub-pattern in a pattern-matching.";
   13, "Instance variable overridden.";
   14, "Illegal backslash escape in a string constant.";
   15, "Private method made public implicitly.";
   16, "Unerasable optional argument.";
   17, "Undeclared virtual method.";
   18, "Non-principal type.";
   19, "Type without principality.";
   20, "Unused function argument.";
   21, "Non-returning statement.";
   22, "Proprocessor warning.";
   23, "Useless record \"with\" clause.";
   24, "Bad module name: the source file name is not a valid OCaml module \
        name.";
   25, "Pattern-matching with all clauses guarded.  Exhaustiveness cannot be\n\
   \    checked.";
   26, "Suspicious unused variable: unused variable that is bound\n\
   \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   27, "Innocuous unused variable: unused variable that is not bound with\n\
   \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
   \    character.";
   28, "Wildcard pattern given as argument to a constant constructor.";
   29, "Unescaped end-of-line in a string constant (non-portable code).";
   30, "Two labels or constructors of the same name are defined in two\n\
   \    mutually recursive types.";
   31, "A module is linked twice in the same executable.";
   32, "Unused value declaration.";
   33, "Unused open statement.";
   34, "Unused type declaration.";
   35, "Unused for-loop index.";
   36, "Unused ancestor variable.";
   37, "Unused constructor.";
   38, "Unused extension constructor.";
   39, "Unused rec flag.";
   40, "Constructor or label name used out of scope.";
   41, "Ambiguous constructor or label name.";
   42, "Disambiguated constructor or label name.";
   43, "Nonoptional label applied as optional.";
   44, "Open statement shadows an already defined identifier.";
   45, "Open statement shadows an already defined label or constructor.";
   46, "Error in environment variable.";
   47, "Illegal attribute payload.";
   48, "Implicit elimination of optional arguments.";
   49, "Missing cmi file when looking up module alias.";
   50, "Unexpected documentation comment.";
   101,"Unused bs attributes";
  ]
;;

let help_warnings () =
  List.iter (fun (i, s) -> Printf.printf "%3i %s\n" i s) descriptions;
  print_endline "  A all warnings";
  for i = Char.code 'b' to Char.code 'z' do
    let c = Char.chr i in
    match letter c with
    | [] -> ()
    | [n] ->
        Printf.printf "  %c warning %i\n" (Char.uppercase c) n
    | l ->
        Printf.printf "  %c warnings %s.\n"
          (Char.uppercase c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;

end
module Location : sig 
#1 "location.mli"
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

(* Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(* Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val get_pos_info: Lexing.position -> string * int * int (* file, line, char *)
val print_loc: formatter -> t -> unit
val print_error: formatter -> t -> unit
val print_error_cur_file: formatter -> unit -> unit
val print_warning: t -> formatter -> Warnings.t -> unit
val formatter_for_warnings : formatter ref
val prerr_warning: t -> Warnings.t -> unit
val echo_eof: unit -> unit
val reset: unit -> unit

val warning_printer : (t -> formatter -> Warnings.t -> unit) ref
(** Hook for intercepting warnings. *)

val default_warning_printer : t -> formatter -> Warnings.t -> unit
(** Original warning printer for use in hooks. *)

val highlight_locations: formatter -> t list -> bool

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc

val print: formatter -> t -> unit
val print_filename: formatter -> string -> unit

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)


val absname: bool ref

(* Support for located errors *)

type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

exception Error of error

val print_error_prefix: formatter -> unit -> unit
  (* print the prefix "Error:" possibly with style *)

val error: ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error

 
val pp_ksprintf : ?before:(formatter -> unit) -> (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b


val errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, error) format4 -> 'a

val raise_errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val error_of_printer: t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn: exn -> error option

val register_error_of_exn: (exn -> error option) -> unit
  (* Each compiler module which defines a custom type of exception
     which can surface as a user-visible error should register
     a "printer" for this exception using [register_error_of_exn].
     The result of the printer is an [error] value containing
     a location, a message, and optionally sub-messages (each of them
     being located as well). *)

val report_error: formatter -> error -> unit

val error_reporter : (formatter -> error -> unit) ref
(** Hook for intercepting error reports. *)

val default_error_reporter : formatter -> error -> unit
(** Original error reporter for use in hooks. *)

val report_exception: formatter -> exn -> unit
  (* Reraise the exception if it is unknown. *)

end = struct
#1 "location.ml"
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

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = { loc_start: position; loc_end: position; loc_ghost: bool };;

let in_file name =
  let loc = {
    pos_fname = name;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = -1;
  } in
  { loc_start = loc; loc_end = loc; loc_ghost = true }
;;

let none = in_file "_none_";;

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
  loc_ghost = false
};;

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = false;
};;

let symbol_gloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
  loc_ghost = true;
};;

let rhs_loc n = {
  loc_start = Parsing.rhs_start_pos n;
  loc_end = Parsing.rhs_end_pos n;
  loc_ghost = false;
};;

let input_name = ref "_none_"
let input_lexbuf = ref (None : lexbuf option)

(* Terminal info *)

let status = ref Terminfo.Uninitialised

let num_loc_lines = ref 0 (* number of lines already printed after input *)

let print_updating_num_loc_lines ppf f arg =
  let open Format in
  let out_functions = pp_get_formatter_out_functions ppf () in
  let out_string str start len =
    let rec count i c =
      if i = start + len then c
      else if String.get str i = '\n' then count (succ i) (succ c)
      else count (succ i) c in
    num_loc_lines := !num_loc_lines + count start 0 ;
    out_functions.out_string str start len in
  pp_set_formatter_out_functions ppf
    { out_functions with out_string } ;
  f ppf arg ;
  pp_print_flush ppf ();
  pp_set_formatter_out_functions ppf out_functions

(* Highlight the locations using standout mode. *)

let highlight_terminfo ppf num_lines lb locs =
  Format.pp_print_flush ppf ();  (* avoid mixing Format and normal output *)
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  (* Count number of lines in phrase *)
  let lines = ref !num_loc_lines in
  for i = pos0 to lb.lex_buffer_len - 1 do
    if Bytes.get lb.lex_buffer i = '\n' then incr lines
  done;
  (* If too many lines, give up *)
  if !lines >= num_lines - 2 then raise Exit;
  (* Move cursor up that number of lines *)
  flush stdout; Terminfo.backup !lines;
  (* Print the input, switching to standout for the location *)
  let bol = ref false in
  print_string "# ";
  for pos = 0 to lb.lex_buffer_len - pos0 - 1 do
    if !bol then (print_string "  "; bol := false);
    if List.exists (fun loc -> pos = loc.loc_start.pos_cnum) locs then
      Terminfo.standout true;
    if List.exists (fun loc -> pos = loc.loc_end.pos_cnum) locs then
      Terminfo.standout false;
    let c = Bytes.get lb.lex_buffer (pos + pos0) in
    print_char c;
    bol := (c = '\n')
  done;
  (* Make sure standout mode is over *)
  Terminfo.standout false;
  (* Position cursor back to original location *)
  Terminfo.resume !num_loc_lines;
  flush stdout

(* Highlight the location by printing it again. *)

let highlight_dumb ppf lb loc =
  (* Char 0 is at offset -lb.lex_abs_pos in lb.lex_buffer. *)
  let pos0 = -lb.lex_abs_pos in
  (* Do nothing if the buffer does not contain the whole phrase. *)
  if pos0 < 0 then raise Exit;
  let end_pos = lb.lex_buffer_len - pos0 - 1 in
  (* Determine line numbers for the start and end points *)
  let line_start = ref 0 and line_end = ref 0 in
  for pos = 0 to end_pos do
    if Bytes.get lb.lex_buffer (pos + pos0) = '\n' then begin
      if loc.loc_start.pos_cnum > pos then incr line_start;
      if loc.loc_end.pos_cnum   > pos then incr line_end;
    end
  done;
  (* Print character location (useful for Emacs) *)
  Format.fprintf ppf "Characters %i-%i:@."
                 loc.loc_start.pos_cnum loc.loc_end.pos_cnum;
  (* Print the input, underlining the location *)
  Format.pp_print_string ppf "  ";
  let line = ref 0 in
  let pos_at_bol = ref 0 in
  for pos = 0 to end_pos do
    match Bytes.get lb.lex_buffer (pos + pos0) with
    | '\n' ->
      if !line = !line_start && !line = !line_end then begin
        (* loc is on one line: underline location *)
        Format.fprintf ppf "@.  ";
        for _i = !pos_at_bol to loc.loc_start.pos_cnum - 1 do
          Format.pp_print_char ppf ' '
        done;
        for _i = loc.loc_start.pos_cnum to loc.loc_end.pos_cnum - 1 do
          Format.pp_print_char ppf '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        Format.fprintf ppf "@.";
        if pos < loc.loc_end.pos_cnum then Format.pp_print_string ppf "  "
      end;
      incr line;
      pos_at_bol := pos + 1
    | '\r' -> () (* discard *)
    | c ->
      if !line = !line_start && !line = !line_end then
        (* loc is on one line: print whole line *)
        Format.pp_print_char ppf c
      else if !line = !line_start then
        (* first line of multiline loc:
           print a dot for each char before loc_start *)
        if pos < loc.loc_start.pos_cnum then
          Format.pp_print_char ppf '.'
        else
          Format.pp_print_char ppf c
      else if !line = !line_end then
        (* last line of multiline loc: print a dot for each char
           after loc_end, even whitespaces *)
        if pos < loc.loc_end.pos_cnum then
          Format.pp_print_char ppf c
        else
          Format.pp_print_char ppf '.'
      else if !line > !line_start && !line < !line_end then
        (* intermediate line of multiline loc: print whole line *)
        Format.pp_print_char ppf c
  done

(* Highlight the location using one of the supported modes. *)

let rec highlight_locations ppf locs =
  match !status with
    Terminfo.Uninitialised ->
      status := Terminfo.setup stdout; highlight_locations ppf locs
  | Terminfo.Bad_term ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          let norepeat =
            try Sys.getenv "TERM" = "norepeat" with Not_found -> false in
          if norepeat then false else
            let loc1 = List.hd locs in
            try highlight_dumb ppf lb loc1; true
            with Exit -> false
      end
  | Terminfo.Good_term num_lines ->
      begin match !input_lexbuf with
        None -> false
      | Some lb ->
          try highlight_terminfo ppf num_lines lb locs; true
          with Exit -> false
      end

(* Print the location in some way or another *)

open Format

let absolute_path s = (* This function could go into Filename *)
  let open Filename in
  let s = if is_relative s then concat (Sys.getcwd ()) s else s in
  (* Now simplify . and .. components *)
  let rec aux s =
    let base = basename s in
    let dir = dirname s in
    if dir = s then dir
    else if base = current_dir_name then aux dir
    else if base = parent_dir_name then dirname (aux dir)
    else concat (aux dir) base
  in
  aux s

let show_filename file =
  if !absname then absolute_path file else file

let print_filename ppf file =
  Format.fprintf ppf "%s" (show_filename file)

let reset () =
  num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) =
  ("File \"", "\", line ", ", characters ", "-", ":")

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
;;

let setup_colors () =
  Misc.Color.setup !Clflags.color

let print_loc ppf loc =
  setup_colors ();
  let (file, line, startchar) = get_pos_info loc.loc_start in
 
  let startchar = 
    if Clflags.bs_vscode then startchar + 1 else startchar in 
    
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  if file = "//toplevel//" then begin
    if highlight_locations ppf [loc] then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "%s@{<loc>%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar;
    fprintf ppf "@}"
  end
;;

let print ppf loc =
  setup_colors ();
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else fprintf ppf "@{<loc>%a@}%s@." print_loc loc msg_colon
;;

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf () =
  setup_colors ();
  fprintf ppf "@{<error>%s@}:" error_prefix;
  ()
;;

let print_error ppf loc =
  print ppf loc;
  print_error_prefix ppf ()
;;

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  if Warnings.is_active w then begin
    setup_colors ();
    print ppf loc;
    fprintf ppf "@{<warning>%s@} %a@." warning_prefix Warnings.print w
  end
;;

let warning_printer = ref default_warning_printer ;;

let print_warning loc ppf w =
  print_updating_num_loc_lines ppf (!warning_printer loc) w
;;

let formatter_for_warnings = ref err_formatter;;
let prerr_warning loc w = print_warning loc !formatter_for_warnings w;;

let echo_eof () =
  print_newline ();
  incr num_loc_lines

type 'a loc = {
  txt : 'a;
  loc : t;
}

let mkloc txt loc = { txt ; loc }
let mknoloc txt = mkloc txt none


type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

let pp_ksprintf ?before k fmt =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  begin match before with
    | None -> ()
    | Some f -> f ppf
  end;
  kfprintf
    (fun _ ->
      pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

(* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted *)
let print_phanton_error_prefix ppf =
  Format.pp_print_as ppf (String.length error_prefix + 2 (* ": " *)) ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

let error_of_exn exn =
  let rec loop = function
    | [] -> None
    | f :: rest ->
        match f exn with
        | Some _ as r -> r
        | None -> loop rest
  in
  loop !error_of_exn

let rec default_error_reporter ppf ({loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" then
      let rec collect_locs locs {loc; sub; if_highlight; _} =
        List.fold_left collect_locs (loc :: locs) sub
      in
      let locs = collect_locs [] err in
      highlight_locations ppf locs
    else
      false
  in
  if highlighted then
    Format.pp_print_string ppf if_highlight
  else begin
    fprintf ppf "%a%a %s" print loc print_error_prefix () msg;
    List.iter (Format.fprintf ppf "@\n@[<2>%a@]" default_error_reporter) sub
  end

let error_reporter = ref default_error_reporter

let report_error ppf err =
  print_updating_num_loc_lines ppf !error_reporter err
;;

let error_of_printer loc print x =
  errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn
    (function
      | Sys_error msg ->
          Some (errorf ~loc:(in_file !input_name)
                "I/O error: %s" msg)
      | Warnings.Errors n ->
          Some
            (errorf ~loc:(in_file !input_name)
             "Some fatal warnings were triggered (%d occurrences)" n)
      | _ ->
          None
    )


let rec report_exception_rec n ppf exn =
  try match error_of_exn exn with
  | Some err ->
      fprintf ppf "@[%a@]@." report_error err
  | None -> raise exn
  with exn when n > 0 ->
    report_exception_rec (n-1) ppf exn

let report_exception ppf exn = report_exception_rec 5 ppf exn


exception Error of error

let () =
  register_error_of_exn
    (function
      | Error e -> Some e
      | _ -> None
    )

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> raise (Error ({loc; msg; sub; if_highlight})))

end
(** Interface as module  *)
module Asttypes
= struct
#1 "asttypes.mli"
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

(* Auxiliary a.s.t. types used by parsetree and typedtree. *)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}


type variance =
  | Covariant
  | Contravariant
  | Invariant

end
module Longident : sig 
#1 "longident.mli"
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

(* Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val last: t -> string
val parse: string -> t

end = struct
#1 "longident.ml"
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

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [] -> Lident ""  (* should not happen, but don't put assert false
                        so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl

end
(** Interface as module  *)
module Parsetree
= struct
#1 "parsetree.mli"
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

(** Abstract syntax tree produced by parsing *)

open Asttypes

(** {2 Extension points} *)

type attribute = string loc * payload
       (* [@id ARG]
          [@@id ARG]

          Metadata containers passed around within the AST.
          The compiler ignores unknown attributes.
       *)

and extension = string loc * payload
      (* [%id ARG]
         [%%id ARG]

         Sub-language placeholder -- rejected by the typechecker.
      *)

and attributes = attribute list

and payload =
  | PStr of structure
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option  (* ? P  or  ? P when E *)

(** {2 Core language} *)

(* Type expressions *)

and core_type =
    {
     ptyp_desc: core_type_desc;
     ptyp_loc: Location.t;
     ptyp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and core_type_desc =
  | Ptyp_any
        (*  _ *)
  | Ptyp_var of string
        (* 'a *)
  | Ptyp_arrow of label * core_type * core_type
        (* T1 -> T2       (label = "")
           ~l:T1 -> T2    (label = "l")
           ?l:T1 -> T2    (label = "?l")
         *)
  | Ptyp_tuple of core_type list
        (* T1 * ... * Tn

           Invariant: n >= 2
        *)
  | Ptyp_constr of Longident.t loc * core_type list
        (* tconstr
           T tconstr
           (T1, ..., Tn) tconstr
         *)
  | Ptyp_object of (string * attributes * core_type) list * closed_flag
        (* < l1:T1; ...; ln:Tn >     (flag = Closed)
           < l1:T1; ...; ln:Tn; .. > (flag = Open)
         *)
  | Ptyp_class of Longident.t loc * core_type list
        (* #tconstr
           T #tconstr
           (T1, ..., Tn) #tconstr
         *)
  | Ptyp_alias of core_type * string
        (* T as 'a *)
  | Ptyp_variant of row_field list * closed_flag * label list option
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
  | Ptyp_poly of string list * core_type
        (* 'a1 ... 'an. T

           Can only appear in the following context:

           - As the core_type of a Ppat_constraint node corresponding
             to a constraint on a let-binding: let x : 'a1 ... 'an. T
             = e ...

           - Under Cfk_virtual for methods (not values).

           - As the core_type of a Pctf_method node.

           - As the core_type of a Pexp_poly node.

           - As the pld_type field of a label_declaration.

           - As a core_type of a Ptyp_object node.
         *)

  | Ptyp_package of package_type
        (* (module S) *)
  | Ptyp_extension of extension
        (* [%id] *)

and package_type = Longident.t loc * (Longident.t loc * core_type) list
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

and row_field =
  | Rtag of label * attributes * bool * core_type list
        (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

          - The 2nd field is true if the tag contains a
            constant (empty) constructor.
          - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)

          - TODO: switch to a record representation, and keep location
        *)
  | Rinherit of core_type
        (* [ T ] *)

(* Patterns *)

and pattern =
    {
     ppat_desc: pattern_desc;
     ppat_loc: Location.t;
     ppat_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and pattern_desc =
  | Ppat_any
        (* _ *)
  | Ppat_var of string loc
        (* x *)
  | Ppat_alias of pattern * string loc
        (* P as 'a *)
  | Ppat_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval of constant * constant
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple of pattern list
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct of Longident.t loc * pattern option
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant of label * pattern option
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array of pattern list
        (* [| P1; ...; Pn |] *)
  | Ppat_or of pattern * pattern
        (* P1 | P2 *)
  | Ppat_constraint of pattern * core_type
        (* (P : T) *)
  | Ppat_type of Longident.t loc
        (* #tconst *)
  | Ppat_lazy of pattern
        (* lazy P *)
  | Ppat_unpack of string loc
        (* (module P)
           Note: (module P : S) is represented as
           Ppat_constraint(Ppat_unpack, Ptyp_package)
         *)
  | Ppat_exception of pattern
        (* exception P *)
  | Ppat_extension of extension
        (* [%id] *)

(* Value expressions *)

and expression =
    {
     pexp_desc: expression_desc;
     pexp_loc: Location.t;
     pexp_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and expression_desc =
  | Pexp_ident of Longident.t loc
        (* x
           M.x
         *)
  | Pexp_constant of constant
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Pexp_let of rec_flag * value_binding list * expression
        (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
         *)
  | Pexp_function of case list
        (* function P1 -> E1 | ... | Pn -> En *)
  | Pexp_fun of label * expression option * pattern * expression
        (* fun P -> E1                          (lab = "", None)
           fun ~l:P -> E1                       (lab = "l", None)
           fun ?l:P -> E1                       (lab = "?l", None)
           fun ?l:(P = E0) -> E1                (lab = "?l", Some E0)

           Notes:
           - If E0 is provided, lab must start with '?'.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Pexp_apply of expression * (label * expression) list
        (* E0 ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pexp_match of expression * case list
        (* match E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_try of expression * case list
        (* try E0 with P1 -> E1 | ... | Pn -> En *)
  | Pexp_tuple of expression list
        (* (E1, ..., En)

           Invariant: n >= 2
        *)
  | Pexp_construct of Longident.t loc * expression option
        (* C                None
           C E              Some E
           C (E1, ..., En)  Some (Pexp_tuple[E1;...;En])
        *)
  | Pexp_variant of label * expression option
        (* `A             (None)
           `A E           (Some E)
         *)
  | Pexp_record of (Longident.t loc * expression) list * expression option
        (* { l1=P1; ...; ln=Pn }     (None)
           { E0 with l1=P1; ...; ln=Pn }   (Some E0)

           Invariant: n > 0
         *)
  | Pexp_field of expression * Longident.t loc
        (* E.l *)
  | Pexp_setfield of expression * Longident.t loc * expression
        (* E1.l <- E2 *)
  | Pexp_array of expression list
        (* [| E1; ...; En |] *)
  | Pexp_ifthenelse of expression * expression * expression option
        (* if E1 then E2 else E3 *)
  | Pexp_sequence of expression * expression
        (* E1; E2 *)
  | Pexp_while of expression * expression
        (* while E1 do E2 done *)
  | Pexp_for of
      pattern *  expression * expression * direction_flag * expression
        (* for i = E1 to E2 do E3 done      (flag = Upto)
           for i = E1 downto E2 do E3 done  (flag = Downto)
         *)
  | Pexp_constraint of expression * core_type
        (* (E : T) *)
  | Pexp_coerce of expression * core_type option * core_type
        (* (E :> T)        (None, T)
           (E : T0 :> T)   (Some T0, T)
         *)
  | Pexp_send of expression * string
        (*  E # m *)
  | Pexp_new of Longident.t loc
        (* new M.c *)
  | Pexp_setinstvar of string loc * expression
        (* x <- 2 *)
  | Pexp_override of (string loc * expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
        (* let module M = ME in E *)
  | Pexp_assert of expression
        (* assert E
           Note: "assert false" is treated in a special way by the
           type-checker. *)
  | Pexp_lazy of expression
        (* lazy E *)
  | Pexp_poly of expression * core_type option
        (* Used for method bodies.

           Can only be used as the expression under Cfk_concrete
           for methods (not values). *)
  | Pexp_object of class_structure
        (* object ... end *)
  | Pexp_newtype of string * expression
        (* fun (type t) -> E *)
  | Pexp_pack of module_expr
        (* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of override_flag * Longident.t loc * expression
        (* let open M in E
           let! open M in E
        *)
  | Pexp_extension of extension
        (* [%id] *)

and case =   (* (P -> E) or (P when E0 -> E) *)
    {
     pc_lhs: pattern;
     pc_guard: expression option;
     pc_rhs: expression;
    }

(* Value descriptions *)

and value_description =
    {
     pval_name: string loc;
     pval_type: core_type;
     pval_prim: string list;
     pval_attributes: attributes;  (* ... [@@id1] [@@id2] *)
     pval_loc: Location.t;
    }

(*
  val x: T                            (prim = [])
  external x: T = "s1" ... "sn"       (prim = ["s1";..."sn"])

  Note: when used under Pstr_primitive, prim cannot be empty
*)

(* Type declarations *)

and type_declaration =
    {
     ptype_name: string loc;
     ptype_params: (core_type * variance) list;
           (* ('a1,...'an) t; None represents  _*)
     ptype_cstrs: (core_type * core_type * Location.t) list;
           (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
     ptype_kind: type_kind;
     ptype_private: private_flag;   (* = private ... *)
     ptype_manifest: core_type option;  (* = T *)
     ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
     ptype_loc: Location.t;
    }

(*
  type t                     (abstract, no manifest)
  type t = T0                (abstract, manifest=T0)
  type t = C of T | ...      (variant,  no manifest)
  type t = T0 = C of T | ... (variant,  manifest=T0)
  type t = {l: T; ...}       (record,   no manifest)
  type t = T0 = {l : T; ...} (record,   manifest=T0)
  type t = ..                (open,     no manifest)
*)

and type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
        (* Invariant: non-empty list *)
  | Ptype_record of label_declaration list
        (* Invariant: non-empty list *)
  | Ptype_open

and label_declaration =
    {
     pld_name: string loc;
     pld_mutable: mutable_flag;
     pld_type: core_type;
     pld_loc: Location.t;
     pld_attributes: attributes; (* l [@id1] [@id2] : T *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_args: core_type list;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }
(*
  | C of T1 * ... * Tn     (res = None)
  | C: T0                  (args = [], res = Some T0)
  | C: T1 * ... * Tn -> T0 (res = Some T0)
*)

and type_extension =
    {
     ptyext_path: Longident.t loc;
     ptyext_params: (core_type * variance) list;
     ptyext_constructors: extension_constructor list;
     ptyext_private: private_flag;
     ptyext_attributes: attributes;   (* ... [@@id1] [@@id2] *)
    }
(*
  type t += ...
*)

and extension_constructor =
    {
     pext_name: string loc;
     pext_kind : extension_constructor_kind;
     pext_loc : Location.t;
     pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and extension_constructor_kind =
    Pext_decl of core_type list * core_type option
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t loc
      (*
         | C = D
       *)

(** {2 Class language} *)

(* Type expressions for the class language *)

and class_type =
    {
     pcty_desc: class_type_desc;
     pcty_loc: Location.t;
     pcty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_type_desc =
  | Pcty_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcty_signature of class_signature
        (* object ... end *)
  | Pcty_arrow of label * core_type * class_type
        (* T -> CT       (label = "")
           ~l:T -> CT    (label = "l")
           ?l:T -> CT    (label = "?l")
         *)
  | Pcty_extension of extension
        (* [%id] *)

and class_signature =
    {
     pcsig_self: core_type;
     pcsig_fields: class_type_field list;
    }
(* object('selfpat) ... end
   object ... end             (self = Ptyp_any)
 *)

and class_type_field =
    {
     pctf_desc: class_type_field_desc;
     pctf_loc: Location.t;
     pctf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_type_field_desc =
  | Pctf_inherit of class_type
        (* inherit CT *)
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (string * private_flag * virtual_flag * core_type)
        (* method x: T

           Note: T can be a Ptyp_poly.
         *)
  | Pctf_constraint  of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pctf_attribute of attribute
        (* [@@@id] *)
  | Pctf_extension of extension
        (* [%%id] *)

and 'a class_infos =
    {
     pci_virt: virtual_flag;
     pci_params: (core_type * variance) list;
     pci_name: string loc;
     pci_expr: 'a;
     pci_loc: Location.t;
     pci_attributes: attributes;  (* ... [@@id1] [@@id2] *)
    }
(* class c = ...
   class ['a1,...,'an] c = ...
   class virtual c = ...

   Also used for "class type" declaration.
*)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
    {
     pcl_desc: class_expr_desc;
     pcl_loc: Location.t;
     pcl_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and class_expr_desc =
  | Pcl_constr of Longident.t loc * core_type list
        (* c
           ['a1, ..., 'an] c *)
  | Pcl_structure of class_structure
        (* object ... end *)
  | Pcl_fun of label * expression option * pattern * class_expr
        (* fun P -> CE                          (lab = "", None)
           fun ~l:P -> CE                       (lab = "l", None)
           fun ?l:P -> CE                       (lab = "?l", None)
           fun ?l:(P = E0) -> CE                (lab = "?l", Some E0)
         *)
  | Pcl_apply of class_expr * (label * expression) list
        (* CE ~l1:E1 ... ~ln:En
           li can be empty (non labeled argument) or start with '?'
           (optional argument).

           Invariant: n > 0
         *)
  | Pcl_let of rec_flag * value_binding list * class_expr
        (* let P1 = E1 and ... and Pn = EN in CE      (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN in CE  (flag = Recursive)
         *)
  | Pcl_constraint of class_expr * class_type
        (* (CE : CT) *)
  | Pcl_extension of extension
        (* [%id] *)

and class_structure =
    {
     pcstr_self: pattern;
     pcstr_fields: class_field list;
    }
(* object(selfpat) ... end
   object ... end           (self = Ppat_any)
 *)

and class_field =
    {
     pcf_desc: class_field_desc;
     pcf_loc: Location.t;
     pcf_attributes: attributes; (* ... [@@id1] [@@id2] *)
    }

and class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string option
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val of (string loc * mutable_flag * class_field_kind)
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method of (string loc * private_flag * class_field_kind)
        (* method x = E            (E can be a Pexp_poly)
           method virtual x: T     (T can be a Ptyp_poly)
         *)
  | Pcf_constraint of (core_type * core_type)
        (* constraint T1 = T2 *)
  | Pcf_initializer of expression
        (* initializer E *)
  | Pcf_attribute of attribute
        (* [@@@id] *)
  | Pcf_extension of extension
        (* [%%id] *)

and class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

(** {2 Module language} *)

(* Type expressions for the module language *)

and module_type =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     pmty_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_type_desc =
  | Pmty_ident of Longident.t loc
        (* S *)
  | Pmty_signature of signature
        (* sig ... end *)
  | Pmty_functor of string loc * module_type option * module_type
        (* functor(X : MT1) -> MT2 *)
  | Pmty_with of module_type * with_constraint list
        (* MT with ... *)
  | Pmty_typeof of module_expr
        (* module type of ME *)
  | Pmty_extension of extension
        (* [%id] *)
  | Pmty_alias of Longident.t loc
        (* (module M) *)

and signature = signature_item list

and signature_item =
    {
     psig_desc: signature_item_desc;
     psig_loc: Location.t;
    }

and signature_item_desc =
  | Psig_value of value_description
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
  | Psig_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Psig_typext of type_extension
        (* type t1 += ... *)
  | Psig_exception of extension_constructor
        (* exception C of T *)
  | Psig_module of module_declaration
        (* module X : MT *)
  | Psig_recmodule of module_declaration list
        (* module rec X1 : MT1 and ... and Xn : MTn *)
  | Psig_modtype of module_type_declaration
        (* module type S = MT
           module type S *)
  | Psig_open of open_description
        (* open X *)
  | Psig_include of include_description
        (* include MT *)
  | Psig_class of class_description list
        (* class c1 : ... and ... and cn : ... *)
  | Psig_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Psig_attribute of attribute
        (* [@@@id] *)
  | Psig_extension of extension * attributes
        (* [%%id] *)

and module_declaration =
    {
     pmd_name: string loc;
     pmd_type: module_type;
     pmd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmd_loc: Location.t;
    }
(* S : MT *)

and module_type_declaration =
    {
     pmtd_name: string loc;
     pmtd_type: module_type option;
     pmtd_attributes: attributes; (* ... [@@id1] [@@id2] *)
     pmtd_loc: Location.t;
    }
(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)

and open_description =
    {
     popen_lid: Longident.t loc;
     popen_override: override_flag;
     popen_loc: Location.t;
     popen_attributes: attributes;
    }
(* open! X - popen_override = Override (silences the 'used identifier
                              shadowing' warning)
   open  X - popen_override = Fresh
 *)

and 'a include_infos =
    {
     pincl_mod: 'a;
     pincl_loc: Location.t;
     pincl_attributes: attributes;
    }

and include_description = module_type include_infos
(* include MT *)

and include_declaration = module_expr include_infos
(* include ME *)

and with_constraint =
  | Pwith_type of Longident.t loc * type_declaration
        (* with type X.t = ...

           Note: the last component of the longident must match
           the name of the type_declaration. *)
  | Pwith_module of Longident.t loc * Longident.t loc
        (* with module X.Y = Z *)
  | Pwith_typesubst of type_declaration
        (* with type t := ... *)
  | Pwith_modsubst of string loc * Longident.t loc
        (* with module X := Z *)

(* Value expressions for the module language *)

and module_expr =
    {
     pmod_desc: module_expr_desc;
     pmod_loc: Location.t;
     pmod_attributes: attributes; (* ... [@id1] [@id2] *)
    }

and module_expr_desc =
  | Pmod_ident of Longident.t loc
        (* X *)
  | Pmod_structure of structure
        (* struct ... end *)
  | Pmod_functor of string loc * module_type option * module_expr
        (* functor(X : MT1) -> ME *)
  | Pmod_apply of module_expr * module_expr
        (* ME1(ME2) *)
  | Pmod_constraint of module_expr * module_type
        (* (ME : MT) *)
  | Pmod_unpack of expression
        (* (val E) *)
  | Pmod_extension of extension
        (* [%id] *)

and structure = structure_item list

and structure_item =
    {
     pstr_desc: structure_item_desc;
     pstr_loc: Location.t;
    }

and structure_item_desc =
  | Pstr_eval of expression * attributes
        (* E *)
  | Pstr_value of rec_flag * value_binding list
        (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
           let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
         *)
  | Pstr_primitive of value_description
        (* external x: T = "s1" ... "sn" *)
  | Pstr_type of type_declaration list
        (* type t1 = ... and ... and tn = ... *)
  | Pstr_typext of type_extension
        (* type t1 += ... *)
  | Pstr_exception of extension_constructor
        (* exception C of T
           exception C = M.X *)
  | Pstr_module of module_binding
        (* module X = ME *)
  | Pstr_recmodule of module_binding list
        (* module rec X1 = ME1 and ... and Xn = MEn *)
  | Pstr_modtype of module_type_declaration
        (* module type S = MT *)
  | Pstr_open of open_description
        (* open X *)
  | Pstr_class of class_declaration list
        (* class c1 = ... and ... and cn = ... *)
  | Pstr_class_type of class_type_declaration list
        (* class type ct1 = ... and ... and ctn = ... *)
  | Pstr_include of include_declaration
        (* include ME *)
  | Pstr_attribute of attribute
        (* [@@@id] *)
  | Pstr_extension of extension * attributes
        (* [%%id] *)

and value_binding =
  {
    pvb_pat: pattern;
    pvb_expr: expression;
    pvb_attributes: attributes;
    pvb_loc: Location.t;
  }

and module_binding =
    {
     pmb_name: string loc;
     pmb_expr: module_expr;
     pmb_attributes: attributes;
     pmb_loc: Location.t;
    }
(* X = ME *)

(** {2 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument
     (* #use, #load ... *)

and directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of int
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

end
module Docstrings : sig 
#1 "docstrings.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** (Re)Initialise all docstring state *)
val init : unit -> unit

(** Emit warnings for unattached and ambiguous docstrings *)
val warn_bad_docstrings : unit -> unit

(** {3 Docstrings} *)

(** Documentation comments *)
type docstring

(** Create a docstring *)
val docstring : string -> Location.t -> docstring

(** Get the text of a docstring *)
val docstring_body : docstring -> string

(** Get the location of a docstring *)
val docstring_loc : docstring -> Location.t

(** {3 Set functions}

   These functions are used by the lexer to associate docstrings to
   the locations of tokens. *)

(** Docstrings immediately preceding a token *)
val set_pre_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following a token *)
val set_post_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings not immediately adjacent to a token *)
val set_floating_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately following the token which precedes this one *)
val set_pre_extra_docstrings : Lexing.position -> docstring list -> unit

(** Docstrings immediately preceding the token which follows this one *)
val set_post_extra_docstrings : Lexing.position -> docstring list -> unit

(** {3 Items}

    The {!docs} type represents documentation attached to an item. *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

val empty_docs : docs

val docs_attr : docstring -> Parsetree.attribute

(** Convert item documentation to attributes and add them to an
    attribute list *)
val add_docs_attrs : docs -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the item documentation for the current symbol. This also
    marks this documentation (for ambiguity warnings). *)
val symbol_docs : unit -> docs
val symbol_docs_lazy : unit -> docs Lazy.t

(** Fetch the item documentation for the symbols between two
    positions. This also marks this documentation (for ambiguity
    warnings). *)
val rhs_docs : int -> int -> docs
val rhs_docs_lazy : int -> int -> docs Lazy.t

(** Mark the item documentation for the current symbol (for ambiguity
    warnings). *)
val mark_symbol_docs : unit -> unit

(** Mark as associated the item documentation for the symbols between
    two positions (for ambiguity warnings) *)
val mark_rhs_docs : int -> int -> unit

(** {3 Fields and constructors}

    The {!info} type represents documentation attached to a field or
    constructor. *)

type info = docstring option

val empty_info : info

val info_attr : docstring -> Parsetree.attribute

(** Convert field info to attributes and add them to an
    attribute list *)
val add_info_attrs : info -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the field info for the current symbol. *)
val symbol_info : unit -> info

(** Fetch the field info following the symbol at a given position. *)
val rhs_info : int -> info

(** {3 Unattached comments}

    The {!text} type represents documentation which is not attached to
    anything. *)

type text = docstring list

val empty_text : text

val text_attr : docstring -> Parsetree.attribute

(** Convert text to attributes and add them to an attribute list *)
val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the text preceding the current symbol. *)
val symbol_text : unit -> text
val symbol_text_lazy : unit -> text Lazy.t

(** Fetch the text preceding the symbol at the given position. *)
val rhs_text : int -> text
val rhs_text_lazy : int -> text Lazy.t

(** {3 Extra text}

    There may be additional text attached to the delimiters of a block
    (e.g. [struct] and [end]). This is fetched by the following
    functions, which are applied to the contents of the block rather
    than the delimiters. *)

(** Fetch additional text preceding the current symbol *)
val symbol_pre_extra_text : unit -> text

(** Fetch additional text following the current symbol *)
val symbol_post_extra_text : unit -> text

(** Fetch additional text preceding the symbol at the given position *)
val rhs_pre_extra_text : int -> text

(** Fetch additional text following the symbol at the given position *)
val rhs_post_extra_text : int -> text

end = struct
#1 "docstrings.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                              Leo White                              *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Location

(* Docstrings *)

(* A docstring is "attached" if it has been inserted in the AST. This
   is used for generating unexpected docstring warnings. *)
type ds_attached =
  | Unattached   (* Not yet attached anything.*)
  | Info         (* Attached to a field or constructor. *)
  | Docs         (* Attached to an item or as floating text. *)

(* A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. *)
type ds_associated =
  | Zero             (* Not associated with an item *)
  | One              (* Associated with one item *)
  | Many             (* Associated with multiple items (ambiguity) *)

type docstring =
  { ds_body: string;
    ds_loc: Location.t;
    mutable ds_attached: ds_attached;
    mutable ds_associated: ds_associated; }

(* List of docstrings *)

let docstrings : docstring list ref = ref []

(* Warn for unused and ambiguous docstrings *)

let warn_bad_docstrings () =
  if Warnings.is_active (Warnings.Bad_docstring true) then begin
    List.iter
      (fun ds ->
         match ds.ds_attached with
         | Info -> ()
         | Unattached ->
           prerr_warning ds.ds_loc (Warnings.Bad_docstring true)
         | Docs ->
             match ds.ds_associated with
             | Zero | One -> ()
             | Many ->
               prerr_warning ds.ds_loc (Warnings.Bad_docstring false))
      (List.rev !docstrings)
end

(* Docstring constructors and descturctors *)

let docstring body loc =
  let ds =
    { ds_body = body;
      ds_loc = loc;
      ds_attached = Unattached;
      ds_associated = Zero; }
  in
  docstrings := ds :: !docstrings;
  ds

let docstring_body ds = ds.ds_body

let docstring_loc ds = ds.ds_loc

(* Docstrings attached to items *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

let empty_docs = { docs_pre = None; docs_post = None }

let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

let docs_attr ds =
  let open Asttypes in
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (doc_loc, PStr [item])

let add_docs_attrs docs attrs =
  let attrs =
    match docs.docs_pre with
    | None -> attrs
    | Some ds -> docs_attr ds :: attrs
  in
  let attrs =
    match docs.docs_post with
    | None -> attrs
    | Some ds -> attrs @ [docs_attr ds]
  in
  attrs

(* Docstrings attached to consturctors or fields *)

type info = docstring option

let empty_info = None

let info_attr = docs_attr

let add_info_attrs info attrs =
  let attrs =
    match info with
    | None -> attrs
    | Some ds -> attrs @ [info_attr ds]
  in
  attrs

(* Docstrings not attached to a specifc item *)

type text = docstring list

let empty_text = []

let text_loc = {txt = "ocaml.text"; loc = Location.none}

let text_attr ds =
  let open Asttypes in
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (text_loc, PStr [item])

let add_text_attrs dsl attrs =
  (List.map text_attr dsl) @ attrs

(* Find the first non-info docstring in a list, attach it and return it *)
let get_docstring ~info dsl =
  let rec loop = function
    | [] -> None
    | {ds_attached = Info; _} :: rest -> loop rest
    | ds :: rest ->
        ds.ds_attached <- if info then Info else Docs;
        Some ds
  in
  loop dsl

(* Find all the non-info docstrings in a list, attach them and return them *)
let get_docstrings dsl =
  let rec loop acc = function
    | [] -> List.rev acc
    | {ds_attached = Info; _} :: rest -> loop acc rest
    | ds :: rest ->
        ds.ds_attached <- Docs;
        loop (ds :: acc) rest
  in
    loop [] dsl

(* "Associate" all the docstrings in a list *)
let associate_docstrings dsl =
  List.iter
    (fun ds ->
       match ds.ds_associated with
       | Zero -> ds.ds_associated <- One
       | (One | Many) -> ds.ds_associated <- Many)
    dsl

(* Map from positions to pre docstrings *)

let pre_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_table pos dsl

let get_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_pre_docs pos =
  try
    let dsl = Hashtbl.find pre_table pos in
      associate_docstrings dsl
  with Not_found -> ()

(* Map from positions to post docstrings *)

let post_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_table pos dsl

let get_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl;
      get_docstring ~info:false dsl
  with Not_found -> None

let mark_post_docs pos =
  try
    let dsl = Hashtbl.find post_table pos in
      associate_docstrings dsl
  with Not_found -> ()

let get_info pos =
  try
    let dsl = Hashtbl.find post_table pos in
      get_docstring ~info:true dsl
  with Not_found -> None

(* Map from positions to floating docstrings *)

let floating_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_floating_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add floating_table pos dsl

let get_text pos =
  try
    let dsl = Hashtbl.find floating_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Maps from positions to extra docstrings *)

let pre_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_pre_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add pre_extra_table pos dsl

let get_pre_extra_text pos =
  try
    let dsl = Hashtbl.find pre_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

let post_extra_table : (Lexing.position, docstring list) Hashtbl.t =
  Hashtbl.create 50

let set_post_extra_docstrings pos dsl =
  if dsl <> [] then Hashtbl.add post_extra_table pos dsl

let get_post_extra_text pos =
  try
    let dsl = Hashtbl.find post_extra_table pos in
      get_docstrings dsl
  with Not_found -> []

(* Docstrings from parser actions *)

let symbol_docs () =
  { docs_pre = get_pre_docs (Parsing.symbol_start_pos ());
    docs_post = get_post_docs (Parsing.symbol_end_pos ()); }

let symbol_docs_lazy () =
  let p1 = Parsing.symbol_start_pos () in
  let p2 = Parsing.symbol_end_pos () in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let rhs_docs pos1 pos2 =
  { docs_pre = get_pre_docs (Parsing.rhs_start_pos pos1);
    docs_post = get_post_docs (Parsing.rhs_end_pos pos2); }

let rhs_docs_lazy pos1 pos2 =
  let p1 = Parsing.rhs_start_pos pos1 in
  let p2 = Parsing.rhs_end_pos pos2 in
    lazy { docs_pre = get_pre_docs p1;
           docs_post = get_post_docs p2; }

let mark_symbol_docs () =
  mark_pre_docs (Parsing.symbol_start_pos ());
  mark_post_docs (Parsing.symbol_end_pos ())

let mark_rhs_docs pos1 pos2 =
  mark_pre_docs (Parsing.rhs_start_pos pos1);
  mark_post_docs (Parsing.rhs_end_pos pos2)

let symbol_info () =
  get_info (Parsing.symbol_end_pos ())

let rhs_info pos =
  get_info (Parsing.rhs_end_pos pos)

let symbol_text () =
  get_text (Parsing.symbol_start_pos ())

let symbol_text_lazy () =
  let pos = Parsing.symbol_start_pos () in
    lazy (get_text pos)

let rhs_text pos =
  get_text (Parsing.rhs_start_pos pos)

let rhs_text_lazy pos =
  let pos = Parsing.rhs_start_pos pos in
    lazy (get_text pos)

let symbol_pre_extra_text () =
  get_pre_extra_text (Parsing.symbol_start_pos ())

let symbol_post_extra_text () =
  get_post_extra_text (Parsing.symbol_end_pos ())

let rhs_pre_extra_text pos =
  get_pre_extra_text (Parsing.rhs_start_pos pos)

let rhs_post_extra_text pos =
  get_post_extra_text (Parsing.rhs_end_pos pos)


(* (Re)Initialise all comment state *)

let init () =
  docstrings := [];
  Hashtbl.reset pre_table;
  Hashtbl.reset post_table;
  Hashtbl.reset floating_table;
  Hashtbl.reset pre_extra_table;
  Hashtbl.reset post_extra_table




end
module Ast_helper : sig 
#1 "ast_helper.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Parsetree
open Asttypes
open Docstrings

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

(** {2 Default locations} *)

val default_loc: loc ref
    (** Default value for all optional location arguments. *)

val with_default_loc: loc -> (unit -> 'a) -> 'a
    (** Set the [default_loc] within the scope of the execution
        of the provided function. *)

(** {2 Core language} *)

(** Type expressions *)
module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> core_type
               -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val object_: ?loc:loc -> ?attrs:attrs ->
                  (string * attributes * core_type) list -> closed_flag ->
                  core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
                 -> label list option -> core_type
    val poly: ?loc:loc -> ?attrs:attrs -> string list -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
                 -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type
  end

(** Patterns *)
module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
                -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

(** Expressions *)
module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
              -> expression -> expression
    val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern
              -> expression -> expression
    val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
               -> (label * expression) list -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
                -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
                   -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
                 -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
                -> expression option -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
                  -> expression
    val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
                    -> expression option -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
                  -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
                -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
              -> direction_flag -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
                -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
                     -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> string -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
                  -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression
                   -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> string -> expression -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression

    val case: pattern -> ?guard:expression -> expression -> case
  end

(** Value declarations *)
module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list -> str -> core_type -> value_description
  end

(** Type declarations *)
module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * variance) list -> ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?args:core_type list -> ?res:core_type -> str -> constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> str -> core_type -> label_declaration
  end

(** Type extensions *)
module Te:
  sig
    val mk: ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * variance) list -> ?priv:private_flag ->
      lid -> extension_constructor list -> type_extension

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?args:core_type list -> ?res:core_type -> str -> extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

(** {2 Module language} *)

(** Module type expressions *)
module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      str -> module_type option -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type -> with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

(** Module expressions *)
module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      str -> module_type option -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr -> module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type -> module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
  end

(** Signature items *)
module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> extension_constructor -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

(** Structure items *)
module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> extension_constructor -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_description -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

(** Module declarations *)
module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_type -> module_declaration
  end

(** Module type declarations *)
module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

(** Module bindings *)
module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> module_expr -> module_binding
  end

(* Opens *)
module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag -> lid -> open_description
  end

(* Includes *)
module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

(** Value bindings *)

module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      pattern -> expression -> value_binding
  end


(** {2 Class language} *)

(** Class type expressions *)
module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> label -> core_type -> class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
  end

(** Class type fields *)
module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> string -> mutable_flag -> virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> string -> private_flag -> virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

(** Class expressions *)
module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> label -> expression option -> pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr -> (label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list -> class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type -> class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
  end

(** Class fields *)
module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc -> class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr -> string option -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag -> class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag -> class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type -> class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag -> expression -> class_field_kind

  end

(** Classes *)
module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag -> ?params:(core_type * variance) list ->
      str -> 'a -> 'a class_infos
  end

(** Class signatures *)
module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

(** Class structures *)
module Cstr:
  sig
    val mk: pattern -> class_field list -> class_structure
  end

end = struct
#1 "ast_helper.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree
open Docstrings

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

let default_loc = ref Location.none

let with_default_loc l f =
  let old = !default_loc in
  default_loc := l;
  try let r = f () in default_loc := old; r
  with exn -> default_loc := old; raise exn

module Typ = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ptyp_desc = d; ptyp_loc = loc; ptyp_attributes = attrs}
  let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
  let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
  let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
  let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
  let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
  let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

  let force_poly t =
    match t.ptyp_desc with
    | Ptyp_poly _ -> t
    | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)
end

module Pat = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = attrs}
  let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

  let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
  let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
  let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
  let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
  let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
  let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
  let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
  let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
  let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
  let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
end

module Exp = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pexp_desc = d; pexp_loc = loc; pexp_attributes = attrs}
  let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
  let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
  let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
  let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
  let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
  let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
  let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
  let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
  let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
  let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
  let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
  let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
  let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
  let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
  let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
  let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
  let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
  let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
  let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
  let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
  let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
  let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
  let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
  let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
  let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
  let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
  let open_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_open (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)

  let case lhs ?guard rhs =
    {
     pc_lhs = lhs;
     pc_guard = guard;
     pc_rhs = rhs;
    }
end

module Mty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
  let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

  let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
  let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
  let functor_ ?loc ?attrs a b c = mk ?loc ?attrs (Pmty_functor (a, b, c))
  let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
  let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
end

module Mod = struct
let mk ?(loc = !default_loc) ?(attrs = []) d =
  {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
  let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

  let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
  let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
  let functor_ ?loc ?attrs arg arg_ty body =
    mk ?loc ?attrs (Pmod_functor (arg, arg_ty, body))
  let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
  let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
  let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
end

module Sig = struct
  let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

  let value ?loc a = mk ?loc (Psig_value a)
  let type_ ?loc a = mk ?loc (Psig_type a)
  let type_extension ?loc a = mk ?loc (Psig_typext a)
  let exception_ ?loc a = mk ?loc (Psig_exception a)
  let module_ ?loc a = mk ?loc (Psig_module a)
  let rec_module ?loc a = mk ?loc (Psig_recmodule a)
  let modtype ?loc a = mk ?loc (Psig_modtype a)
  let open_ ?loc a = mk ?loc (Psig_open a)
  let include_ ?loc a = mk ?loc (Psig_include a)
  let class_ ?loc a = mk ?loc (Psig_class a)
  let class_type ?loc a = mk ?loc (Psig_class_type a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Psig_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt
end

module Str = struct
  let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

  let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
  let value ?loc a b = mk ?loc (Pstr_value (a, b))
  let primitive ?loc a = mk ?loc (Pstr_primitive a)
  let type_ ?loc a = mk ?loc (Pstr_type a)
  let type_extension ?loc a = mk ?loc (Pstr_typext a)
  let exception_ ?loc a = mk ?loc (Pstr_exception a)
  let module_ ?loc a = mk ?loc (Pstr_module a)
  let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
  let modtype ?loc a = mk ?loc (Pstr_modtype a)
  let open_ ?loc a = mk ?loc (Pstr_open a)
  let class_ ?loc a = mk ?loc (Pstr_class a)
  let class_type ?loc a = mk ?loc (Pstr_class_type a)
  let include_ ?loc a = mk ?loc (Pstr_include a)
  let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
  let attribute ?loc a = mk ?loc (Pstr_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt
end

module Cl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcl_desc = d;
     pcl_loc = loc;
     pcl_attributes = attrs;
    }
  let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
  let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
  let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
  let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
end

module Cty = struct
  let mk ?(loc = !default_loc) ?(attrs = []) d =
    {
     pcty_desc = d;
     pcty_loc = loc;
     pcty_attributes = attrs;
    }
  let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

  let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
  let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
  let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
end

module Ctf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
           ?(docs = empty_docs) d =
    {
     pctf_desc = d;
     pctf_loc = loc;
     pctf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
  let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
  let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
  let attribute ?loc a = mk ?loc (Pctf_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt

  let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

end

module Cf = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
    {
     pcf_desc = d;
     pcf_loc = loc;
     pcf_attributes = add_docs_attrs docs attrs;
    }

  let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
  let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
  let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
  let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
  let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
  let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
  let attribute ?loc a = mk ?loc (Pcf_attribute a)
  let text txt =
    List.map
      (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
      txt

  let virtual_ ct = Cfk_virtual ct
  let concrete o e = Cfk_concrete (o, e)

  let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

end

module Val = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
    {
     pval_name = name;
     pval_type = typ;
     pval_attributes = add_docs_attrs docs attrs;
     pval_loc = loc;
     pval_prim = prim;
    }
end

module Md = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name typ =
    {
     pmd_name = name;
     pmd_type = typ;
     pmd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmd_loc = loc;
    }
end

module Mtd = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
    {
     pmtd_name = name;
     pmtd_type = typ;
     pmtd_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmtd_loc = loc;
    }
end

module Mb = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name expr =
    {
     pmb_name = name;
     pmb_expr = expr;
     pmb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pmb_loc = loc;
    }
end

module Opn = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) lid =
    {
     popen_lid = lid;
     popen_override = override;
     popen_loc = loc;
     popen_attributes = add_docs_attrs docs attrs;
    }
end

module Incl = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
    {
     pincl_mod = mexpr;
     pincl_loc = loc;
     pincl_attributes = add_docs_attrs docs attrs;
    }

end

module Vb = struct
  let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(text = []) pat expr =
    {
     pvb_pat = pat;
     pvb_expr = expr;
     pvb_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pvb_loc = loc;
    }
end

module Ci = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
    {
     pci_virt = virt;
     pci_params = params;
     pci_name = name;
     pci_expr = expr;
     pci_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     pci_loc = loc;
    }
end

module Type = struct
  let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
      ?(params = [])
      ?(cstrs = [])
      ?(kind = Ptype_abstract)
      ?(priv = Public)
      ?manifest
      name =
    {
     ptype_name = name;
     ptype_params = params;
     ptype_cstrs = cstrs;
     ptype_kind = kind;
     ptype_private = priv;
     ptype_manifest = manifest;
     ptype_attributes =
       add_text_attrs text (add_docs_attrs docs attrs);
     ptype_loc = loc;
    }

  let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(args = []) ?res name =
    {
     pcd_name = name;
     pcd_args = args;
     pcd_res = res;
     pcd_loc = loc;
     pcd_attributes = add_info_attrs info attrs;
    }

  let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
    {
     pld_name = name;
     pld_mutable = mut;
     pld_type = typ;
     pld_loc = loc;
     pld_attributes = add_info_attrs info attrs;
    }

end

(** Type extensions *)
module Te = struct
  let mk ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
    {
     ptyext_path = path;
     ptyext_params = params;
     ptyext_constructors = constructors;
     ptyext_private = priv;
     ptyext_attributes = add_docs_attrs docs attrs;
    }

  let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
    {
     pext_name = name;
     pext_kind = kind;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let decl ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) ?(args = []) ?res name =
    {
     pext_name = name;
     pext_kind = Pext_decl(args, res);
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

  let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
    {
     pext_name = name;
     pext_kind = Pext_rebind lid;
     pext_loc = loc;
     pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
    }

end

module Csig = struct
  let mk self fields =
    {
     pcsig_self = self;
     pcsig_fields = fields;
    }
end

module Cstr = struct
  let mk self fields =
    {
     pcstr_self = self;
     pcstr_fields = fields;
    }
end


end
module Ext_utf8 : sig 
#1 "ext_utf8.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type byte =
  | Single of int
  | Cont of int
  | Leading of int * int
  | Invalid


val classify : char -> byte 

val follow : 
    string -> 
    int -> 
    int -> 
    int ->
    int * int 


(** 
  return [-1] if failed 
*)
val next :  string -> remaining:int -> int -> int 


exception Invalid_utf8 of string 
 
 
val decode_utf8_string : string -> int list
end = struct
#1 "ext_utf8.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type byte =
  | Single of int
  | Cont of int
  | Leading of int * int
  | Invalid

(** [classify chr] returns the {!byte} corresponding to [chr] *)
let classify chr =
  let c = int_of_char chr in
  (* Classify byte according to leftmost 0 bit *)
  if c land 0b1000_0000 = 0 then Single c else
    (* c 0b0____*)
  if c land 0b0100_0000 = 0 then Cont (c land 0b0011_1111) else
    (* c 0b10___*)
  if c land 0b0010_0000 = 0 then Leading (1, c land 0b0001_1111) else
    (* c 0b110__*)
  if c land 0b0001_0000 = 0 then Leading (2, c land 0b0000_1111) else
    (* c 0b1110_ *)
  if c land 0b0000_1000 = 0 then Leading (3, c land 0b0000_0111) else
    (* c 0b1111_0___*)
  if c land 0b0000_0100 = 0 then Leading (4, c land 0b0000_0011) else
    (* c 0b1111_10__*)
  if c land 0b0000_0010 = 0 then Leading (5, c land 0b0000_0001)
  (* c 0b1111_110__ *)
  else Invalid

exception Invalid_utf8 of string 

(* when the first char is [Leading],
  TODO: need more error checking 
  when out of bond
 *)
let rec follow s n (c : int) offset = 
  if n = 0 then (c, offset)
  else 
    begin match classify s.[offset+1] with
      | Cont cc -> follow s (n-1) ((c lsl 6) lor (cc land 0x3f)) (offset+1)
      | _ -> raise (Invalid_utf8 "Continuation byte expected")
    end


let rec next s ~remaining  offset = 
  if remaining = 0 then offset 
  else 
    begin match classify s.[offset+1] with
      | Cont cc -> next s ~remaining:(remaining-1) (offset+1)
      | _ ->  -1 
      | exception _ ->  -1 (* it can happen when out of bound *)
    end




let decode_utf8_string s =
  let lst = ref [] in
  let add elem = lst := elem :: !lst in
  let rec  decode_utf8_cont s i s_len =
    if i = s_len  then ()
    else 
      begin 
        match classify s.[i] with
        | Single c -> 
          add c; decode_utf8_cont s (i+1) s_len
        | Cont _ -> raise (Invalid_utf8 "Unexpected continuation byte")
        | Leading (n, c) ->
          let (c', i') = follow s n c i in add c';
          decode_utf8_cont s (i' + 1) s_len
        | Invalid -> raise (Invalid_utf8 "Invalid byte")
      end
  in decode_utf8_cont s 0 (String.length s); 
  List.rev !lst


(** To decode {j||j} we need verify in the ast so that we have better error 
    location, then we do the decode later
*)  

let verify s loc = 
  assert false
end
module Ext_js_regex : sig 
#1 "ext_js_regex.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(* This is a module that checks if js regex is valid or not *)

val js_regex_checker : string -> bool
end = struct
#1 "ext_js_regex.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let check_from_end al =
  let rec aux l seen =
    match l with
    | [] -> false
    | (e::r) ->
      if e < 0 || e > 255 then false
      else (let c = Char.chr e in
            if c = '/' then true
            else (if List.exists (fun x -> x = c) seen then false (* flag should not be repeated *)
                  else (if c = 'i' || c = 'g' || c = 'm' || c = 'y' || c ='u' then aux r (c::seen) 
                        else false)))
  in aux al []

let js_regex_checker s =
  match Ext_utf8.decode_utf8_string s with 
  | [] -> false 
  | 47 (* [Char.code '/' = 47 ]*)::tail -> 
    check_from_end (List.rev tail)       
  | _ :: _ -> false 
  | exception Ext_utf8.Invalid_utf8 _ -> false 

end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







(** Port the {!Bytes.escaped} from trunk to make it not locale sensitive *)

val escaped : bytes -> bytes

end = struct
#1 "ext_bytes.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = Pervasives.ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

end
module Ext_string : sig 
#1 "ext_string.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Extension to the standard library [String] module, fixed some bugs like
    avoiding locale sensitivity *) 

(** default is false *)    
val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list


(** remove whitespace letters ('\t', '\n', ' ') on both side*)
val trim : string -> string 


(** default is false *)
val split : ?keep_empty:bool -> string -> char -> string list

(** split by space chars for quick scripting *)
val quick_split_by_ws : string -> string list 



val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
  [ends_with_then_chop name ext]
  @example:
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option


val escaped : string -> string

(**
  [for_all_from  s start p]
  if [start] is negative, it raises,
  if [start] is too large, it returns true
*)
val for_all_from:
  string -> 
  int -> 
  (char -> bool) -> 
  bool 

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

(**
  [find ~start ~sub s]
  returns [-1] if not found
*)
val find : ?start:int -> sub:string -> string -> int

val contain_substring : string -> string -> bool 

val non_overlap_count : sub:string -> string -> int 

val rfind : sub:string -> string -> int

(** [tail_from s 1]
  return a substring from offset 1 (inclusive)
*)
val tail_from : string -> int -> string


(** returns negative number if not found *)
val rindex_neg : string -> char -> int 

val rindex_opt : string -> char -> int option

type check_result = 
    | Good | Invalid_module_name | Suffix_mismatch

val is_valid_source_name :
   string -> check_result





val no_char : string -> char -> int -> int -> bool 


val no_slash : string -> bool 

(** return negative means no slash, otherwise [i] means the place for first slash *)
val no_slash_idx : string -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";;
  
val single_space : string

val concat3 : string -> string -> string -> string 
val concat4 : string -> string -> string -> string -> string 
val concat5 : string -> string -> string -> string -> string -> string  
val inter2 : string -> string -> string
val inter3 : string -> string -> string -> string 
val inter4 : string -> string -> string -> string -> string
val concat_array : string -> string array -> string 

val single_colon : string 

val parent_dir_lit : string
val current_dir_lit : string

val capitalize_ascii : string -> string


end = struct
#1 "ext_string.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







(*
   {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
*)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then

        acc
      else 
        String.sub str 0 last_pos :: acc
    else
    if is_delim str.[pos] then
      let new_len = (last_pos - pos - 1) in
      if new_len <> 0 || keep_empty then 
        let v = String.sub str (pos + 1) new_len in
        loop ( v :: acc)
          pos (pos - 1)
      else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  
        let u = String.unsafe_get s !i in 
        u = '\t' || u = '\n' || u = ' ' 
  do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && 
        let u = String.unsafe_get s !k in 
        u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
    split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let quick_split_by_ws str : string list = 
  split_by ~keep_empty:false (fun x -> x = '\t' || x = '\n' || x = ' ') str

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
  beg_len <=  s_len &&
  (let i = ref 0 in
   while !i <  beg_len 
         && String.unsafe_get s !i =
            String.unsafe_get beg !i do 
     incr i 
   done;
   !i = beg_len
  )

let rec ends_aux s end_ j k = 
  if k < 0 then (j + 1)
  else if String.unsafe_get s j = String.unsafe_get end_ k then 
    ends_aux s end_ (j - 1) (k - 1)
  else  -1   

(** return an index which is minus when [s] does not 
    end with [beg]
*)
let ends_with_index s end_ = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length end_ - 1 in
  if s_beg > s_finish then -1
  else
    ends_aux s end_ s_finish s_beg

let ends_with s end_ = ends_with_index s end_ >= 0 

let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

let check_suffix_case = ends_with 
let check_suffix_case_then_chop = ends_with_then_chop

let check_any_suffix_case s suffixes = 
  List.exists (fun x -> check_suffix_case s x) suffixes

let check_any_suffix_case_then_chop s suffixes = 
  let rec aux suffixes = 
    match suffixes with 
    | [] -> None 
    | x::xs -> 
      let id = ends_with_index s x in 
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in 
  aux suffixes    



(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s

(* it is unsafe to expose such API as unsafe since 
   user can provide bad input range 

*)
let rec unsafe_for_all_range s ~start ~finish p =     
  start > finish ||
  p (String.unsafe_get s start) && 
  unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start  p = 
  let len = String.length s in 
  if start < 0  then invalid_arg "Ext_string.for_all_from"
  else unsafe_for_all_range s ~start ~finish:(len - 1) p 


let for_all (p : char -> bool) s =   
  unsafe_for_all_range s ~start:0  ~finish:(String.length s - 1) p 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0


exception Local_exit 
let find ?(start=0) ~sub s =
  let n = String.length sub in
  let s_len = String.length s in 
  let i = ref start in  
  try
    while !i + n <= s_len do
      if unsafe_is_sub ~sub 0 s !i ~len:n then
        raise_notrace Local_exit;
      incr i
    done;
    -1
  with Local_exit ->
    !i

let contain_substring s sub = 
  find s ~sub >= 0 

(** TODO: optimize 
    avoid nonterminating when string is empty 
*)
let non_overlap_count ~sub s = 
  let sub_len = String.length sub in 
  let rec aux  acc off = 
    let i = find ~start:off ~sub s  in 
    if i < 0 then acc 
    else aux (acc + 1) (i + sub_len) in
  if String.length sub = 0 then invalid_arg "Ext_string.non_overlap_count"
  else aux 0 0  


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  let module M = struct exception Exit end in 
  try
    while !i >= 0 do
      if unsafe_is_sub ~sub 0 s !i ~len:n then 
        raise_notrace Local_exit;
      decr i
    done;
    -1
  with Local_exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)

let equal (x : string) y  = x = y

let rec rindex_rec s i c =
  if i < 0 then i else
  if String.unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if String.unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c;;

let rindex_neg s c = 
  rindex_rec s (String.length s - 1) c;;

let rindex_opt s c = 
  rindex_rec_opt s (String.length s - 1) c;;

let is_valid_module_file (s : string) = 
  let len = String.length s in 
  len > 0 &&
  match String.unsafe_get s 0 with 
  | 'A' .. 'Z'
  | 'a' .. 'z' -> 
    unsafe_for_all_range s ~start:1 ~finish:(len - 1)
      (fun x -> 
         match x with 
         | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
         | _ -> false )
  | _ -> false 




type check_result = 
  | Good 
  | Invalid_module_name 
  | Suffix_mismatch
  (** 
     TODO: move to another module 
     Make {!Ext_filename} not stateful
  *)
let is_valid_source_name name : check_result =
  match check_any_suffix_case_then_chop name [
      ".ml"; 
      ".re";
      ".mli"; 
      ".rei"
    ] with 
  | None -> Suffix_mismatch
  | Some x -> 
    if is_valid_module_file  x then
      Good
    else Invalid_module_name  

(** TODO: can be improved to return a positive integer instead *)
let rec unsafe_no_char x ch i  last_idx = 
  i > last_idx  || 
  (String.unsafe_get x i <> ch && unsafe_no_char x ch (i + 1)  last_idx)

let rec unsafe_no_char_idx x ch i last_idx = 
  if i > last_idx  then -1 
  else 
  if String.unsafe_get x i <> ch then 
    unsafe_no_char_idx x ch (i + 1)  last_idx
  else i

let no_char x ch i len  : bool =
  let str_len = String.length x in 
  if i < 0 || i >= str_len || len >= str_len then invalid_arg "Ext_string.no_char"   
  else unsafe_no_char x ch i len 


let no_slash x = 
  unsafe_no_char x '/' 0 (String.length x - 1)

let no_slash_idx x = 
  unsafe_no_char_idx x '/' 0 (String.length x - 1)

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if unsafe_no_char x '/' 0  (len - 1) then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let replace_backward_slash (x : string)=
  let len = String.length x in
  if unsafe_no_char x '\\' 0  (len -1) then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x

let empty = ""

    
external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";;

let single_space = " "
let single_colon = ":"

let concat_array sep (s : string array) =   
  let s_len = Array.length s in 
  match s_len with 
  | 0 -> empty 
  | 1 -> Array.unsafe_get s 0
  | _ ->     
    let sep_len = String.length sep in 
    let len = ref 0 in 
    for i = 0 to  s_len - 1 do 
      len := !len + String.length (Array.unsafe_get s i)
    done;
    let target = 
      Bytes.create 
        (!len + (s_len - 1) * sep_len ) in    
    let hd = (Array.unsafe_get s 0) in     
    let hd_len = String.length hd in 
    String.unsafe_blit hd  0  target 0 hd_len;   
    let current_offset = ref hd_len in     
    for i = 1 to s_len - 1 do 
      String.unsafe_blit sep 0 target  !current_offset sep_len;
      let cur = Array.unsafe_get s i in 
      let cur_len = String.length cur in     
      let new_off_set = (!current_offset + sep_len ) in
      String.unsafe_blit cur 0 target new_off_set cur_len; 
      current_offset := 
        new_off_set + cur_len ; 
    done;
    Bytes.unsafe_to_string target   

let concat3 a b c = 
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let len = a_len + b_len + c_len in 
  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  Bytes.unsafe_to_string target

let concat4 a b c d =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let len = a_len + b_len + c_len + d_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  Bytes.unsafe_to_string target


let concat5 a b c d e =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let e_len = String.length e in 
  let len = a_len + b_len + c_len + d_len + e_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  String.unsafe_blit e 0 target (a_len + b_len + c_len + d_len) e_len;
  Bytes.unsafe_to_string target



let inter2 a b = 
  concat3 a single_space b 


let inter3 a b c = 
  concat5 a  single_space  b  single_space  c 





let inter4 a b c d =
  concat_array single_space [| a; b ; c; d|]


let parent_dir_lit = ".."    
let current_dir_lit = "."


(* reference {!Bytes.unppercase} *)
let capitalize_ascii (s : string) : string = 
  if String.length s = 0 then s 
  else 
    begin
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        let uc = Char.unsafe_chr (Char.code c - 32) in 
        let bytes = Bytes.of_string s in
        Bytes.unsafe_set bytes 0 uc;
        Bytes.unsafe_to_string bytes 
      else s 
    end






end
module Ext_list : sig 
#1 "ext_list.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val map : ('a -> 'b) -> 'a list -> 'b list 

(** [map_last f xs ]
    will pass [true] to [f] for the last element, 
    [false] otherwise. 
    For empty list, it returns empty
*)
val map_last : (bool -> 'a -> 'b) -> 'a list -> 'b list

(** [last l]
    return the last element
    raise if the list is empty
*)
val last : 'a list -> 'a

val append : 'a list -> 'a list -> 'a list 

val map_append :  ('b -> 'a) -> 'b list -> 'a list -> 'a list

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val map2 : 
  ('a -> 'b -> 'c) ->
  'a list ->
  'b list ->
  'c list

val fold_left_with_offset : 
  (int -> 'acc -> 'a -> 'acc) -> 
  int -> 
  'acc -> 
  'a list -> 'acc 


(** @unused *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list  

(** [exclude p l] is the opposite of [filter p l] *)
val exclude : ('a -> bool) -> 'a list -> 'a list 

(** [excludes p l]
    return a tuple [excluded,newl]
    where [exluded] is true indicates that at least one  
    element is removed,[newl] is the new list where all [p x] for [x] is false

*)
val exclude_with_val : ('a -> bool) -> 'a list -> bool * 'a list 


val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

(** [split_at n l]
    will split [l] into two lists [a,b], [a] will be of length [n], 
    otherwise, it will raise
*)
val split_at : int -> 'a list -> 'a list * 'a list


(** [split_at_last l]
    It is equivalent to [split_at (List.length l - 1) l ]
*)
val split_at_last : 'a list -> 'a list * 'a

val filter_mapi : 
  (int -> 'a -> 'b option) -> 'a list -> 'b list

val filter_map2 : 
  ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list


val length_compare : 'a list -> int -> [`Gt | `Eq | `Lt ]

val length_ge : 'a list -> int -> bool
(**

   {[length xs = length ys + n ]}
   input n should be positive 
   TODO: input checking
*)

val length_larger_than_n : 
  int -> 'a list -> 'a list -> bool


(**
   [rev_map_append f l1 l2]
   [map f l1] and reverse it to append [l2]
   This weird semantics is due to it is the most efficient operation
   we can do
*)
val rev_map_append : ('a -> 'b) -> 'a list -> 'b list -> 'b list


val flat_map : 
  ('a -> 'b list) -> 
  'a list -> 
  'b list

val flat_map_append : 
  ('a -> 'b list) -> 
  'a list -> 
  'b list  ->
  'b list


(**
    [stable_group eq lst]
    Example:
    Input:
   {[
     stable_group (=) [1;2;3;4;3]
   ]}
    Output:
   {[
     [[1];[2];[4];[3;3]]
   ]}
    TODO: this is O(n^2) behavior 
    which could be improved later
*)
val stable_group : ('a -> 'a -> bool) -> 'a list -> 'a list list 

(** [drop n list]
    raise when [n] is negative
    raise when list's length is less than [n]
*)
val drop : int -> 'a list -> 'a list 

(** [find_first_not p lst ]
    if all elements in [lst] pass, return [None] 
    otherwise return the first element [e] as [Some e] which
    fails the predicate
*)
val find_first_not : ('a -> bool) -> 'a list -> 'a option 

(** [find_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
*)

val find_opt : ('a -> 'b option) -> 'a list -> 'b option 


val rev_iter : ('a -> unit) -> 'a list -> unit 

(** [for_all2_no_exn p xs ys]
    return [true] if all satisfied,
    [false] otherwise or length not equal
*)
val for_all2_no_exn : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool



(** [f] is applied follow the list order *)
val split_map : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list       

(** [fn] is applied from left to right *)
val reduce_from_left : 
  ('a -> 'a -> 'a) -> 'a list -> 'a

val sort_via_array :
  ('a -> 'a -> int) -> 'a list -> 'a list  




(** [assoc_by_string default key lst]
    if  [key] is found in the list  return that val,
    other unbox the [default], 
    otherwise [assert false ]
*)
val assoc_by_string : 
  'a  option -> string -> (string * 'a) list -> 'a  

val assoc_by_int : 
  'a  option -> int -> (int * 'a) list -> 'a   


val nth_opt : 'a list -> int -> 'a option  
end = struct
#1 "ext_list.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)




let rec map f l =
  match l with
  | [] ->
    []
  | [x1] ->
    let y1 = f x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [y1; y2; y3; y4]
  | x1::x2::x3::x4::x5::tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    y1::y2::y3::y4::y5::(map f tail)


let rec map_last f l =
  match l with
  | [] ->
    []
  | [x1] ->
    let y1 = f true x1 in
    [y1]
  | [x1; x2] ->
    let y1 = f false x1 in
    let y2 = f true x2 in
    [y1; y2]
  | [x1; x2; x3] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f true x3 in
    [y1; y2; y3]
  | [x1; x2; x3; x4] ->
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f true x4 in
    [y1; y2; y3; y4]
  | x1::x2::x3::x4::tail ->
    (* make sure that tail is not empty *)    
    let y1 = f false x1 in
    let y2 = f false x2 in
    let y3 = f false x3 in
    let y4 = f false x4 in
    y1::y2::y3::y4::(map_last f tail)

let rec last xs =
  match xs with 
  | [x] -> x 
  | _ :: tl -> last tl 
  | [] -> invalid_arg "Ext_list.last"    



let rec append_aux l1 l2 = 
  match l1 with
  | [] -> l2
  | [a0] -> a0::l2
  | [a0;a1] -> a0::a1::l2
  | [a0;a1;a2] -> a0::a1::a2::l2
  | [a0;a1;a2;a3] -> a0::a1::a2::a3::l2
  | [a0;a1;a2;a3;a4] -> a0::a1::a2::a3::a4::l2
  | a0::a1::a2::a3::a4::rest -> a0::a1::a2::a3::a4::append_aux rest l2

let append l1 l2 =   
  match l2 with 
  | [] -> l1 
  | _ -> append_aux l1 l2  


let rec map_append  f l1 l2 =   
  match l1 with
  | [] -> l2
  | [a0] -> f a0::l2
  | [a0;a1] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    b0::b1::l2
  | [a0;a1;a2] -> 
    let b0 = f a0 in 
    let b1 = f a1 in  
    let b2 = f a2 in 
    b0::b1::b2::l2
  | [a0;a1;a2;a3] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    b0::b1::b2::b3::l2
  | [a0;a1;a2;a3;a4] -> 
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    let b4 = f a4 in 
    b0::b1::b2::b3::b4::l2

  | a0::a1::a2::a3::a4::rest ->
    let b0 = f a0 in 
    let b1 = f a1 in 
    let b2 = f a2 in 
    let b3 = f a3 in 
    let b4 = f a4 in 
    b0::b1::b2::b3::b4::map_append f rest l2 



let rec fold_right f l acc = 
  match l with  
  | [] -> acc 
  | [a0] -> f a0 acc 
  | [a0;a1] -> f a0 (f a1 acc)
  | [a0;a1;a2] -> f a0 (f a1 (f a2 acc))
  | [a0;a1;a2;a3] -> f a0 (f a1 (f a2 (f a3 acc))) 
  | [a0;a1;a2;a3;a4] -> 
    f a0 (f a1 (f a2 (f a3 (f a4 acc))))
  | a0::a1::a2::a3::a4::rest -> 
    f a0 (f a1 (f a2 (f a3 (f a4 (fold_right f rest acc)))))  

let rec fold_right2 f l r acc = 
  match l,r  with  
  | [],[] -> acc 
  | [a0],[b0] -> f a0 b0 acc 
  | [a0;a1],[b0;b1] -> f a0 b0 (f a1 b1 acc)
  | [a0;a1;a2],[b0;b1;b2] -> f a0 b0 (f a1 b1 (f a2 b2 acc))
  | [a0;a1;a2;a3],[b0;b1;b2;b3] ->
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 acc))) 
  | [a0;a1;a2;a3;a4], [b0;b1;b2;b3;b4] -> 
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 acc))))
  | a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest -> 
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 (fold_right2 f arest brest acc)))))  
  | _, _ -> invalid_arg "Ext_list.fold_right2"

let rec map2 f l r = 
  match l,r  with  
  | [],[] -> []
  | [a0],[b0] -> [f a0 b0]
  | [a0;a1],[b0;b1] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    [c0; c1]
  | [a0;a1;a2],[b0;b1;b2] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    [c0;c1;c2]
  | [a0;a1;a2;a3],[b0;b1;b2;b3] ->
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    [c0;c1;c2;c3]
  | [a0;a1;a2;a3;a4], [b0;b1;b2;b3;b4] -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    let c4 = f a4 b4 in 
    [c0;c1;c2;c3;c4]
  | a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest -> 
    let c0 = f a0 b0 in 
    let c1 = f a1 b1 in 
    let c2 = f a2 b2 in 
    let c3 = f a3 b3 in 
    let c4 = f a4 b4 in 
    c0::c1::c2::c3::c4::map2 f arest brest
  | _, _ -> invalid_arg "Ext_list.map2"

let rec fold_left_with_offset f i accu l =
  match l with
  | [] -> accu
  | a::l -> fold_left_with_offset f (succ i) (f i accu a) l


let rec filter_map (f: 'a -> 'b option) xs = 
  match xs with 
  | [] -> []
  | y :: ys -> 
    begin match f y with 
      | None -> filter_map f ys
      | Some z -> z :: filter_map f ys
    end

let rec exclude p xs =   
  match xs with 
  | [] ->  []
  | x::xs -> 
    if p x then exclude p xs 
    else x:: exclude p xs  

let rec exclude_with_val p l =
  match l with 
  | [] ->  false, l
  | a0::xs -> 
    if p a0 then true, exclude p xs 
    else 
      match xs with 
      | [] -> false, l 
      | a1::rest -> 
        if p a1 then 
          true, a0:: exclude p rest 
        else 
          let st,rest = exclude_with_val p rest in 
          if st then 
            st, a0::a1::rest
          else st, l 



let rec same_length xs ys = 
  match xs, ys with 
  | [], [] -> true
  | _::xs, _::ys -> same_length xs ys 
  | _, _ -> false 


let init n f = 
  match n with 
  | 0 -> []
  | 1 -> 
    let a0 = f 0 in  
    [a0]
  | 2 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    [a0; a1]
  | 3 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    [a0; a1; a2]
  | 4 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    let a3 = f 3 in 
    [a0; a1; a2; a3]
  | 5 -> 
    let a0 = f 0 in 
    let a1 = f 1 in 
    let a2 = f 2 in 
    let a3 = f 3 in 
    let a4 = f 4 in  
    [a0; a1; a2; a3; a4]
  | _ ->
    Array.to_list (Array.init n f)

let rec small_split_at n acc l = 
  if n <= 0 then List.rev acc , l 
  else 
    match l with 
    | x::xs -> small_split_at (n - 1) (x ::acc) xs 
    | _ -> invalid_arg "Ext_list.split_at"

let split_at n l = 
  small_split_at n [] l 

let rec split_at_last_aux acc x = 
  match x with 
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [ x] -> List.rev acc, x
  | y0::ys -> split_at_last_aux (y0::acc) ys   

let split_at_last (x : 'a list) = 
  match x with 
  | [] -> invalid_arg "Ext_list.split_at_last"
  | [a0] -> 
    [], a0
  | [a0;a1] -> 
    [a0], a1  
  | [a0;a1;a2] -> 
    [a0;a1], a2 
  | [a0;a1;a2;a3] -> 
    [a0;a1;a2], a3 
  | [a0;a1;a2;a3;a4] ->
    [a0;a1;a2;a3], a4 
  | a0::a1::a2::a3::a4::rest  ->  
    let rev, last = split_at_last_aux [] rest
    in 
    a0::a1::a2::a3::a4::  rev , last

(**
   can not do loop unroll due to state combination
*)  
let  filter_mapi (f: int -> 'a -> 'b option) xs = 
  let rec aux i xs = 
    match xs with 
    | [] -> []
    | y :: ys -> 
      begin match f i y with 
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys
      end in
  aux 0 xs 

let rec filter_map2 (f: 'a -> 'b -> 'c option) xs ys = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
    begin match f u v with 
      | None -> filter_map2 f us vs (* idea: rec f us vs instead? *)
      | Some z -> z :: filter_map2 f us vs
    end
  | _ -> invalid_arg "Ext_list.filter_map2"


let rec rev_map_append  f l1 l2 =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append f l (f a :: l2)


let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l   (a :: l2)

(** It is not worth loop unrolling, 
    it is already tail-call, and we need to be careful 
    about evaluation order when unroll
*)
let rec flat_map_aux f acc append lx =
  match lx with
  | [] -> rev_append acc  append
  | a0::rest -> flat_map_aux f (rev_append (f a0)  acc ) append rest 

let flat_map f lx =
  flat_map_aux f [] [] lx

let flat_map_append f lx append  =
  flat_map_aux f [] append lx  


let rec length_compare l n = 
  if n < 0 then `Gt 
  else 
    begin match l with 
      | _ ::xs -> length_compare xs (n - 1)
      | [] ->  
        if n = 0 then `Eq 
        else `Lt 
    end

let rec length_ge l n =   
  if n > 0 then
    match l with 
    | _ :: tl -> length_ge tl (n - 1)
    | [] -> false
  else true
(**

   {[length xs = length ys + n ]}
*)
let rec length_larger_than_n n xs ys =
  match xs, ys with 
  | _, [] -> length_compare xs n = `Eq   
  | _::xs, _::ys -> 
    length_larger_than_n n xs ys
  | [], _ -> false 




let rec group (eq : 'a -> 'a -> bool) lst =
  match lst with 
  | [] -> []
  | x::xs -> 
    aux eq x (group eq xs )

and aux eq (x : 'a)  (xss : 'a list list) : 'a list list = 
  match xss with 
  | [] -> [[x]]
  | (y0::_ as y)::ys -> (* cannot be empty *) 
    if eq x y0 then
      (x::y) :: ys 
    else
      y :: aux eq x ys                                 
  | _ :: _ -> assert false    

let stable_group eq lst =  group eq lst |> List.rev  

let rec drop n h = 
  if n < 0 then invalid_arg "Ext_list.drop"
  else
  if n = 0 then h 
  else 
    match h with 
    | [] ->
      invalid_arg "Ext_list.drop"
    | _ :: tl ->   
      drop (n - 1) tl

let rec find_first_not  p = function
  | [] -> None
  | a::l -> 
    if p a 
    then find_first_not p l
    else Some a 


let rec rev_iter f l = 
  match l with
  | [] -> ()    
  | [x1] ->
    f x1 
  | [x1; x2] ->
    f x2 ; f x1 
  | [x1; x2; x3] ->
    f x3 ; f x2 ; f x1 
  | [x1; x2; x3; x4] ->
    f x4; f x3; f x2; f x1 
  | x1::x2::x3::x4::x5::tail ->
    rev_iter f tail;
    f x5; f x4 ; f x3; f x2 ; f x1


let rec for_all2_no_exn p l1 l2 = 
  match (l1, l2) with
  | ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_no_exn p l1 l2
  | (_, _) -> false


let rec find_opt p = function
  | [] -> None
  | x :: l -> 
    match  p x with 
    | Some _ as v  ->  v
    | None -> find_opt p l 



let rec split_map f l = 
  match l with
  | [] ->
    [],[]
  | [x1] ->
    let a0,b0 = f x1 in
    [a0],[b0]
  | [x1; x2] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    [a1;a2],[b1;b2]
  | [x1; x2; x3] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    [a1;a2;a3], [b1;b2;b3]
  | [x1; x2; x3; x4] ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    let a4,b4 = f x4 in
    [a1;a2;a3;a4], [b1;b2;b3;b4] 
  | x1::x2::x3::x4::x5::tail ->
    let a1,b1 = f x1 in
    let a2,b2 = f x2 in
    let a3,b3 = f x3 in
    let a4,b4 = f x4 in
    let a5,b5 = f x5 in
    let ass,bss = split_map f tail in 
    a1::a2::a3::a4::a5::ass,
    b1::b2::b3::b4::b5::bss


let reduce_from_left fn lst = 
  match lst with 
  | first :: rest ->  List.fold_left fn first rest 
  | _ -> invalid_arg "Ext_list.reduce_from_left"


let sort_via_array cmp lst =
  let arr = Array.of_list lst  in
  Array.sort cmp arr;
  Array.to_list arr




let rec assoc_by_string def (k : string) lst = 
  match lst with 
  | [] -> 
    begin match def with 
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if Ext_string.equal k1 k then v1 else 
      assoc_by_string def k rest 

let rec assoc_by_int def (k : int) lst = 
  match lst with 
  | [] -> 
    begin match def with
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if k1 = k then v1 else 
      assoc_by_int def k rest     


let rec nth_aux l n =
  match l with
  | [] -> None
  | a::l -> if n = 0 then Some a else nth_aux l (n-1)

let nth_opt l n =
  if n < 0 then None 
  else
    nth_aux l n
end
module Map_gen
= struct
#1 "map_gen.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** adapted from stdlib *)

type ('key,'a) t =
  | Empty
  | Node of ('key,'a) t * 'key * 'a * ('key,'a) t * int

type ('key,'a) enumeration =
  | End
  | More of 'key * 'a * ('key,'a) t * ('key, 'a) enumeration

let rec cardinal_aux acc  = function
  | Empty -> acc 
  | Node (l,_,_,r, _) -> 
    cardinal_aux  (cardinal_aux (acc + 1)  r ) l 

let cardinal s = cardinal_aux 0 s 

let rec bindings_aux accu = function
  | Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings s =
  bindings_aux [] s

let rec keys_aux accu = function
    Empty -> accu
  | Node(l, v, _, r, _) -> keys_aux (v :: keys_aux accu r) l

let keys s = keys_aux [] s



let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))


let height = function
  | Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton x d = Node(Empty, x, d, Empty, 1)

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec min_binding_exn = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding_exn l

let choose = min_binding_exn

let rec max_binding_exn = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> (x, d)
  | Node(l, x, d, r, _) -> max_binding_exn r

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    bal t1 x d (remove_min_binding t2)


let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter f l; f v d; iter f r

let rec map f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map f l in
    let d' = f d in
    let r' = map f r in
    Node(l', v, d', r', h)

let rec mapi f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi f l in
    let d' = f v d in
    let r' = mapi f r in
    Node(l', v, d', r', h)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold f r (f v d (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, h) ->
    bal (add_min_binding k v l) x d r

let rec add_max_binding k v = function
  | Empty -> singleton k v
  | Node (l, x, d, r, h) ->
    bal l x d (add_max_binding k v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
    (Empty, _) -> add_min_binding v d r
  | (_, Empty) -> add_max_binding v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal ll lv ld (join lr v d r) else
    if rh > lh + 2 then bal (join l v d rl) rv rd rr else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding_exn t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec filter p = function
    Empty -> Empty
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter p l in
    let pvd = p v d in
    let r' = filter p r in
    if pvd then join l' v d r' else concat l' r'

let rec partition p = function
    Empty -> (Empty, Empty)
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition p l in
    let pvd = p v d in
    let (rt, rf) = partition p r in
    if pvd
    then (join lt v d rt, concat lf rf)
    else (concat lt rt, join lf v d rf)

let compare compare_key cmp_val m1 m2 =
  let rec compare_aux e1  e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      let c = compare_key v1 v2 in
      if c <> 0 then c else
        let c = cmp_val d1 d2 in
        if c <> 0 then c else
          compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal compare_key cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      compare_key v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)



    
module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] 
        If [x] was already bound in [m], its previous binding disappears. *)
    val adjust: key -> (unit -> 'a)  -> ('a ->  'a) -> 'a t -> 'a t 
    (** [adjust k v f map] if not exist [add k v], otherwise 
        [add k v (f old)]
    *)
    val singleton: key -> 'a -> 'a t

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

     val disjoint_merge : 'a t -> 'a t -> 'a t
     (* merge two maps, will raise if they have the same key *)
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
        The bindings are passed to [f] in increasing order. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order) *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map.
        order unspecified
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p]. 
        order unspecified
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        order unspecified
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map. *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering *)
    val keys : 'a t -> key list 
    (* Increasing order *)

    val min_binding_exn: 'a t -> (key * 'a)
    (** raise [Not_found] if the map is empty. *)

    val max_binding_exn: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding} *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
     *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find_exn: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find_opt: key -> 'a t -> 'a option
    val find_default: key  -> 'a t -> 'a  -> 'a 
    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val of_list : (key * 'a) list -> 'a t 
    val of_array : (key * 'a ) array -> 'a t 
    val add_list : (key * 'b) list -> 'b t -> 'b t

  end

end
module String_map : sig 
#1 "string_map.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


include Map_gen.S with type key = string

end = struct
#1 "string_map.ml"

# 2 "ext/map.cppo.ml"
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)


  
# 10
  type key = string 
  let compare_key = Ext_string.compare

# 22
type 'a t = (key,'a) Map_gen.t
exception Duplicate_key of key 

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let keys = Map_gen.keys
let choose = Map_gen.choose 
let partition = Map_gen.partition 
let filter = Map_gen.filter 
let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 
let max_binding_exn = Map_gen.max_binding_exn
let min_binding_exn = Map_gen.min_binding_exn


let rec add x data (tree : _ Map_gen.t as 'a) : 'a = match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add x data l) v d r
    else
      bal l v d (add x data r)


let rec adjust x data replace (tree : _ Map_gen.t as 'a) : 'a = 
  match tree with 
  | Empty ->
    Node(Empty, x, data (), Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, replace  d , r, h)
    else if c < 0 then
      bal (adjust x data replace l) v d r
    else
      bal l v d (adjust x data replace r)


let rec find_exn x (tree : _ Map_gen.t )  = match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find_exn x (if c < 0 then l else r)

let rec find_opt x (tree : _ Map_gen.t )  = match tree with 
  | Empty -> None 
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then Some d
    else find_opt x (if c < 0 then l else r)

let rec find_default x (tree : _ Map_gen.t ) default     = match tree with 
  | Empty -> default  
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then  d
    else find_default x   (if c < 0 then l else r) default

let rec mem x (tree : _ Map_gen.t )   = match tree with 
  | Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ Map_gen.t as 'a) : 'a = match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove x l) v d r
    else
      bal l v d (remove x r)


let rec split x (tree : _ Map_gen.t as 'a) : 'a * _ option * 'a  = match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, Map_gen.join rl v d r)
    else
      let (lr, pres, rr) = split x r in (Map_gen.join l v d lr, pres, rr)

let rec merge f (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    Map_gen.concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    Map_gen.concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
  | _ ->
    assert false

let rec disjoint_merge  (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    begin match split v1 s2 with 
    | l2, None, r2 -> 
      Map_gen.join (disjoint_merge  l1 l2) v1 d1 (disjoint_merge r1 r2)
    | _, Some _, _ ->
      raise (Duplicate_key  v1)
    end        
  | (_, Node (l2, v2, d2, r2, h2)) ->
    begin match  split v2 s1 with 
    | (l1, None, r1) -> 
      Map_gen.join (disjoint_merge  l1 l2) v2 d2 (disjoint_merge  r1 r2)
    | (_, Some _, _) -> 
      raise (Duplicate_key v2)
    end
  | _ ->
    assert false



let compare cmp m1 m2 = Map_gen.compare compare_key cmp m1 m2

let equal cmp m1 m2 = Map_gen.equal compare_key cmp m1 m2 

let add_list (xs : _ list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 

let of_list xs = add_list xs empty

let of_array xs = 
  Array.fold_left (fun acc (k,v) -> add k v acc) empty xs

end
module Ast_payload : sig 
#1 "ast_payload.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



(** A utility module used when destructuring parsetree attributes, used for 
    compiling FFI attributes and built-in ppx  *)

type t = Parsetree.payload
type lid = string Asttypes.loc
type label_expr = lid  * Parsetree.expression
type action = 
   lid * Parsetree.expression option

val is_single_string : t -> (string * string option) option
val is_single_int : t -> int option 

type rtn = Not_String_Lteral | JS_Regex_Check_Failed | Correct of Parsetree.expression
val as_string_exp : check_js_regex: bool -> t -> rtn
val as_core_type : Location.t -> t -> Parsetree.core_type    
val as_empty_structure :  t -> bool 
val as_ident : t -> Longident.t Asttypes.loc option
val raw_string_payload : Location.t -> string -> t 
val assert_strings :
  Location.t -> t -> string list  

(** if only [abstract] happens  [true]
    if [abstract] does not appear [false]
    if [abstract] happens with other, raise exception
*)  
val isAbstract : action list -> bool   
(** as a record or empty 
    it will accept 

    {[ [@@@bs.config ]]}
    or 
    {[ [@@@bs.config no_export ] ]}
    or 
    {[ [@@@bs.config { property  .. } ]]}    
    Note that we only 
    {[
      { flat_property}
    ]}
    below  is not allowed 
    {[
      {M.flat_property}
    ]}
*)

val ident_or_record_as_config : 
  Location.t ->
  t -> action list 

val assert_bool_lit : Parsetree.expression -> bool

val empty : t 

val table_dispatch : 
  (Parsetree.expression option  -> 'a) String_map.t -> action -> 'a

end = struct
#1 "ast_payload.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.payload

let is_single_string (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,dec));
           _},_);
      _}] -> Some (name,dec)
  | _  -> None

let is_single_int (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_int name);
           _},_);
      _}] -> Some name
  | _  -> None

type rtn = Not_String_Lteral | JS_Regex_Check_Failed | Correct of Parsetree.expression

let as_string_exp ~check_js_regex (x : t ) = 
  match x with  (** TODO also need detect empty phrase case *)
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (str,_));
           _} as e ,_);
      _}] -> if check_js_regex then (if Ext_js_regex.js_regex_checker str then Correct e else JS_Regex_Check_Failed) else Correct e
  | _  -> Not_String_Lteral

let as_core_type loc x =
  match  x with
  | Parsetree.PTyp x -> x
  | _ -> Location.raise_errorf ~loc "except a core type"

let as_ident (x : t ) =
  match x with
  | PStr [
      {pstr_desc =
         Pstr_eval (
           {
             pexp_desc =
               Pexp_ident ident 

           } , _)
      }
    ] -> Some ident
  | _ -> None
open Ast_helper

let raw_string_payload loc (s : string) : t =
  PStr [ Str.eval ~loc (Exp.constant ~loc (Const_string (s,None)  ))]

let as_empty_structure (x : t ) = 
  match x with 
  | PStr ([]) -> true
  | PTyp _ | PPat _ | PStr (_ :: _ ) -> false 

type lid = string Asttypes.loc
type label_expr = lid  * Parsetree.expression

type action = 
  lid * Parsetree.expression option 
(** None means punning is hit 
    {[ { x } ]}
    otherwise it comes with a payload 
    {[ { x = exp }]}
*)

let  isAbstract (xs : action list) = 
  match xs with 
  | [{loc; txt = "abstract"}, None]  -> 
    true 
  | [{loc; txt = "abstract"}, Some _ ]
    -> 
      Location.raise_errorf ~loc "invalid config for abstract"
  | xs -> 
    List.iter (function (({loc; txt} : lid),_) ->  
      match txt with 
      | "abstract" -> 
        Location.raise_errorf ~loc 
          "bs.deriving abstract does not work with any other deriving"
      | _ -> ()
    ) xs ;
    false


let ident_or_record_as_config     
    loc
    (x : Parsetree.payload) 
  : ( string Location.loc * Parsetree.expression option) list 
  = 
  match  x with 
  | PStr 
      [ {pstr_desc = Pstr_eval
             ({pexp_desc = Pexp_record (label_exprs, with_obj) ; pexp_loc = loc}, _); 
         _
        }]
    -> 
    begin match with_obj with
      | None ->
        Ext_list.map
          (fun ((x,y) : (Longident.t Asttypes.loc * _) ) -> 
             match (x,y) with 
             | ({txt = Lident name; loc} ) , 
               ({Parsetree.pexp_desc = Pexp_ident{txt = Lident name2}} )
               when name2 = name -> 
               ({Asttypes.txt = name ; loc}, None)
             | ({txt = Lident name; loc} ), y 
               -> 
               ({Asttypes.txt = name ; loc}, Some y)
             | _ -> 
               Location.raise_errorf ~loc "Qualified label is not allood"
          )
          label_exprs
      | Some _ -> 
        Location.raise_errorf ~loc "with is not supported"
    end
  | PStr [
      {pstr_desc =
         Pstr_eval (
           {
             pexp_desc =
               Pexp_ident ({loc = lloc; txt = Lident txt});

           } , _)
      }
    ] -> [ {Asttypes.txt ; loc = lloc}, None] 
  | PStr [] -> []
  | _ -> 
    Location.raise_errorf ~loc "this is not a valid record config"



let assert_strings loc (x : t) : string list
  = 
  let module M = struct exception Not_str end  in 
  match x with 
  | PStr [ {pstr_desc =  
              Pstr_eval (
                {pexp_desc = 
                   Pexp_tuple strs;
                 _},_);
            pstr_loc = loc ;            
            _}] ->
    (try 
       strs |> Ext_list.map (fun e ->
           match (e : Parsetree.expression) with
           | {pexp_desc = Pexp_constant (Const_string (name,_)); _} -> 
             name
           | _ -> raise M.Not_str)
     with M.Not_str ->
       Location.raise_errorf ~loc "expect string tuple list"
    )
  | PStr [ {
      pstr_desc =  
        Pstr_eval (
          {pexp_desc = 
             Pexp_constant 
               (Const_string (name,_));
           _},_);
      _}] ->  [name] 
  | PStr [] ->  []
  | PStr _                
  | PTyp _ | PPat _ ->
    Location.raise_errorf ~loc "expect string tuple list"
let assert_bool_lit  (e : Parsetree.expression) = 
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident "true" }, None)
    -> true
  | Pexp_construct ({txt = Lident "false" }, None)
    -> false 
  | _ ->
    Location.raise_errorf ~loc:e.pexp_loc "expect `true` or `false` in this field"


let empty : t = Parsetree.PStr []



let table_dispatch table (action : action)
  = 
  match action with 
  | {txt =  name; loc  }, y -> 
    begin match String_map.find_exn name table with 
      | fn -> fn y
      | exception _ -> Location.raise_errorf ~loc "%s is not supported" name
    end

end
module Ast_literal : sig 
#1 "ast_literal.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type 'a  lit = ?loc: Location.t -> unit -> 'a
module Lid : sig
  type t = Longident.t 
  val val_unit : t 
  val type_unit : t 
  val type_int : t 
  val js_fn : t 
  val js_meth : t 
  val js_meth_callback : t 
  val js_obj : t 

  val ignore_id : t 
  val js_null : t 
  val js_undefined : t
  val js_null_undefined : t 
  val js_re_id : t 
  val js_unsafe : t 
end

type expression_lit = Parsetree.expression lit 
type core_type_lit = Parsetree.core_type lit 
type pattern_lit = Parsetree.pattern lit 

val val_unit : expression_lit

val type_unit : core_type_lit
val type_exn : core_type_lit
val type_string : core_type_lit
val type_int : core_type_lit 
val type_any : core_type_lit

val pat_unit : pattern_lit

end = struct
#1 "ast_literal.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper


module Lid = struct 
  type t = Longident.t 
  let val_unit : t = Lident "()"
  let type_unit : t = Lident "unit"
  let type_string : t = Lident "string"
  let type_int : t = Lident "int" (* use *predef* *)
  let type_exn : t = Lident "exn" (* use *predef* *)
  (* TODO should be renamed in to {!Js.fn} *)
  (* TODO should be moved into {!Js.t} Later *)
  let js_fn : t = 
      Ldot (Ldot (Lident "Js", "Internal"),  "fn")
  let js_meth : t = 
      Ldot (Ldot (Lident "Js", "Internal") , "meth")
  let js_meth_callback : t = 
      Ldot (Ldot (Lident "Js", "Internal"), "meth_callback")
  let js_obj : t = Ldot (Lident "Js", "t") 
  let ignore_id : t = Ldot (Lident "Pervasives", "ignore")
  let js_null  : t = Ldot (Lident "Js", "null")
  let js_undefined : t = Ldot (Lident "Js", "undefined")
  let js_null_undefined : t = Ldot (Lident "Js", "null_undefined")
  let js_re_id : t = Ldot (Ldot (Lident "Js", "Re"), "t")
  let js_unsafe : t = Lident "Js_unsafe"
end

module No_loc = struct 
  let loc = Location.none
  let val_unit = 
    Ast_helper.Exp.construct {txt = Lid.val_unit; loc }  None

  let type_unit =   
    Ast_helper.Typ.mk  (Ptyp_constr ({ txt = Lid.type_unit; loc}, []))
  let type_exn =   
    Ast_helper.Typ.mk  (Ptyp_constr ({ txt = Lid.type_unit; loc}, []))

  let type_int = 
    Ast_helper.Typ.mk (Ptyp_constr ({txt = Lid.type_int; loc}, []))  
  let type_string =   
    Ast_helper.Typ.mk  (Ptyp_constr ({ txt = Lid.type_string; loc}, []))

  let type_any = Ast_helper.Typ.any ()
  let pat_unit = Pat.construct {txt = Lid.val_unit; loc} None
end 

type 'a  lit = ?loc: Location.t -> unit -> 'a
type expression_lit = Parsetree.expression lit 
type core_type_lit = Parsetree.core_type lit 
type pattern_lit = Parsetree.pattern lit 

let val_unit ?loc () = 
  match loc with 
  | None -> No_loc.val_unit
  | Some loc -> Ast_helper.Exp.construct {txt = Lid.val_unit; loc}  None


let type_unit ?loc () = 
  match loc with
  | None ->     
    No_loc.type_unit
  | Some loc -> 
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_unit; loc}, []))

let type_exn ?loc () = 
  match loc with
  | None ->     
    No_loc.type_exn
  | Some loc -> 
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_exn; loc}, []))


let type_string ?loc () = 
  match loc with 
  | None -> No_loc.type_string 
  | Some loc ->     
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_string; loc}, []))

let type_int ?loc () = 
  match loc with 
  | None -> No_loc.type_int
  | Some loc ->     
    Ast_helper.Typ.mk ~loc  (Ptyp_constr ({ txt = Lid.type_int; loc}, []))

let type_any ?loc () = 
  match loc with 
  | None -> No_loc.type_any
  | Some loc -> Ast_helper.Typ.any ~loc ()

let pat_unit ?loc () = 
  match loc with 
  | None -> No_loc.pat_unit
  | Some loc -> 
    Pat.construct ~loc {txt = Lid.val_unit; loc} None

end
module Ast_comb : sig 
#1 "ast_comb.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val exp_apply_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.expression -> Parsetree.expression list -> Parsetree.expression

val fun_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.pattern -> Parsetree.expression -> Parsetree.expression

val arrow_no_label : 
  ?loc:Location.t ->
  ?attrs:Parsetree.attributes ->
  Parsetree.core_type -> Parsetree.core_type -> Parsetree.core_type

(* note we first declare its type is [unit], 
   then [ignore] it, [ignore] is necessary since 
   the js value  maybe not be of type [unit] and 
   we can use [unit] value (though very little chance) 
   sometimes
*)
val discard_exp_as_unit : 
  Location.t -> Parsetree.expression -> Parsetree.expression


val tuple_type_pair : 
  ?loc:Ast_helper.loc ->
  [< `Make | `Run ] ->
  int -> Parsetree.core_type * Parsetree.core_type list * Parsetree.core_type

val to_js_type :
  Location.t -> Parsetree.core_type -> Parsetree.core_type



val to_undefined_type :
  Location.t -> Parsetree.core_type -> Parsetree.core_type  

val to_js_re_type : Location.t -> Parsetree.core_type

val single_non_rec_value : 
  Ast_helper.str -> 
  Parsetree.expression -> 
  Parsetree.structure_item

val single_non_rec_val :   
  Ast_helper.str -> 
  Parsetree.core_type -> 
  Parsetree.signature_item
end = struct
#1 "ast_comb.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


open Ast_helper 

let exp_apply_no_label ?loc ?attrs a b = 
  Exp.apply ?loc ?attrs a (Ext_list.map (fun x -> "", x) b)

let fun_no_label ?loc ?attrs  pat body = 
  Exp.fun_ ?loc ?attrs "" None pat body

let arrow_no_label ?loc ?attrs b c = 
  Typ.arrow ?loc ?attrs "" b c 

let discard_exp_as_unit loc e = 
  exp_apply_no_label ~loc     
    (Exp.ident ~loc {txt = Ast_literal.Lid.ignore_id; loc})
    [Exp.constraint_ ~loc e 
       (Ast_literal.type_unit ~loc ())]


let tuple_type_pair ?loc kind arity = 
  let prefix  = "a" in
  if arity = 0 then 
    let ty = Typ.var ?loc ( prefix ^ "0") in 
    match kind with 
    | `Run -> ty,  [], ty 
    | `Make -> 
      (Typ.arrow "" ?loc
         (Ast_literal.type_unit ?loc ())
         ty ,
       [], ty)
  else
    let number = arity + 1 in
    let tys = Ext_list.init number (fun i -> 
        Typ.var ?loc (prefix ^ string_of_int (number - i - 1))
      )  in
    match tys with 
    | result :: rest -> 
      Ext_list.reduce_from_left (fun r arg -> Typ.arrow "" ?loc arg r) tys, 
      List.rev rest , result
    | [] -> assert false
    
    

let js_obj_type_id  = 
  Ast_literal.Lid.js_obj 

let re_id  = 
  Ast_literal.Lid.js_re_id 

let to_js_type loc  x  = 
  Typ.constr ~loc {txt = js_obj_type_id; loc} [x]

let to_js_re_type loc  =
  Typ.constr ~loc { txt = re_id ; loc} []
    
let to_undefined_type loc x =
  Typ.constr ~loc
    {txt = Ast_literal.Lid.js_undefined ; loc}
    [x]  

let single_non_rec_value  name exp = 
  Str.value Nonrecursive 
    [Vb.mk (Pat.var name) exp]

let single_non_rec_val name ty = 
  Sig.value 
    (Val.mk name ty)
end
module Bs_syntaxerr : sig 
#1 "bs_syntaxerr.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type error 
  = Unsupported_predicates
  | Conflict_bs_bs_this_bs_meth  
  | Duplicated_bs_deriving
  | Conflict_attributes

  | Duplicated_bs_as 
  | Expect_int_literal
  | Expect_string_literal
  | Expect_int_or_string_or_json_literal
  | Unhandled_poly_type
  | Unregistered of string 
  | Invalid_underscore_type_in_external
  | Invalid_bs_string_type 
  | Invalid_bs_int_type 
  | Invalid_bs_unwrap_type
  | Conflict_ffi_attribute of string
  | Not_supported_in_bs_deriving
  | Canot_infer_arity_by_syntax
  | Illegal_attribute
  | Inconsistent_arity of int * int 
  (* we still rqeuire users to have explicit annotation to avoid
     {[ (((int -> int) -> int) -> int )]}
  *)
  | Not_supported_directive_in_bs_return
  | Expect_opt_in_bs_return_to_opt
  | Label_in_uncurried_bs_attribute

  | Bs_this_simple_pattern


val err : Location.t -> error -> 'a

end = struct
#1 "bs_syntaxerr.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



type error 
  = Unsupported_predicates
  | Conflict_bs_bs_this_bs_meth  
  | Duplicated_bs_deriving
  | Conflict_attributes

  | Duplicated_bs_as 
  | Expect_int_literal
  | Expect_string_literal
  | Expect_int_or_string_or_json_literal
  | Unhandled_poly_type
  | Unregistered of string 
  | Invalid_underscore_type_in_external
  | Invalid_bs_string_type 
  | Invalid_bs_int_type 
  | Invalid_bs_unwrap_type
  | Conflict_ffi_attribute of string
  | Not_supported_in_bs_deriving
  | Canot_infer_arity_by_syntax
  | Illegal_attribute
  | Inconsistent_arity of int * int 
  (* we still rqeuire users to have explicit annotation to avoid
     {[ (((int -> int) -> int) -> int )]}
  *)
  | Not_supported_directive_in_bs_return
  | Expect_opt_in_bs_return_to_opt
  | Label_in_uncurried_bs_attribute

  | Bs_this_simple_pattern

let pp_error fmt err =
  Format.pp_print_string fmt @@ match err with
  | Label_in_uncurried_bs_attribute 
    -> "label is not allowed here, it is due to `bs.` attribute indicate uncurried calling convention which does not support label argument yet"
  | Expect_opt_in_bs_return_to_opt
      ->
        "bs.return directive *_to_opt expect return type to be \n\
         syntax wise `_ option` for safety"

  | Not_supported_directive_in_bs_return
    ->
    "Not supported return directive"                
  | Illegal_attribute ->
    "Illegal attributes"
  | Canot_infer_arity_by_syntax
    ->   "Can not infer the arity by syntax, either [@bs.uncurry n] or \n\
              write it in arrow syntax "
  | Inconsistent_arity (arity,n)
      -> Printf.sprintf "Inconsistent arity %d vs %d" arity n 
  | Not_supported_in_bs_deriving
    ->
    "not supported in deriving"
  | Unsupported_predicates 
    ->
     "unsupported predicates"
  | Conflict_bs_bs_this_bs_meth -> 
     "[@bs.this], [@bs], [@bs.meth] can not be applied at the same time"
  | Duplicated_bs_deriving
    -> "duplicated bs.deriving attribute"
  | Conflict_attributes
    -> "conflicting attributes " 
  | Expect_string_literal
    -> "expect string literal "
  | Duplicated_bs_as 
    -> 
    "duplicated bs.as "
  | Expect_int_literal 
    -> 
    "expect int literal "
  | Expect_int_or_string_or_json_literal
    ->
    "expect int or string literal or json literal ({json||json}) "
  | Unhandled_poly_type 
    -> 
    "Unhandled poly type"
  | Unregistered str 
    -> "Unregistered " ^ str 
  | Invalid_underscore_type_in_external
    ->
    "_ is not allowed in combination with external optional type"
  | Invalid_bs_string_type
    -> 
    "Not a valid type for [@bs.string]"
  | Invalid_bs_int_type 
    -> 
    "Not a valid type for [@bs.int]"
  | Invalid_bs_unwrap_type
    ->
    "Not a valid type for [@bs.unwrap]. Type must be an inline variant (closed), and\n\
     each constructor must have an argument."
  | Conflict_ffi_attribute str
    ->
    "Conflicting FFI attributes found: " ^ str
  | Bs_this_simple_pattern
    -> 
    "[@bs.this] expect its pattern variable to be simple form"

type exn +=  Error of Location.t * error


let () = 
  Location.register_error_of_exn (function
    | Error(loc,err) -> 
      Some (Location.error_of_printer loc pp_error err)
    | _ -> None
    )

let err loc error = raise (Error(loc, error))

end
module Ast_core_type : sig 
#1 "ast_core_type.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.core_type


val extract_option_type_exn : t -> t
val extract_option_type : t -> t option

val lift_option_type : t -> t
val is_any : t -> bool
val replace_result : t -> t -> t

val opt_arrow: Location.t -> string -> t -> t -> t

val is_unit : t -> bool
val is_array : t -> bool
type arg_label =
  | Label of string
  | Optional of string
  | Empty


(** for
       [x:t] -> "x"
       [?x:t] -> "?x"
*)
val label_name : string -> arg_label





(** return a function type
    [from_labels ~loc tyvars labels]
    example output:
    {[x:'a0 -> y:'a1 -> < x :'a0 ;y :'a1  > Js.t]}
*)
val from_labels :
  loc:Location.t -> int ->  string Asttypes.loc list -> t

val make_obj :
  loc:Location.t ->
  (string * Parsetree.attributes * t) list ->
  t

val is_user_option : t -> bool

val is_user_bool : t -> bool

val is_user_int : t -> bool

val is_optional_label : string -> bool

(**
  returns 0 when it can not tell arity from the syntax
*)
val get_uncurry_arity : t -> [`Arity of int | `Not_function ]


(** fails when Ptyp_poly *)
val list_of_arrow :
  t ->
  t *  (Asttypes.label * t * Parsetree.attributes * Location.t) list

val is_arity_one : t -> bool

end = struct
#1 "ast_core_type.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.core_type

type arg_label =
  | Label of string
  | Optional of string
  | Empty (* it will be ignored , side effect will be recorded *)



let extract_option_type_exn (ty : t) =
  begin match ty with
    | {ptyp_desc =
         Ptyp_constr
           ({txt =
               Ldot (Lident "*predef*", "option")
               | Lident "option"
            },
            [ty])}
      ->
      ty
    | _ -> assert false
  end

let extract_option_type (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr(
    {txt = (Ldot (Lident "*predef*", "option")
      | Lident "option")},
     [ty]) -> Some ty
  | _ -> None

let predef_option : Longident.t =
  Longident.Ldot (Lident "*predef*", "option")

let predef_int : Longident.t =
  Ldot (Lident "*predef*", "int")


let lift_option_type ({ptyp_loc} as ty:t) : t =
  {ptyp_desc =
     Ptyp_constr(
       {txt = predef_option;
        loc = ptyp_loc}
        , [ty]);
        ptyp_loc = ptyp_loc;
      ptyp_attributes = []
    }

let is_any (ty : t) =
  ty.ptyp_desc = Ptyp_any

open Ast_helper

let replace_result (ty : t) (result : t) : t =
  let rec aux (ty : Parsetree.core_type) =
    match ty with
    | { ptyp_desc =
          Ptyp_arrow (label,t1,t2)
      } -> { ty with ptyp_desc = Ptyp_arrow(label,t1, aux t2)}
    | {ptyp_desc = Ptyp_poly(fs,ty)}
      ->  {ty with ptyp_desc = Ptyp_poly(fs, aux ty)}
    | _ -> result in
  aux ty

let is_unit (ty : t ) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt =Lident "unit"}, []) -> true
  | _ -> false

let is_array (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt =Lident "array"}, [_]) -> true
  | _ -> false

let is_user_option (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr(
    {txt = Lident "option" |
     (Ldot (Lident "*predef*", "option")) },
    [_]) -> true
  | _ -> false

let is_user_bool (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt = Lident "bool"},[]) -> true
  | _ -> false

let is_user_int (ty : t) =
  match ty.ptyp_desc with
  | Ptyp_constr({txt = Lident "int"},[]) -> true
  | _ -> false

let is_optional_label l =
  String.length l > 0 && l.[0] = '?'

let label_name l : arg_label =
  if l = "" then Empty else
  if is_optional_label l
  then Optional (String.sub l 1 (String.length l - 1))
  else Label l


(* Note that OCaml type checker will not allow arbitrary
   name as type variables, for example:
   {[
     '_x'_
   ]}
   will be recognized as a invalid program
*)
let from_labels ~loc arity labels
  : t =
  let tyvars =
    ((Ext_list.init arity (fun i ->
         Typ.var ~loc ("a" ^ string_of_int i)))) in
  let result_type =
    Ast_comb.to_js_type loc
      (Typ.object_ ~loc
         (Ext_list.map2 (fun x y -> x.Asttypes.txt ,[], y) labels tyvars) Closed)
  in
  Ext_list.fold_right2
    (fun {Asttypes.loc ; txt = label }
      tyvar acc -> Typ.arrow ~loc label tyvar acc) labels tyvars  result_type


let make_obj ~loc xs =
  Ast_comb.to_js_type loc
    (Ast_helper.Typ.object_  ~loc xs Closed)


let opt_arrow loc label ty1 ty2 =
  Typ.arrow ~loc ("?" ^ label) ty1 ty2
(**

{[ 'a . 'a -> 'b ]}
OCaml does not support such syntax yet
{[ 'a -> ('a. 'a -> 'b) ]}

*)
let rec get_uncurry_arity_aux  (ty : t) acc =
    match ty.ptyp_desc with
    | Ptyp_arrow(_, _ , new_ty) ->
      get_uncurry_arity_aux new_ty (succ acc)
    | Ptyp_poly (_,ty) ->
      get_uncurry_arity_aux ty acc
    | _ -> acc

(**
   {[ unit -> 'a1 -> a2']}  arity 2
   {[ unit -> 'b ]} return arity 0
   {[ 'a1 -> 'a2 -> ... 'aN -> 'b ]} return arity N
*)
let get_uncurry_arity (ty : t ) =
  match ty.ptyp_desc  with
  | Ptyp_arrow("", {ptyp_desc = (Ptyp_constr ({txt = Lident "unit"}, []))},
    ({ptyp_desc = Ptyp_arrow _ } as rest  )) -> `Arity (get_uncurry_arity_aux rest 1 )
  | Ptyp_arrow("", {ptyp_desc = (Ptyp_constr ({txt = Lident "unit"}, []))}, _) -> `Arity 0
  | Ptyp_arrow(_,_,rest ) ->
    `Arity(get_uncurry_arity_aux rest 1)
  | _ -> `Not_function

let get_curry_arity  ty =
  get_uncurry_arity_aux ty 0

let is_arity_one ty = get_curry_arity ty =  1

let list_of_arrow (ty : t) =
  let rec aux (ty : t) acc =
    match ty.ptyp_desc with
    | Ptyp_arrow(label,t1,t2) ->
      aux t2 ((label,t1,ty.ptyp_attributes,ty.ptyp_loc) ::acc)
    | Ptyp_poly(_, ty) -> (* should not happen? *)
      Bs_syntaxerr.err ty.ptyp_loc Unhandled_poly_type
    | return_type -> ty, List.rev acc
  in aux ty []

end
module Bs_ast_iterator : sig 
#1 "bs_ast_iterator.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {!iterator} allows to implement AST inspection using open recursion.  A
    typical mapper would be based on {!default_iterator}, a trivial iterator,
    and will fall back on it for handling the syntax it does not modify. *)

open Parsetree

(** {2 A generic Parsetree iterator} *)

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  class_declaration: iterator -> class_declaration -> unit;
  class_description: iterator -> class_description -> unit;
  class_expr: iterator -> class_expr -> unit;
  class_field: iterator -> class_field -> unit;
  class_signature: iterator -> class_signature -> unit;
  class_structure: iterator -> class_structure -> unit;
  class_type: iterator -> class_type -> unit;
  class_type_declaration: iterator -> class_type_declaration -> unit;
  class_type_field: iterator -> class_type_field -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

val default_iterator: iterator
(** A default iterator, which implements a "do not do anything" mapping. *)

end = struct
#1 "bs_ast_iterator.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+9"]
(* A generic Parsetree mapping class *)
(* Back-ported from 4.04 By Hongbo ZHang, after grading to 4.04, we will remove this file  *)
(*

  (* Ensure that record patterns don't miss any field. *)
*)


open Parsetree
open Location

type iterator = {
  attribute: iterator -> attribute -> unit;
  attributes: iterator -> attribute list -> unit;
  case: iterator -> case -> unit;
  cases: iterator -> case list -> unit;
  class_declaration: iterator -> class_declaration -> unit;
  class_description: iterator -> class_description -> unit;
  class_expr: iterator -> class_expr -> unit;
  class_field: iterator -> class_field -> unit;
  class_signature: iterator -> class_signature -> unit;
  class_structure: iterator -> class_structure -> unit;
  class_type: iterator -> class_type -> unit;
  class_type_declaration: iterator -> class_type_declaration -> unit;
  class_type_field: iterator -> class_type_field -> unit;
  constructor_declaration: iterator -> constructor_declaration -> unit;
  expr: iterator -> expression -> unit;
  extension: iterator -> extension -> unit;
  extension_constructor: iterator -> extension_constructor -> unit;
  include_declaration: iterator -> include_declaration -> unit;
  include_description: iterator -> include_description -> unit;
  label_declaration: iterator -> label_declaration -> unit;
  location: iterator -> Location.t -> unit;
  module_binding: iterator -> module_binding -> unit;
  module_declaration: iterator -> module_declaration -> unit;
  module_expr: iterator -> module_expr -> unit;
  module_type: iterator -> module_type -> unit;
  module_type_declaration: iterator -> module_type_declaration -> unit;
  open_description: iterator -> open_description -> unit;
  pat: iterator -> pattern -> unit;
  payload: iterator -> payload -> unit;
  signature: iterator -> signature -> unit;
  signature_item: iterator -> signature_item -> unit;
  structure: iterator -> structure -> unit;
  structure_item: iterator -> structure_item -> unit;
  typ: iterator -> core_type -> unit;
  type_declaration: iterator -> type_declaration -> unit;
  type_extension: iterator -> type_extension -> unit;
  type_kind: iterator -> type_kind -> unit;
  value_binding: iterator -> value_binding -> unit;
  value_description: iterator -> value_description -> unit;
  with_constraint: iterator -> with_constraint -> unit;
}
(** A [iterator] record implements one "method" per syntactic category,
    using an open recursion style: each method takes as its first
    argument the iterator to be applied to children in the syntax
    tree. *)

let iter_fst f (x, _) = f x
let iter_snd f (_, y) = f y
let iter_tuple f1 f2 (x, y) = f1 x; f2 y
let iter_tuple3 f1 f2 f3 (x, y, z) = f1 x; f2 y; f3 z
let iter_opt f = function None -> () | Some x -> f x

let iter_loc sub {loc; txt = _} = sub.location sub loc

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (_, attrs, _, tl) ->
        sub.attributes sub attrs; List.iter (sub.typ sub) tl
    | Rinherit t -> sub.typ sub t

  let iter sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ptyp_any
    | Ptyp_var _ -> ()
    | Ptyp_arrow (_lab, t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Ptyp_tuple tyl -> List.iter (sub.typ sub) tyl
    | Ptyp_constr (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_object (l, _o) ->
        let f (_, a, t) = sub.attributes sub a; sub.typ sub t in
        List.iter f l
    | Ptyp_class (lid, tl) ->
        iter_loc sub lid; List.iter (sub.typ sub) tl
    | Ptyp_alias (t, _) -> sub.typ sub t
    | Ptyp_variant (rl, _b, _ll) ->
        List.iter (row_field sub) rl
    | Ptyp_poly (_, t) -> sub.typ sub t
    | Ptyp_package (lid, l) ->
        iter_loc sub lid;
        List.iter (iter_tuple (iter_loc sub) (sub.typ sub)) l
    | Ptyp_extension x -> sub.extension sub x

  let iter_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private = _;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    iter_loc sub ptype_name;
    List.iter (iter_fst (sub.typ sub)) ptype_params;
    List.iter
      (iter_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
      ptype_cstrs;
    sub.type_kind sub ptype_kind;
    iter_opt (sub.typ sub) ptype_manifest;
    sub.location sub ptype_loc;
    sub.attributes sub ptype_attributes

  let iter_type_kind sub = function
    | Ptype_abstract -> ()
    | Ptype_variant l ->
        List.iter (sub.constructor_declaration sub) l
    | Ptype_record l -> List.iter (sub.label_declaration sub) l
    | Ptype_open -> ()

  let iter_constructor_arguments sub l = List.iter (sub.typ sub) l
      (*# no inline record in 4.02.3*)
  let iter_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private = _;
       ptyext_attributes} =
    iter_loc sub ptyext_path;
    List.iter (sub.extension_constructor sub) ptyext_constructors;
    List.iter (iter_fst (sub.typ sub)) ptyext_params;
    sub.attributes sub ptyext_attributes

  let iter_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        iter_constructor_arguments sub ctl; iter_opt (sub.typ sub) cto
    | Pext_rebind li ->
        iter_loc sub li

  let iter_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    iter_loc sub pext_name;
    iter_extension_constructor_kind sub pext_kind;
    sub.location sub pext_loc;
    sub.attributes sub pext_attributes

end

module CT = struct
  (* Type expressions for the class language *)

  let iter sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcty_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcty_signature x -> sub.class_signature sub x
    | Pcty_arrow (_lab, t, ct) ->
        sub.typ sub t; sub.class_type sub ct
    | Pcty_extension x -> sub.extension sub x

  let iter_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pctf_inherit ct -> sub.class_type sub ct
    | Pctf_val (_s, _m, _v, t) -> sub.typ sub t
    | Pctf_method (_s, _p, _v, t) -> sub.typ sub t
    | Pctf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pctf_attribute x -> sub.attribute sub x
    | Pctf_extension x -> sub.extension sub x

  let iter_signature sub {pcsig_self; pcsig_fields} =
    sub.typ sub pcsig_self;
    List.iter (sub.class_type_field sub) pcsig_fields
end

module MT = struct
  (* Type expressions for the module language *)

  let iter sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmty_ident s -> iter_loc sub s
    | Pmty_alias s -> iter_loc sub s
    | Pmty_signature sg -> sub.signature sub sg
    | Pmty_functor (s, mt1, mt2) ->
        iter_loc sub s;
        iter_opt (sub.module_type sub) mt1;
        sub.module_type sub mt2
    | Pmty_with (mt, l) ->
        sub.module_type sub mt;
        List.iter (sub.with_constraint sub) l
    | Pmty_typeof me -> sub.module_expr sub me
    | Pmty_extension x -> sub.extension sub x

  let iter_with_constraint sub = function
    | Pwith_type (lid, d) ->
        iter_loc sub lid; sub.type_declaration sub d
    | Pwith_module (lid, lid2) ->
        iter_loc sub lid; iter_loc sub lid2
    | Pwith_typesubst d -> sub.type_declaration sub d
    | Pwith_modsubst (s, lid) ->
        iter_loc sub s; iter_loc sub lid

  let iter_signature_item sub {psig_desc = desc; psig_loc = loc} =
    sub.location sub loc;
    match desc with
    | Psig_value vd -> sub.value_description sub vd
    | Psig_type ( l) -> List.iter (sub.type_declaration sub) l
    (*#2 no rec_flag in 4.02.3*)
    | Psig_typext te -> sub.type_extension sub te
    | Psig_exception ed -> sub.extension_constructor sub ed
    | Psig_module x -> sub.module_declaration sub x
    | Psig_recmodule l ->
        List.iter (sub.module_declaration sub) l
    | Psig_modtype x -> sub.module_type_declaration sub x
    | Psig_open x -> sub.open_description sub x
    | Psig_include x -> sub.include_description sub x
    | Psig_class l -> List.iter (sub.class_description sub) l
    | Psig_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Psig_extension (x, attrs) ->
        sub.extension sub x; sub.attributes sub attrs
    | Psig_attribute x -> sub.attribute sub x
end


module M = struct
  (* Value expressions for the module language *)

  let iter sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pmod_ident x -> iter_loc sub x
    | Pmod_structure str -> sub.structure sub str
    | Pmod_functor (arg, arg_ty, body) ->
        iter_loc sub arg;
        iter_opt (sub.module_type sub) arg_ty;
        sub.module_expr sub body
    | Pmod_apply (m1, m2) ->
        sub.module_expr sub m1; sub.module_expr sub m2
    | Pmod_constraint (m, mty) ->
        sub.module_expr sub m; sub.module_type sub mty
    | Pmod_unpack e -> sub.expr sub e
    | Pmod_extension x -> sub.extension sub x

  let iter_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    sub.location sub loc;
    match desc with
    | Pstr_eval (x, attrs) ->
        sub.expr sub x; sub.attributes sub attrs
    | Pstr_value (_r, vbs) -> List.iter (sub.value_binding sub) vbs
    | Pstr_primitive vd -> sub.value_description sub vd
    | Pstr_type ( l) -> List.iter (sub.type_declaration sub) l
    (*#3 no rec flag in 4.02.3*)
    | Pstr_typext te -> sub.type_extension sub te
    | Pstr_exception ed -> sub.extension_constructor sub ed
    | Pstr_module x -> sub.module_binding sub x
    | Pstr_recmodule l -> List.iter (sub.module_binding sub) l
    | Pstr_modtype x -> sub.module_type_declaration sub x
    | Pstr_open x -> sub.open_description sub x
    | Pstr_class l -> List.iter (sub.class_declaration sub) l
    | Pstr_class_type l ->
        List.iter (sub.class_type_declaration sub) l
    | Pstr_include x -> sub.include_declaration sub x
    | Pstr_extension (x, attrs) ->
        sub.extension sub x; sub.attributes sub attrs
    | Pstr_attribute x -> sub.attribute sub x
end

module E = struct
  (* Value expressions for the core language *)

  let iter sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pexp_ident x -> iter_loc sub x
    | Pexp_constant _ -> ()
    | Pexp_let (_r, vbs, e) ->
        List.iter (sub.value_binding sub) vbs;
        sub.expr sub e
    | Pexp_fun (_lab, def, p, e) ->
        iter_opt (sub.expr sub) def;
        sub.pat sub p;
        sub.expr sub e
    | Pexp_function pel -> sub.cases sub pel
    | Pexp_apply (e, l) ->
        sub.expr sub e; List.iter (iter_snd (sub.expr sub)) l
    | Pexp_match (e, pel) ->
        sub.expr sub e; sub.cases sub pel
    | Pexp_try (e, pel) -> sub.expr sub e; sub.cases sub pel
    | Pexp_tuple el -> List.iter (sub.expr sub) el
    | Pexp_construct (lid, arg) ->
        iter_loc sub lid; iter_opt (sub.expr sub) arg
    | Pexp_variant (_lab, eo) ->
        iter_opt (sub.expr sub) eo
    | Pexp_record (l, eo) ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) l;
        iter_opt (sub.expr sub) eo
    | Pexp_field (e, lid) ->
        sub.expr sub e; iter_loc sub lid
    | Pexp_setfield (e1, lid, e2) ->
        sub.expr sub e1; iter_loc sub lid;
        sub.expr sub e2
    | Pexp_array el -> List.iter (sub.expr sub) el
    | Pexp_ifthenelse (e1, e2, e3) ->
        sub.expr sub e1; sub.expr sub e2;
        iter_opt (sub.expr sub) e3
    | Pexp_sequence (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_while (e1, e2) ->
        sub.expr sub e1; sub.expr sub e2
    | Pexp_for (p, e1, e2, _d, e3) ->
        sub.pat sub p; sub.expr sub e1; sub.expr sub e2;
        sub.expr sub e3
    | Pexp_coerce (e, t1, t2) ->
        sub.expr sub e; iter_opt (sub.typ sub) t1;
        sub.typ sub t2
    | Pexp_constraint (e, t) ->
        sub.expr sub e; sub.typ sub t
    | Pexp_send (e, _s) -> sub.expr sub e
    | Pexp_new lid -> iter_loc sub lid
    | Pexp_setinstvar (s, e) ->
        iter_loc sub s; sub.expr sub e
    | Pexp_override sel ->
        List.iter (iter_tuple (iter_loc sub) (sub.expr sub)) sel
    | Pexp_letmodule (s, me, e) ->
        iter_loc sub s; sub.module_expr sub me;
        sub.expr sub e
    (* | Pexp_letexception (cd, e) -> *)
    (*     sub.extension_constructor sub cd; *)
    (*     sub.expr sub e *)
    (* no local exception *)
    | Pexp_assert e -> sub.expr sub e
    | Pexp_lazy e -> sub.expr sub e
    | Pexp_poly (e, t) ->
        sub.expr sub e; iter_opt (sub.typ sub) t
    | Pexp_object cls -> sub.class_structure sub cls
    | Pexp_newtype (_s, e) -> sub.expr sub e
    | Pexp_pack me -> sub.module_expr sub me
    | Pexp_open (_ovf, lid, e) ->
        iter_loc sub lid; sub.expr sub e
    | Pexp_extension x -> sub.extension sub x
    (* | Pexp_unreachable -> () *)
end

module P = struct
  (* Patterns *)

  let iter sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Ppat_any -> ()
    | Ppat_var s -> iter_loc sub s
    | Ppat_alias (p, s) -> sub.pat sub p; iter_loc sub s
    | Ppat_constant _ -> ()
    | Ppat_interval _ -> ()
    | Ppat_tuple pl -> List.iter (sub.pat sub) pl
    | Ppat_construct (l, p) ->
        iter_loc sub l; iter_opt (sub.pat sub) p
    | Ppat_variant (_l, p) -> iter_opt (sub.pat sub) p
    | Ppat_record (lpl, _cf) ->
        List.iter (iter_tuple (iter_loc sub) (sub.pat sub)) lpl
    | Ppat_array pl -> List.iter (sub.pat sub) pl
    | Ppat_or (p1, p2) -> sub.pat sub p1; sub.pat sub p2
    | Ppat_constraint (p, t) ->
        sub.pat sub p; sub.typ sub t
    | Ppat_type s -> iter_loc sub s
    | Ppat_lazy p -> sub.pat sub p
    | Ppat_unpack s -> iter_loc sub s
    | Ppat_exception p -> sub.pat sub p
    | Ppat_extension x -> sub.extension sub x
    (* | Ppat_open (lid, p) -> *)
    (*     iter_loc sub lid; sub.pat sub p *)

end

module CE = struct
  (* Value expressions for the class language *)

  let iter sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcl_constr (lid, tys) ->
        iter_loc sub lid; List.iter (sub.typ sub) tys
    | Pcl_structure s ->
        sub.class_structure sub s
    | Pcl_fun (_lab, e, p, ce) ->
        iter_opt (sub.expr sub) e;
        sub.pat sub p;
        sub.class_expr sub ce
    | Pcl_apply (ce, l) ->
        sub.class_expr sub ce;
        List.iter (iter_snd (sub.expr sub)) l
    | Pcl_let (_r, vbs, ce) ->
        List.iter (sub.value_binding sub) vbs;
        sub.class_expr sub ce
    | Pcl_constraint (ce, ct) ->
        sub.class_expr sub ce; sub.class_type sub ct
    | Pcl_extension x -> sub.extension sub x

  let iter_kind sub = function
    | Cfk_concrete (_o, e) -> sub.expr sub e
    | Cfk_virtual t -> sub.typ sub t

  let iter_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    sub.location sub loc;
    sub.attributes sub attrs;
    match desc with
    | Pcf_inherit (_o, ce, _s) -> sub.class_expr sub ce
    | Pcf_val (s, _m, k) -> iter_loc sub s; iter_kind sub k
    | Pcf_method (s, _p, k) ->
        iter_loc sub s; iter_kind sub k
    | Pcf_constraint (t1, t2) ->
        sub.typ sub t1; sub.typ sub t2
    | Pcf_initializer e -> sub.expr sub e
    | Pcf_attribute x -> sub.attribute sub x
    | Pcf_extension x -> sub.extension sub x

  let iter_structure sub {pcstr_self; pcstr_fields} =
    sub.pat sub pcstr_self;
    List.iter (sub.class_field sub) pcstr_fields

  let class_infos sub f {pci_virt = _; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    List.iter (iter_fst (sub.typ sub)) pl;
    iter_loc sub pci_name;
    f pci_expr;
    sub.location sub pci_loc;
    sub.attributes sub pci_attributes
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_iterator =
  {
    structure = (fun this l -> List.iter (this.structure_item this) l);
    structure_item = M.iter_structure_item;
    module_expr = M.iter;
    signature = (fun this l -> List.iter (this.signature_item this) l);
    signature_item = MT.iter_signature_item;
    module_type = MT.iter;
    with_constraint = MT.iter_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.iter;
    class_field = CE.iter_field;
    class_structure = CE.iter_structure;
    class_type = CT.iter;
    class_type_field = CT.iter_field;
    class_signature = CT.iter_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.iter_type_declaration;
    type_kind = T.iter_type_kind;
    typ = T.iter;
    type_extension = T.iter_type_extension;
    extension_constructor = T.iter_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim = _; pval_loc;
                 pval_attributes} ->
        iter_loc this pval_name;
        this.typ this pval_type;
        this.attributes this pval_attributes;
        this.location this pval_loc
      );

    pat = P.iter;
    expr = E.iter;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         iter_loc this pmd_name;
         this.module_type this pmd_type;
         this.attributes this pmd_attributes;
         this.location this pmd_loc
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         iter_loc this pmtd_name;
         iter_opt (this.module_type this) pmtd_type;
         this.attributes this pmtd_attributes;
         this.location this pmtd_loc
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         iter_loc this pmb_name; this.module_expr this pmb_expr;
         this.attributes this pmb_attributes;
         this.location this pmb_loc
      );


    open_description =
      (fun this {popen_lid; popen_override = _; popen_attributes; popen_loc} ->
         iter_loc this popen_lid;
         this.location this popen_loc;
         this.attributes this popen_attributes
      );


    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_type this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         this.module_expr this pincl_mod;
         this.location this pincl_loc;
         this.attributes this pincl_attributes
      );


    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         this.pat this pvb_pat;
         this.expr this pvb_expr;
         this.location this pvb_loc;
         this.attributes this pvb_attributes
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
         iter_loc this pcd_name;
         T.iter_constructor_arguments this pcd_args;
         iter_opt (this.typ this) pcd_res;
         this.location this pcd_loc;
         this.attributes this pcd_attributes
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable = _; pld_attributes}->
         iter_loc this pld_name;
         this.typ this pld_type;
         this.location this pld_loc;
         this.attributes this pld_attributes
      );

    cases = (fun this l -> List.iter (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         this.pat this pc_lhs;
         iter_opt (this.expr this) pc_guard;
         this.expr this pc_rhs
      );

    location = (fun _this _l -> ());

    extension = (fun this (s, e) -> iter_loc this s; this.payload this e);
    attribute = (fun this (s, e) -> iter_loc this s; this.payload this e);
    attributes = (fun this l -> List.iter (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> this.structure this x
         (* | PSig x -> this.signature this x *)
         | PTyp x -> this.typ this x
         | PPat (x, g) -> this.pat this x; iter_opt (this.expr this) g
      );
  }

end
module Js_config : sig 
#1 "js_config.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





(* val get_packages_info :
   unit -> Js_packages_info.t *)


(** set/get header *)
val no_version_header : bool ref 


(** return [package_name] and [path] 
    when in script mode: 
*)

(* val get_current_package_name_and_path : 
  Js_packages_info.module_system -> 
  Js_packages_info.info_query *)


(* val set_package_name : string -> unit  
val get_package_name : unit -> string option *)

(** cross module inline option *)
val cross_module_inline : bool ref
val set_cross_module_inline : bool -> unit
val get_cross_module_inline : unit -> bool
  
(** diagnose option *)
val diagnose : bool ref 
val get_diagnose : unit -> bool 
val set_diagnose : bool -> unit 


(** options for builtin ppx *)
val no_builtin_ppx_ml : bool ref 
val no_builtin_ppx_mli : bool ref 



val no_warn_unimplemented_external : bool ref 

(** check-div-by-zero option *)
val check_div_by_zero : bool ref 
val get_check_div_by_zero : unit -> bool 





(** Debugging utilies *)
val set_current_file : string -> unit 
val get_current_file : unit -> string
val get_module_name : unit -> string

val iset_debug_file : string -> unit
val set_debug_file : string -> unit
val get_debug_file : unit -> string

val is_same_file : unit -> bool 

val tool_name : string


val sort_imports : bool ref 
val dump_js : bool ref
val syntax_only  : bool ref
val binary_ast : bool ref


val bs_suffix : bool ref
val debug : bool ref
end = struct
#1 "js_config.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






(* let add_npm_package_path s =
  match !packages_info  with
  | Empty ->
    Ext_pervasives.bad_argf "please set package name first using -bs-package-name ";
  | NonBrowser(name,  envs) ->
    let env, path =
      match Ext_string.split ~keep_empty:false s ':' with
      | [ package_name; path]  ->
        (match Js_packages_info.module_system_of_string package_name with
         | Some x -> x
         | None ->
           Ext_pervasives.bad_argf "invalid module system %s" package_name), path
      | [path] ->
        NodeJS, path
      | _ ->
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    packages_info := NonBrowser (name,  ((env,path) :: envs)) *)
(** Browser is not set via command line only for internal use *)


let no_version_header = ref false

let cross_module_inline = ref false

let get_cross_module_inline () = !cross_module_inline
let set_cross_module_inline b =
  cross_module_inline := b


let diagnose = ref false
let get_diagnose () = !diagnose
let set_diagnose b = diagnose := b

let (//) = Filename.concat

(* let get_packages_info () = !packages_info *)

let no_builtin_ppx_ml = ref false
let no_builtin_ppx_mli = ref false


(** TODO: will flip the option when it is ready *)
let no_warn_unimplemented_external = ref false 
let current_file = ref ""
let debug_file = ref ""

let set_current_file f  = current_file := f
let get_current_file () = !current_file
let get_module_name () =
  Filename.chop_extension
    (Filename.basename (String.uncapitalize !current_file))

let iset_debug_file _ = ()
let set_debug_file  f = debug_file := f
let get_debug_file  () = !debug_file


let is_same_file () =
  !debug_file <> "" &&  !debug_file = !current_file

let tool_name = "BuckleScript"

let check_div_by_zero = ref true
let get_check_div_by_zero () = !check_div_by_zero




let sort_imports = ref true
let dump_js = ref false



let syntax_only = ref false
let binary_ast = ref false

let bs_suffix = ref false 

let debug = ref false
end
module Bs_warnings : sig 
#1 "bs_warnings.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type t =
  | Unsafe_poly_variant_type

val prerr_bs_ffi_warning : Location.t -> t -> unit


val warn_missing_primitive : Location.t -> string -> unit 

val error_unescaped_delimiter : 
  Location.t -> string  -> unit 
end = struct
#1 "bs_warnings.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



type t =
  | Unsafe_poly_variant_type
  (* for users write code like this:
     {[ external f : [`a of int ] -> string = ""]}
     Here users forget about `[@bs.string]` or `[@bs.int]`
  *)    



let to_string t =
  match t with
  | Unsafe_poly_variant_type 
    -> 
    "Here a OCaml polymorphic variant type passed into JS, probably you forgot annotations like `[@bs.int]` or `[@bs.string]`  "

let warning_formatter = Format.err_formatter

let print_string_warning (loc : Location.t) x =   
  if loc.loc_ghost then 
    Format.fprintf warning_formatter "File %s@." 
      (Js_config.get_current_file ())
  else 
    Location.print warning_formatter loc ; 
  Format.fprintf warning_formatter "@{<error>Warning@}: %s@." x 

let prerr_bs_ffi_warning loc x =  
    Location.prerr_warning loc (Warnings.Bs_ffi_warning (to_string x))

let unimplemented_primitive = "Unimplemented primitive used:" 
type error = 
  | Uninterpreted_delimiters of string
  | Unimplemented_primitive of string 
exception  Error of Location.t * error

let pp_error fmt x =
  match x with 
  | Unimplemented_primitive str -> 
    Format.pp_print_string fmt unimplemented_primitive;
    Format.pp_print_string fmt str
  
  | Uninterpreted_delimiters str -> 
    Format.pp_print_string fmt "Uninterpreted delimiters" ;
    Format.pp_print_string fmt str



let () = 
  Location.register_error_of_exn (function 
      | Error (loc,err) -> 
        Some (Location.error_of_printer loc pp_error err)
      | _ -> None
    )




let warn_missing_primitive loc txt =      
  if not @@ !Js_config.no_warn_unimplemented_external then
    begin 
      print_string_warning loc ( unimplemented_primitive ^ txt ^ " \n" );
      Format.pp_print_flush warning_formatter ()
    end



let error_unescaped_delimiter loc txt = 
  raise (Error(loc, Uninterpreted_delimiters txt))






(**
   Note the standard way of reporting error in compiler:

   val Location.register_error_of_exn : (exn -> Location.error option) -> unit 
   val Location.error_of_printer : Location.t ->
   (Format.formatter -> error -> unit) -> error -> Location.error

   Define an error type

   type error 
   exception Error of Location.t * error 

   Provide a printer to error

   {[
     let () = 
       Location.register_error_of_exn
         (function 
           | Error(loc,err) -> 
             Some (Location.error_of_printer loc pp_error err)
           | _ -> None
         )
   ]}
*)

end
module Ext_util : sig 
#1 "ext_util.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


 
val power_2_above : int -> int -> int


val stats_to_string : Hashtbl.statistics -> string 
end = struct
#1 "ext_util.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(**
   {[
     (power_2_above 16 63 = 64)
       (power_2_above 16 76 = 128)
   ]}
*)
let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n


let stats_to_string ({num_bindings; num_buckets; max_bucket_length; bucket_histogram} : Hashtbl.statistics) = 
  Printf.sprintf 
    "bindings: %d,buckets: %d, longest: %d, hist:[%s]" 
    num_bindings 
    num_buckets 
    max_bucket_length
    (String.concat "," (Array.to_list (Array.map string_of_int bucket_histogram)))
end
module Hash_set_gen
= struct
#1 "hash_set_gen.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a t =
  { mutable size: int;                        (* number of entries *)
    mutable data: 'a list array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }




let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; data = Array.make s [] }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i  []
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size [ ]


let copy h = { h with data = Array.copy h.data }

let length h = h.size

let iter f h =
  let rec do_bucket = function
    | [ ] ->
      ()
    | k ::  rest ->
      f k ; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      [ ] ->
      accu
    | k ::  rest ->
      do_bucket rest (f k  accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize [ ] in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        [ ] -> ()
      | key :: rest ->
        let nidx = indexfun h key in
        ndata.(nidx) <- key :: ndata.(nidx);
        insert_bucket rest
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done
  end

let elements set = 
  fold  (fun k  acc ->  k :: acc) set []




let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (List.length b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
       let l = List.length b in
       histo.(l) <- histo.(l) + 1)
    h.data;
  {Hashtbl.num_bindings = h.size;
   num_buckets = Array.length h.data;
   max_bucket_length = mbl;
   bucket_histogram = histo }

let rec small_bucket_mem eq_key key lst =
  match lst with 
  | [] -> false 
  | key1::rest -> 
    eq_key key   key1 ||
    match rest with 
    | [] -> false 
    | key2 :: rest -> 
      eq_key key   key2 ||
      match rest with 
      | [] -> false 
      | key3 :: rest -> 
        eq_key key   key3 ||
        small_bucket_mem eq_key key rest 

let rec remove_bucket eq_key key (h : _ t) buckets = 
  match buckets with 
  | [ ] ->
    [ ]
  | k :: next ->
    if  eq_key k   key
    then begin h.size <- h.size - 1; next end
    else k :: remove_bucket eq_key key h next    

module type S =
sig
  type key
  type t
  val create: int ->  t
  val clear : t -> unit
  val reset : t -> unit
  val copy: t -> t
  val remove:  t -> key -> unit
  val add :  t -> key -> unit
  val of_array : key array -> t 
  val check_add : t -> key -> bool
  val mem :  t -> key -> bool
  val iter: (key -> unit) ->  t -> unit
  val fold: (key -> 'b -> 'b) ->  t -> 'b -> 'b
  val length:  t -> int
  val stats:  t -> Hashtbl.statistics
  val elements : t -> key list 
end

end
module Hash_set_poly : sig 
#1 "hash_set_poly.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type   'a t 

val create : int -> 'a t

val clear : 'a t -> unit

val reset : 'a t -> unit

val copy : 'a t -> 'a t

val add : 'a t -> 'a  -> unit
val remove : 'a t -> 'a -> unit

val mem : 'a t -> 'a -> bool

val iter : ('a -> unit) -> 'a t -> unit

val elements : 'a t -> 'a list

val length : 'a t -> int 

val stats:  'a t -> Hashtbl.statistics

end = struct
#1 "hash_set_poly.ml"
# 1 "ext/hash_set.cppo.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
# 51
external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" "noalloc"
let key_index (h :  _ Hash_set_gen.t ) (key : 'a) =
  seeded_hash_param 10 100 0 key land (Array.length h.data - 1)
let eq_key = (=)
type  'a t = 'a Hash_set_gen.t 


# 62
let create = Hash_set_gen.create
let clear = Hash_set_gen.clear
let reset = Hash_set_gen.reset
let copy = Hash_set_gen.copy
let iter = Hash_set_gen.iter
let fold = Hash_set_gen.fold
let length = Hash_set_gen.length
let stats = Hash_set_gen.stats
let elements = Hash_set_gen.elements



let remove (h : _ Hash_set_gen.t) key =  
  let i = key_index h key in
  let h_data = h.data in
  let old_h_size = h.size in 
  let new_bucket = Hash_set_gen.remove_bucket eq_key key h (Array.unsafe_get h_data i) in
  if old_h_size <> h.size then  
    Array.unsafe_set h_data i new_bucket



let add (h : _ Hash_set_gen.t) key =
  let i = key_index h key  in 
  let h_data = h.data in 
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i (key :: old_bucket);
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h
    end

let of_array arr = 
  let len = Array.length arr in 
  let tbl = create len in 
  for i = 0 to len - 1  do
    add tbl (Array.unsafe_get arr i);
  done ;
  tbl 
  
    
let check_add (h : _ Hash_set_gen.t) key =
  let i = key_index h key  in 
  let h_data = h.data in  
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i  (key :: old_bucket);
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h;
      true 
    end
  else false 


let mem (h :  _ Hash_set_gen.t) key =
  Hash_set_gen.small_bucket_mem eq_key key (Array.unsafe_get h.data (key_index h key)) 

  

end
module Literals : sig 
#1 "literals.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






val js_array_ctor : string 
val js_type_number : string
val js_type_string : string
val js_type_object : string
val js_type_boolean : string
val js_undefined : string
val js_prop_length : string

val param : string
val partial_arg : string
val prim : string

(**temporary varaible used in {!Js_ast_util} *)
val tmp : string 

val create : string 

val app : string
val app_array : string

val runtime : string
val stdlib : string
val imul : string

val setter_suffix : string
val setter_suffix_len : int


val debugger : string
val raw_expr : string
val raw_stmt : string
val raw_function : string
val unsafe_downgrade : string
val fn_run : string
val method_run : string
val fn_method : string
val fn_mk : string

(** callback actually, not exposed to user yet *)
(* val js_fn_runmethod : string *)

val bs_deriving : string
val bs_deriving_dot : string
val bs_type : string

(** nodejs *)

val node_modules : string
val node_modules_length : int
val package_json : string
val bsconfig_json : string
val build_ninja : string

(* Name of the library file created for each external dependency. *)
val library_file : string

val suffix_a : string
val suffix_cmj : string
val suffix_cmo : string
val suffix_cma : string
val suffix_cmi : string
val suffix_cmx : string
val suffix_cmxa : string
val suffix_ml : string
val suffix_mlast : string 
val suffix_mlast_simple : string
val suffix_mliast : string
val suffix_mliast_simple : string
val suffix_mlmap : string
val suffix_mll : string
val suffix_re : string
val suffix_rei : string 

val suffix_d : string
val suffix_mlastd : string
val suffix_mliastd : string

val suffix_mli : string 
val suffix_cmt : string 
val suffix_cmti : string 

val commonjs : string 
val amdjs : string 
val es6 : string 
val es6_global : string
val amdjs_global : string 
val unused_attribute : string 
val dash_nostdlib : string

val reactjs_jsx_ppx_2_exe : string 
val reactjs_jsx_ppx_3_exe : string 
val unescaped_j_delimiter : string 
val escaped_j_delimiter : string 

val unescaped_js_delimiter : string 

val native : string
val bytecode : string
val js : string

val node_sep : string 
val node_parent : string 
val node_current : string 

end = struct
#1 "literals.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







let js_array_ctor = "Array"
let js_type_number = "number"
let js_type_string = "string"
let js_type_object = "object" 
let js_type_boolean = "boolean"
let js_undefined = "undefined"
let js_prop_length = "length"

let prim = "prim"
let param = "param"
let partial_arg = "partial_arg"
let tmp = "tmp"

let create = "create" (* {!Caml_exceptions.create}*)

let app = "_"
let app_array = "app" (* arguments are an array*)

let runtime = "runtime" (* runtime directory *)

let stdlib = "stdlib"

let imul = "imul" (* signed int32 mul *)

let setter_suffix = "#="
let setter_suffix_len = String.length setter_suffix

let debugger = "debugger"
let raw_expr = "raw_expr"
let raw_stmt = "raw_stmt"
let raw_function = "raw_function"
let unsafe_downgrade = "unsafe_downgrade"
let fn_run = "fn_run"
let method_run = "method_run"

let fn_method = "fn_method"
let fn_mk = "fn_mk"
(*let js_fn_runmethod = "js_fn_runmethod"*)

let bs_deriving = "bs.deriving"
let bs_deriving_dot = "bs.deriving."
let bs_type = "bs.type"


(** nodejs *)
let node_modules = "node_modules"
let node_modules_length = String.length "node_modules"
let package_json = "package.json"
let bsconfig_json = "bsconfig.json"
let build_ninja = "build.ninja"

(* Name of the library file created for each external dependency. *)
let library_file = "lib"

let suffix_a = ".a"
let suffix_cmj = ".cmj"
let suffix_cmo = ".cmo"
let suffix_cma = ".cma"
let suffix_cmi = ".cmi"
let suffix_cmx = ".cmx"
let suffix_cmxa = ".cmxa"
let suffix_mll = ".mll"
let suffix_ml = ".ml"
let suffix_mli = ".mli"
let suffix_re = ".re"
let suffix_rei = ".rei"
let suffix_mlmap = ".mlmap"

let suffix_cmt = ".cmt" 
let suffix_cmti = ".cmti" 
let suffix_mlast = ".mlast"
let suffix_mlast_simple = ".mlast_simple"
let suffix_mliast = ".mliast"
let suffix_mliast_simple = ".mliast_simple"
let suffix_d = ".d"
let suffix_mlastd = ".mlast.d"
let suffix_mliastd = ".mliast.d"


let commonjs = "commonjs" 
let amdjs = "amdjs"
let es6 = "es6"
let es6_global = "es6-global"
let amdjs_global = "amdjs-global"
let unused_attribute = "Unused attribute " 
let dash_nostdlib = "-nostdlib"

let reactjs_jsx_ppx_2_exe = "reactjs_jsx_ppx_2.exe"
let reactjs_jsx_ppx_3_exe  = "reactjs_jsx_ppx_3.exe"
let unescaped_j_delimiter = "j"
let unescaped_js_delimiter = "js"
let escaped_j_delimiter =  "*j" (* not user level syntax allowed *)

let native = "native"
let bytecode = "bytecode"
let js = "js"



(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."


end
module Bs_ast_invariant : sig 
#1 "bs_ast_invariant.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

val mark_used_bs_attribute : 
  Parsetree.attribute -> unit 

(** [warn_unused_attributes discarded]
  warn if [discarded] has unused bs attribute
*)  
val warn_unused_attributes :   
  Parsetree.attributes -> unit 
(** Ast invariant checking for detecting errors *)
val emit_external_warnings : Bs_ast_iterator.iterator

end = struct
#1 "bs_ast_invariant.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let is_bs_attribute txt = 
  let len = String.length txt  in
  len >= 2 &&
  (*TODO: check the stringing padding rule, this preciate may not be needed *)
  String.unsafe_get txt 0 = 'b'&& 
  String.unsafe_get txt 1 = 's' &&
  (len = 2 ||
   String.unsafe_get txt 2 = '.'
  )

let used_attributes : Parsetree.attribute Hash_set_poly.t = Hash_set_poly.create 16 

let mark_used_bs_attribute (x : Parsetree.attribute) = 
  Hash_set_poly.add used_attributes x

let warn_unused_attributes attrs = 
  if attrs <> [] then 
    List.iter (fun (({txt; loc}, _) as a : Parsetree.attribute) -> 
        if is_bs_attribute txt && 
           not (Hash_set_poly.mem used_attributes a) then 
          Location.prerr_warning loc (Warnings.Bs_unused_attribute txt)
      ) attrs

let emit_external_warnings : Bs_ast_iterator.iterator=
  {
    Bs_ast_iterator.default_iterator with
    attribute = (fun _ a ->
        match a with
        | {txt ; loc}, _ ->
          if is_bs_attribute txt && 
             not (Hash_set_poly.mem used_attributes a)  then
            Location.prerr_warning loc (Bs_unused_attribute txt)
      );
    expr = (fun self a -> 
        match a.Parsetree.pexp_desc with 
        | Pexp_constant (Const_string (_, Some s)) 
          when Ext_string.equal s Literals.unescaped_j_delimiter 
            || Ext_string.equal s Literals.unescaped_js_delimiter -> 
          Bs_warnings.error_unescaped_delimiter a.pexp_loc s 
        | _ -> Bs_ast_iterator.default_iterator.expr self a 
      );
    value_description =
      (fun self v -> 
         match v with 
         | ( {
             pval_loc;
             pval_prim =
               "%identity"::_;
             pval_type
           } : Parsetree.value_description)
           when not
               (Ast_core_type.is_arity_one pval_type)
           -> 
           Location.raise_errorf
             ~loc:pval_loc
             "%%identity expect its type to be of form 'a -> 'b (arity 1)"
         | _ ->
           Bs_ast_iterator.default_iterator.value_description self v 

      )
  }

end
module Ast_attributes : sig 
#1 "ast_attributes.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
type attr =  Parsetree.attribute
type t =  attr list

type ('a,'b) st =
  { get : 'a option ;
    set : 'b option }

val process_method_attributes_rev :
  t ->
  (bool * bool , [`Get | `No_get ]) st * t

val process_attributes_rev :
  t -> [ `Meth_callback | `Nothing | `Uncurry | `Method ] * t

val process_pexp_fun_attributes_rev :
  t -> [ `Nothing | `Exn ] * t
val process_bs :
  t -> [ `Nothing | `Has] * t

val process_external : t -> bool

type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : Ast_payload.action list option
}


val iter_process_bs_string_int_unwrap_uncurry :
  t -> 
  [`Nothing | `String | `Int | `Ignore | `Unwrap | `Uncurry of int option ]


val iter_process_bs_string_as :
  t -> string option

val has_bs_optional :
  t -> bool 

val iter_process_bs_int_as :
  t -> int option


val iter_process_bs_string_or_int_as :
    t ->
    [ `Int of int
    | `Str of string
    | `Json_str of string  ] option


val process_derive_type :
  t -> derive_attr * t

val iter_process_derive_type :
  t -> derive_attr


val bs : attr
val is_bs : attr -> bool
val bs_this : attr
val bs_method : attr
val bs_obj : attr


val bs_get : attr
val bs_get_arity : attr 
val bs_set : attr
val bs_return_undefined : attr

end = struct
#1 "ast_attributes.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type attr =  Parsetree.attribute
type t =  attr list

type ('a,'b) st =
  { get : 'a option ;
    set : 'b option }


let process_method_attributes_rev (attrs : t) =
  List.fold_left (fun (st,acc) (({txt ; loc}, payload) as attr : attr) ->

      match txt  with
      | "bs.get" (* [@@bs.get{null; undefined}]*)
        ->
        let result =
          List.fold_left
            (fun
              (null, undefined)
              (({txt ; loc}, opt_expr) : Ast_payload.action) ->
              match txt with
              | "null" ->
                (match opt_expr with
                 | None -> true
                 | Some e ->
                   Ast_payload.assert_bool_lit e), undefined

              |  "undefined" ->
                null,
                (match opt_expr with
                 | None ->  true
                 | Some e ->
                   Ast_payload.assert_bool_lit e)
              | "nullable" ->
                begin match opt_expr with
                  | None -> true, true
                  | Some e ->
                    let v = Ast_payload.assert_bool_lit e in
                    v,v
                end
              | _ -> Bs_syntaxerr.err loc Unsupported_predicates
            ) (false, false)
            (Ast_payload.ident_or_record_as_config loc payload)  in

        ({st with get = Some result}, acc  )

      | "bs.set"
        ->
        let result =
          List.fold_left
            (fun st (({txt ; loc}, opt_expr) : Ast_payload.action) ->
               if txt =  "no_get" then
                 match opt_expr with
                 | None -> `No_get
                 | Some e ->
                   if Ast_payload.assert_bool_lit e then
                     `No_get
                   else `Get
               else Bs_syntaxerr.err loc Unsupported_predicates
            ) `Get (Ast_payload.ident_or_record_as_config loc payload)  in
        (* properties -- void
              [@@bs.set{only}]
        *)
        {st with set = Some result }, acc
      | _ ->
        (st, attr::acc  )
    ) ( {get = None ; set = None}, []) attrs


let process_attributes_rev (attrs : t) =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs", (`Nothing | `Uncurry)
        ->
        `Uncurry, acc
      | "bs.this", (`Nothing | `Meth_callback)
        ->  `Meth_callback, acc
      | "bs.meth",  (`Nothing | `Method)
        -> `Method, acc
      | "bs", _
      | "bs.this", _
        -> Bs_syntaxerr.err loc Conflict_bs_bs_this_bs_meth
      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_pexp_fun_attributes_rev (attrs : t) =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs.open", (`Nothing | `Exn)
        ->
        `Exn, acc

      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_bs attrs =
  List.fold_left (fun (st, acc) (({txt; loc}, _) as attr : attr) ->
      match txt, st  with
      | "bs", _
        ->
        `Has, acc
      | _ , _ ->
        st, attr::acc
    ) ( `Nothing, []) attrs

let process_external attrs =
  List.exists (fun (({txt; }, _)  : attr) ->
      if Ext_string.starts_with txt "bs." then true
      else false
    ) attrs


type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : Ast_payload.action list option
}

let process_derive_type attrs : derive_attr * t =
  List.fold_left
    (fun (st, acc)
      (({txt ; loc}, payload  as attr): attr)  ->
      match  st, txt  with
      |  {bs_deriving = None}, "bs.deriving"
        ->
        {st with
         bs_deriving = Some
             (Ast_payload.ident_or_record_as_config loc payload)}, acc
      | {bs_deriving = Some _}, "bs.deriving"
        ->
        Bs_syntaxerr.err loc Duplicated_bs_deriving

      | _ , _ ->
        let st =
          if txt = "nonrec" then
            { st with explict_nonrec = true }
          else st in
        st, attr::acc
    ) ( {explict_nonrec = false; bs_deriving = None }, []) attrs

let iter_process_derive_type attrs =
  let st = ref {explict_nonrec = false; bs_deriving = None } in
  List.iter
    (fun
      (({txt ; loc}, payload  as attr): attr)  ->
      match  txt  with
      |  "bs.deriving"
        ->
        let ost = !st in
        (match ost with
         | {bs_deriving = None } ->
           Bs_ast_invariant.mark_used_bs_attribute attr ;
           st :=
             {ost with
              bs_deriving = Some
                  (Ast_payload.ident_or_record_as_config loc payload)}
         | {bs_deriving = Some _} ->
           Bs_syntaxerr.err loc Duplicated_bs_deriving)

      | "nonrec" ->
        st :=
          { !st with explict_nonrec = true }
      (* non bs attribute, no need to mark its use *)
      | _ -> ()
    )  attrs;
  !st


(* duplicated [bs.uncurry] [bs.string] not allowed,
  it is worse in bs.uncurry since it will introduce
  inconsistency in arity
 *)  
let iter_process_bs_string_int_unwrap_uncurry attrs =
  let st = ref `Nothing in 
  let assign v (({loc;_}, _ ) as attr : attr) = 
    if !st = `Nothing then 
    begin 
      Bs_ast_invariant.mark_used_bs_attribute attr;
      st := v ;
    end  
    else Bs_syntaxerr.err loc Conflict_attributes  in 
  List.iter
    (fun (({txt ; loc}, (payload : _ ) ) as attr : attr)  ->
      match  txt with
      | "bs.string"
        -> assign `String attr
      | "bs.int"
        -> assign `Int attr
      | "bs.ignore"
        -> assign `Ignore attr
      | "bs.unwrap"
        -> assign `Unwrap attr
      | "bs.uncurry"
        ->
        assign (`Uncurry (Ast_payload.is_single_int payload)) attr
      | _ -> ()
    ) attrs;
    !st 

(* let process_bs_string_int_unwrap_uncurry attrs =
  List.fold_left
    (fun (st,attrs)
      (({txt ; loc}, (payload : _ ) ) as attr : attr)  ->
      match  txt, st  with
      | "bs.string", (`Nothing | `String)
        -> `String, attrs
      | "bs.int", (`Nothing | `Int)
        ->  `Int, attrs
      | "bs.ignore", (`Nothing | `Ignore)
        -> `Ignore, attrs
      | "bs.unwrap", (`Nothing | `Unwrap)
        -> `Unwrap, attrs
      | "bs.uncurry", `Nothing
        ->
        `Uncurry (Ast_payload.is_single_int payload), attrs
      (* Don't allow duplicated [bs.uncurry] since
         it may introduce inconsistency in arity
      *)
      | "bs.int", _
      | "bs.string", _
      | "bs.ignore", _
      | "bs.unwrap", _
        ->
        Bs_syntaxerr.err loc Conflict_attributes
      | _ , _ -> st, (attr :: attrs )
    ) (`Nothing, []) attrs *)


let iter_process_bs_string_as  (attrs : t) : string option =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st = None then
          match Ast_payload.is_single_string payload with
          | None ->
            Bs_syntaxerr.err loc Expect_string_literal
          | Some  (v,_dec) ->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st:= Some v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) attrs;
  !st

let has_bs_optional  (attrs : t) : bool =
  List.exists
    (fun
      (({txt ; loc}, _payload ) as attr : attr)  ->
      match  txt with
      | "bs.optional"
        ->
        Bs_ast_invariant.mark_used_bs_attribute attr ;
        true
      | _  -> false
    ) attrs



let iter_process_bs_int_as  attrs =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st =  None then
          match Ast_payload.is_single_int payload with
          | None ->
            Bs_syntaxerr.err loc Expect_int_literal
          | Some  _ as v->
            Bs_ast_invariant.mark_used_bs_attribute attr ;
            st := v
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _  -> ()
    ) attrs; !st


let iter_process_bs_string_or_int_as attrs =
  let st = ref None in
  List.iter
    (fun
      (({txt ; loc}, payload ) as attr : attr)  ->
      match  txt with
      | "bs.as"
        ->
        if !st = None then
          (Bs_ast_invariant.mark_used_bs_attribute attr ;
           match Ast_payload.is_single_int payload with
           | None ->
             begin match Ast_payload.is_single_string payload with
               | Some (s,None) ->
                 st := Some (`Str (s))
               | Some (s, Some "json") ->
                 st := Some (`Json_str s )
               | None | Some (_, Some _) ->
                 Bs_syntaxerr.err loc Expect_int_or_string_or_json_literal

             end
           | Some   v->
             st := (Some (`Int v))
          )
        else
          Bs_syntaxerr.err loc Duplicated_bs_as
      | _ -> ()

    ) attrs;
  !st

let locg = Location.none
let bs : attr
  =  {txt = "bs" ; loc = locg}, Ast_payload.empty

let is_bs (attr : attr) =
  match attr with
  | {Location.txt = "bs"; _}, _ -> true
  | _ -> false

let bs_this : attr
  =  {txt = "bs.this" ; loc = locg}, Ast_payload.empty

let bs_method : attr
  =  {txt = "bs.meth"; loc = locg}, Ast_payload.empty

let bs_obj : attr
  =  {txt = "bs.obj"; loc = locg}, Ast_payload.empty

let bs_get : attr
  =  {txt = "bs.get"; loc = locg}, Ast_payload.empty

let bs_get_arity : attr
  =  {txt = "internal.arity"; loc = locg}, 
    PStr 
    [{pstr_desc =
         Pstr_eval (
           {pexp_desc =
              Pexp_constant
                (Const_int 1);
            pexp_loc = locg;
            pexp_attributes = []
           },[])
      ; pstr_loc = locg}]
  

let bs_set : attr
  =  {txt = "bs.set"; loc = locg}, Ast_payload.empty

let bs_return_undefined : attr
  =
  {txt = "bs.return"; loc = locg },
  PStr
    [
      {pstr_desc =
         Pstr_eval (
           {pexp_desc =
              Pexp_ident
                { txt = Lident "undefined_to_opt";
                  loc = locg};
            pexp_loc = locg;
            pexp_attributes = []
           },[])
      ; pstr_loc = locg}]


end
module Ast_exp : sig 
#1 "ast_exp.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.expression 

end = struct
#1 "ast_exp.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.expression 

end
module Ast_external_mk : sig 
#1 "ast_external_mk.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(**
  [local_module loc ~pval_prim ~pval_type args]
  generate such code 
  {[
    let module J = struct 
       external unsafe_expr : pval_type = pval_prim 
    end in 
    J.unssafe_expr args
  ]}
*)
val local_external : Location.t ->
  ?pval_attributes:Parsetree.attributes ->
  pval_prim:string list ->
  pval_type:Parsetree.core_type ->
  ?local_module_name:string ->
  ?local_fun_name:string ->
  (string * Parsetree.expression) list -> Parsetree.expression_desc

val local_extern_cont : 
  Location.t ->
  ?pval_attributes:Parsetree.attributes ->
  pval_prim:string list ->
  pval_type:Parsetree.core_type ->
  ?local_module_name:string ->
  ?local_fun_name:string ->
  (Parsetree.expression -> Parsetree.expression) -> Parsetree.expression_desc

end = struct
#1 "ast_external_mk.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let local_external loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     args
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     {
       pexp_desc =
         Pexp_apply
           (({pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                      loc};
              pexp_attributes = [] ;
              pexp_loc = loc} : Parsetree.expression),
            args);
       pexp_attributes = [];
       pexp_loc = loc
     })

let local_extern_cont loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     (cb : Parsetree.expression -> 'a) 
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     cb {pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                 loc};
         pexp_attributes = [] ;
         pexp_loc = loc}
)

end
module Ast_pat : sig 
#1 "ast_pat.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Parsetree.pattern

val is_unit_cont : yes:'a -> no:'a -> t -> 'a

(** [arity_of_fun pat e] tells the arity of 
    expression [fun pat -> e]*)
val arity_of_fun : t -> Parsetree.expression -> int


val is_single_variable_pattern_conservative : t -> bool

end = struct
#1 "ast_pat.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type t = Parsetree.pattern


let is_unit_cont ~yes ~no (p : t)  =
  match p  with
  | {ppat_desc = Ppat_construct({txt = Lident "()"}, None)}
    -> yes 
  | _ -> no


(** [arity_of_fun pat e] tells the arity of 
    expression [fun pat -> e]
*)
let arity_of_fun
    (pat : Parsetree.pattern)
    (e : Parsetree.expression) =
  let rec aux (e : Parsetree.expression)  =
    match e.pexp_desc with
    | Pexp_fun ("", None, pat, e) ->
      1 + aux e       
    | Pexp_fun _
      -> Location.raise_errorf
           ~loc:e.pexp_loc "Label is not allowed in JS object"
    | _ -> 0 in
  is_unit_cont ~yes:0 ~no:1 pat + aux e 


let rec is_single_variable_pattern_conservative  (p : t ) =
  match p.ppat_desc with 
  | Parsetree.Ppat_any 
  | Parsetree.Ppat_var _ -> true 
  | Parsetree.Ppat_alias (p,_) 
  | Parsetree.Ppat_constraint (p, _) -> 
    is_single_variable_pattern_conservative p 
  
  | _ -> false

end
module Bs_ast_mapper : sig 
#1 "bs_ast_mapper.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The interface of a -ppx rewriter

  A -ppx rewriter is a program that accepts a serialized abstract syntax
  tree and outputs another, possibly modified, abstract syntax tree.
  This module encapsulates the interface between the compiler and
  the -ppx rewriters, handling such details as the serialization format,
  forwarding of command-line flags, and storing state.

  {!mapper} allows to implement AST rewriting using open recursion.
  A typical mapper would be based on {!default_mapper}, a deep
  identity mapper, and will fall back on it for handling the syntax it
  does not modify. For example:

  {[
open Asttypes
open Parsetree
open Ast_mapper

let test_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "test" }, PStr [])} ->
        Ast_helper.Exp.constant (Const_int 42)
      | other -> default_mapper.expr mapper other; }

let () =
  register "ppx_test" test_mapper]}

  This -ppx rewriter, which replaces [[%test]] in expressions with
  the constant [42], can be compiled using
  [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

  *)

  open Parsetree

  (** {2 A generic Parsetree mapper} *)

  type mapper = {
    attribute: mapper -> attribute -> attribute;
    attributes: mapper -> attribute list -> attribute list;
    case: mapper -> case -> case;
    cases: mapper -> case list -> case list;
    class_declaration: mapper -> class_declaration -> class_declaration;
    class_description: mapper -> class_description -> class_description;
    class_expr: mapper -> class_expr -> class_expr;
    class_field: mapper -> class_field -> class_field;
    class_signature: mapper -> class_signature -> class_signature;
    class_structure: mapper -> class_structure -> class_structure;
    class_type: mapper -> class_type -> class_type;
    class_type_declaration: mapper -> class_type_declaration
                            -> class_type_declaration;
    class_type_field: mapper -> class_type_field -> class_type_field;
    constructor_declaration: mapper -> constructor_declaration
                             -> constructor_declaration;
    expr: mapper -> expression -> expression;
    extension: mapper -> extension -> extension;
    extension_constructor: mapper -> extension_constructor
                           -> extension_constructor;
    include_declaration: mapper -> include_declaration -> include_declaration;
    include_description: mapper -> include_description -> include_description;
    label_declaration: mapper -> label_declaration -> label_declaration;
    location: mapper -> Location.t -> Location.t;
    module_binding: mapper -> module_binding -> module_binding;
    module_declaration: mapper -> module_declaration -> module_declaration;
    module_expr: mapper -> module_expr -> module_expr;
    module_type: mapper -> module_type -> module_type;
    module_type_declaration: mapper -> module_type_declaration
                             -> module_type_declaration;
    open_description: mapper -> open_description -> open_description;
    pat: mapper -> pattern -> pattern;
    payload: mapper -> payload -> payload;
    signature: mapper -> signature -> signature;
    signature_item: mapper -> signature_item -> signature_item;
    structure: mapper -> structure -> structure;
    structure_item: mapper -> structure_item -> structure_item;
    typ: mapper -> core_type -> core_type;
    type_declaration: mapper -> type_declaration -> type_declaration;
(* XXXXX *)
    type_declaration_list: mapper -> type_declaration list -> type_declaration list;
(* XXXXX *)
    type_extension: mapper -> type_extension -> type_extension;
    type_kind: mapper -> type_kind -> type_kind;
    value_binding: mapper -> value_binding -> value_binding;
(* XXXXX *)
    value_bindings_rec: mapper -> value_binding list -> value_binding list;
    value_bindings: mapper -> value_binding list -> value_binding list;
(* XXXXX *)
    value_description: mapper -> value_description -> value_description;
    with_constraint: mapper -> with_constraint -> with_constraint;
  }
  (** A mapper record implements one "method" per syntactic category,
      using an open recursion style: each method takes as its first
      argument the mapper to be applied to children in the syntax
      tree. *)

  val default_mapper: mapper
  (** A default mapper, which implements a "deep identity" mapping. *)

end = struct
#1 "bs_ast_mapper.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A generic Parsetree mapping class *)
(* Adapted for BUcklescript with more flexibilty*)

[@@@ocaml.warning "+9"]
(* Ensure that record patterns don't miss any field. *)



open Asttypes
open Parsetree
open Ast_helper
open Location

type mapper = {
  attribute: mapper -> attribute -> attribute;
  attributes: mapper -> attribute list -> attribute list;
  case: mapper -> case -> case;
  cases: mapper -> case list -> case list;
  class_declaration: mapper -> class_declaration -> class_declaration;
  class_description: mapper -> class_description -> class_description;
  class_expr: mapper -> class_expr -> class_expr;
  class_field: mapper -> class_field -> class_field;
  class_signature: mapper -> class_signature -> class_signature;
  class_structure: mapper -> class_structure -> class_structure;
  class_type: mapper -> class_type -> class_type;
  class_type_declaration: mapper -> class_type_declaration
                          -> class_type_declaration;
  class_type_field: mapper -> class_type_field -> class_type_field;
  constructor_declaration: mapper -> constructor_declaration
                           -> constructor_declaration;
  expr: mapper -> expression -> expression;
  extension: mapper -> extension -> extension;
  extension_constructor: mapper -> extension_constructor
                         -> extension_constructor;
  include_declaration: mapper -> include_declaration -> include_declaration;
  include_description: mapper -> include_description -> include_description;
  label_declaration: mapper -> label_declaration -> label_declaration;
  location: mapper -> Location.t -> Location.t;
  module_binding: mapper -> module_binding -> module_binding;
  module_declaration: mapper -> module_declaration -> module_declaration;
  module_expr: mapper -> module_expr -> module_expr;
  module_type: mapper -> module_type -> module_type;
  module_type_declaration: mapper -> module_type_declaration
                           -> module_type_declaration;
  open_description: mapper -> open_description -> open_description;
  pat: mapper -> pattern -> pattern;
  payload: mapper -> payload -> payload;
  signature: mapper -> signature -> signature;
  signature_item: mapper -> signature_item -> signature_item;
  structure: mapper -> structure -> structure;
  structure_item: mapper -> structure_item -> structure_item;
  typ: mapper -> core_type -> core_type;
  type_declaration: mapper -> type_declaration -> type_declaration;
(* XXXX *)
  type_declaration_list : mapper -> type_declaration list -> type_declaration list;
(* XXXX *)
  type_extension: mapper -> type_extension -> type_extension;
  type_kind: mapper -> type_kind -> type_kind;
  value_binding: mapper -> value_binding -> value_binding;
(* XXXX *)
  value_bindings_rec : mapper -> value_binding list -> value_binding list;
  value_bindings : mapper -> value_binding list -> value_binding list;
(* XXXXX *)
  value_description: mapper -> value_description -> value_description;
  with_constraint: mapper -> with_constraint -> with_constraint;
}

let map_fst f (x, y) = (f x, y)
let map_snd f (x, y) = (x, f y)
let map_tuple f1 f2 (x, y) = (f1 x, f2 y)
let map_tuple3 f1 f2 f3 (x, y, z) = (f1 x, f2 y, f3 z)
let map_opt f = function None -> None | Some x -> Some (f x)

let map_loc sub {loc; txt} = {loc = sub.location sub loc; txt}

module T = struct
  (* Type expressions for the core language *)

  let row_field sub = function
    | Rtag (l, attrs, b, tl) ->
        Rtag (l, sub.attributes sub attrs, b, List.map (sub.typ sub) tl)
    | Rinherit t -> Rinherit (sub.typ sub t)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs} =
    let open Typ in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ptyp_any -> any ~loc ~attrs ()
    | Ptyp_var s -> var ~loc ~attrs s
    | Ptyp_arrow (lab, t1, t2) ->
        arrow ~loc ~attrs lab (sub.typ sub t1) (sub.typ sub t2)
    | Ptyp_tuple tyl -> tuple ~loc ~attrs (List.map (sub.typ sub) tyl)
    | Ptyp_constr (lid, tl) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_object (l, o) ->
        let f (s, a, t) = (s, sub.attributes sub a, sub.typ sub t) in
        object_ ~loc ~attrs (List.map f l) o
    | Ptyp_class (lid, tl) ->
        class_ ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tl)
    | Ptyp_alias (t, s) -> alias ~loc ~attrs (sub.typ sub t) s
    | Ptyp_variant (rl, b, ll) ->
        variant ~loc ~attrs (List.map (row_field sub) rl) b ll
    | Ptyp_poly (sl, t) -> poly ~loc ~attrs sl (sub.typ sub t)
    | Ptyp_package (lid, l) ->
        package ~loc ~attrs (map_loc sub lid)
          (List.map (map_tuple (map_loc sub) (sub.typ sub)) l)
    | Ptyp_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    Type.mk (map_loc sub ptype_name)
      ~params:(List.map (map_fst (sub.typ sub)) ptype_params)
      ~priv:ptype_private
      ~cstrs:(List.map
                (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub))
                ptype_cstrs)
      ~kind:(sub.type_kind sub ptype_kind)
      ?manifest:(map_opt (sub.typ sub) ptype_manifest)
      ~loc:(sub.location sub ptype_loc)
      ~attrs:(sub.attributes sub ptype_attributes)
(* XXXX *)
  let map_type_declaration_list sub l = List.map  (sub.type_declaration sub) l
(* XXXX *)
  let map_type_kind sub = function
    | Ptype_abstract -> Ptype_abstract
    | Ptype_variant l ->
        Ptype_variant (List.map (sub.constructor_declaration sub) l)
    | Ptype_record l -> Ptype_record (List.map (sub.label_declaration sub) l)
    | Ptype_open -> Ptype_open

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_attributes} =
    Te.mk
      (map_loc sub ptyext_path)
      (List.map (sub.extension_constructor sub) ptyext_constructors)
      ~params:(List.map (map_fst (sub.typ sub)) ptyext_params)
      ~priv:ptyext_private
      ~attrs:(sub.attributes sub ptyext_attributes)

  let map_extension_constructor_kind sub = function
      Pext_decl(ctl, cto) ->
        Pext_decl(List.map (sub.typ sub) ctl, map_opt (sub.typ sub) cto)
    | Pext_rebind li ->
        Pext_rebind (map_loc sub li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    Te.constructor
      (map_loc sub pext_name)
      (map_extension_constructor_kind sub pext_kind)
      ~loc:(sub.location sub pext_loc)
      ~attrs:(sub.attributes sub pext_attributes)

end

module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs (sub.class_signature sub x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab (sub.typ sub t) (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs}
    =
    let open Ctf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs (sub.class_type sub ct)
    | Pctf_val (s, m, v, t) -> val_ ~loc ~attrs s m v (sub.typ sub t)
    | Pctf_method (s, p, v, t) -> method_ ~loc ~attrs s p v (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    Csig.mk
      (sub.typ sub pcsig_self)
      (List.map (sub.class_type_field sub) pcsig_fields)
end

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs (sub.signature sub sg)
    | Pmty_functor (s, mt1, mt2) ->
        functor_ ~loc ~attrs (map_loc sub s)
          (Misc.may_map (sub.module_type sub) mt1)
          (sub.module_type sub mt2)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs (sub.module_type sub mt)
          (List.map (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_with_constraint sub = function
    | Pwith_type (lid, d) ->
        Pwith_type (map_loc sub lid, sub.type_declaration sub d)
    | Pwith_module (lid, lid2) ->
        Pwith_module (map_loc sub lid, map_loc sub lid2)
    | Pwith_typesubst d -> Pwith_typesubst (sub.type_declaration sub d)
    | Pwith_modsubst (s, lid) ->
        Pwith_modsubst (map_loc sub s, map_loc sub lid)

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let loc = sub.location sub loc in
    match desc with
    | Psig_value vd -> value ~loc (sub.value_description sub vd)
    | Psig_type l -> type_ ~loc (sub.type_declaration_list sub l)
    | Psig_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Psig_module x -> module_ ~loc (sub.module_declaration sub x)
    | Psig_recmodule l ->
        rec_module ~loc (List.map (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc (sub.open_description sub x)
    | Psig_include x -> include_ ~loc (sub.include_description sub x)
    | Psig_class l -> class_ ~loc (List.map (sub.class_description sub) l)
    | Psig_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Psig_attribute x -> attribute ~loc (sub.attribute sub x)
end


module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs (sub.structure sub str)
    | Pmod_functor (arg, arg_ty, body) ->
        functor_ ~loc ~attrs (map_loc sub arg)
          (Misc.may_map (sub.module_type sub) arg_ty)
          (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs (sub.module_expr sub m1) (sub.module_expr sub m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs (sub.module_expr sub m)
                    (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let loc = sub.location sub loc in
    match desc with
    | Pstr_eval (x, attrs) ->
        eval ~loc ~attrs:(sub.attributes sub attrs) (sub.expr sub x)
    | Pstr_value (r, vbs) ->
(* XXX *)
(* value ~loc r (List.map (sub.value_binding sub) vbs) *)
      value ~loc r
      ((if r = Recursive then sub.value_bindings_rec else sub.value_bindings)
       sub vbs)
(* XXX *)
    | Pstr_primitive vd -> primitive ~loc (sub.value_description sub vd)
    | Pstr_type l -> type_ ~loc (sub.type_declaration_list sub l)
    | Pstr_typext te -> type_extension ~loc (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc (sub.extension_constructor sub ed)
    | Pstr_module x -> module_ ~loc (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc (List.map (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc (sub.open_description sub x)
    | Pstr_class l -> class_ ~loc (List.map (sub.class_declaration sub) l)
    | Pstr_class_type l ->
        class_type ~loc (List.map (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        extension ~loc (sub.extension sub x) ~attrs:(sub.attributes sub attrs)
    | Pstr_attribute x -> attribute ~loc (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs} =
    let open Exp in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs x
    | Pexp_let (r, vbs, e) ->
(* XXXX *)
        (* let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.expr sub e) *)
        let_ ~loc ~attrs r
        (
          (if r = Recursive then sub.value_bindings_rec else sub.value_bindings)
          sub vbs
        )
        (sub.expr sub e)
(* XXXX *)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab (map_opt (sub.expr sub) def) (sub.pat sub p)
          (sub.expr sub e)
    | Pexp_function pel -> function_ ~loc ~attrs (sub.cases sub pel)
    | Pexp_apply (e, l) ->
        apply ~loc ~attrs (sub.expr sub e) (List.map (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) ->
        match_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs (sub.expr sub e) (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_construct (lid, arg) ->
        construct ~loc ~attrs (map_loc sub lid) (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) ->
        variant ~loc ~attrs lab (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs (List.map (map_tuple (map_loc sub) (sub.expr sub)) l)
          (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) ->
        field ~loc ~attrs (sub.expr sub e) (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs (sub.expr sub e1) (map_loc sub lid)
          (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs (List.map (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
          (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs (sub.expr sub e1) (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs (sub.pat sub p) (sub.expr sub e1) (sub.expr sub e2) d
          (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t1)
          (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs (sub.expr sub e) (sub.typ sub t)
    | Pexp_send (e, s) -> send ~loc ~attrs (sub.expr sub e) s
    | Pexp_new lid -> new_ ~loc ~attrs (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs (map_loc sub s) (sub.expr sub e)
    | Pexp_override sel ->
        override ~loc ~attrs
          (List.map (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs (map_loc sub s) (sub.module_expr sub me)
          (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs (sub.expr sub e) (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs (sub.class_structure sub cls)
    | Pexp_newtype (s, e) -> newtype ~loc ~attrs s (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs (sub.module_expr sub me)
    | Pexp_open (ovf, lid, e) ->
        open_ ~loc ~attrs ovf (map_loc sub lid) (sub.expr sub e)
    | Pexp_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs} =
    let open Pat in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Ppat_any -> any ~loc ~attrs ()
    | Ppat_var s -> var ~loc ~attrs (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs (sub.pat sub p) (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs c
    | Ppat_interval (c1, c2) -> interval ~loc ~attrs c1 c2
    | Ppat_tuple pl -> tuple ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs (map_loc sub l) (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs
               (List.map (map_tuple (map_loc sub) (sub.pat sub)) lpl) cf
    | Ppat_array pl -> array ~loc ~attrs (List.map (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs (sub.pat sub p1) (sub.pat sub p2)
    | Ppat_constraint (p, t) ->
        constraint_ ~loc ~attrs (sub.pat sub p) (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs (map_loc sub s)
    | Ppat_exception p -> exception_ ~loc ~attrs (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs (sub.extension sub x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs (map_loc sub lid) (List.map (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          (map_opt (sub.expr sub) e)
          (sub.pat sub p)
          (sub.class_expr sub ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs (sub.class_expr sub ce)
          (List.map (map_snd (sub.expr sub)) l)
    | Pcl_let (r, vbs, ce) ->
(* XXXX *)
        (* let_ ~loc ~attrs r (List.map (sub.value_binding sub) vbs)
          (sub.class_expr sub ce) *)
          let_ ~loc ~attrs r
          ((if r = Recursive then sub.value_bindings_rec else sub.value_bindings)
            sub vbs)
          (sub.class_expr sub ce)
(* XXXX *)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs (sub.class_expr sub ce) (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_kind sub = function
    | Cfk_concrete (o, e) -> Cfk_concrete (o, sub.expr sub e)
    | Cfk_virtual t -> Cfk_virtual (sub.typ sub t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let loc = sub.location sub loc in
    let attrs = sub.attributes sub attrs in
    match desc with
    | Pcf_inherit (o, ce, s) -> inherit_ ~loc ~attrs o (sub.class_expr sub ce) s
    | Pcf_val (s, m, k) -> val_ ~loc ~attrs (map_loc sub s) m (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs (map_loc sub s) p (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs (sub.typ sub t1) (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    {
      pcstr_self = sub.pat sub pcstr_self;
      pcstr_fields = List.map (sub.class_field sub) pcstr_fields;
    }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    Ci.mk
     ~virt:pci_virt
     ~params:(List.map (map_fst (sub.typ sub)) pl)
      (map_loc sub pci_name)
      (f pci_expr)
      ~loc:(sub.location sub pci_loc)
      ~attrs:(sub.attributes sub pci_attributes)
end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  {
    structure = (fun this l -> List.map (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> List.map (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration =
      (fun this -> CE.class_infos this (this.class_expr this));
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_declaration_list = T.map_type_declaration_list;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    extension_constructor = T.map_extension_constructor;
    value_description =
      (fun this {pval_name; pval_type; pval_prim; pval_loc;
                 pval_attributes} ->
        Val.mk
          (map_loc this pval_name)
          (this.typ this pval_type)
          ~attrs:(this.attributes this pval_attributes)
          ~loc:(this.location this pval_loc)
          ~prim:pval_prim
      );

    pat = P.map;
    expr = E.map;

    module_declaration =
      (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
         Md.mk
           (map_loc this pmd_name)
           (this.module_type this pmd_type)
           ~attrs:(this.attributes this pmd_attributes)
           ~loc:(this.location this pmd_loc)
      );

    module_type_declaration =
      (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
         Mtd.mk
           (map_loc this pmtd_name)
           ?typ:(map_opt (this.module_type this) pmtd_type)
           ~attrs:(this.attributes this pmtd_attributes)
           ~loc:(this.location this pmtd_loc)
      );

    module_binding =
      (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
         Mb.mk (map_loc this pmb_name) (this.module_expr this pmb_expr)
           ~attrs:(this.attributes this pmb_attributes)
           ~loc:(this.location this pmb_loc)
      );


    open_description =
      (fun this {popen_lid; popen_override; popen_attributes; popen_loc} ->
         Opn.mk (map_loc this popen_lid)
           ~override:popen_override
           ~loc:(this.location this popen_loc)
           ~attrs:(this.attributes this popen_attributes)
      );


    include_description =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_type this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    include_declaration =
      (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
         Incl.mk (this.module_expr this pincl_mod)
           ~loc:(this.location this pincl_loc)
           ~attrs:(this.attributes this pincl_attributes)
      );

    value_bindings = (fun this vbs ->
      match vbs with
      | [vb] -> [ this.value_binding this vb ]
      | _ -> List.map (this.value_binding this) vbs
    );
    value_bindings_rec = (fun this vbs ->
      match vbs with
      | [vb] -> [ this.value_binding this vb ]
      | _ -> List.map (this.value_binding this) vbs
    );
    value_binding =
      (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
         Vb.mk
           (this.pat this pvb_pat)
           (this.expr this pvb_expr)
           ~loc:(this.location this pvb_loc)
           ~attrs:(this.attributes this pvb_attributes)
      );


    constructor_declaration =
      (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
        Type.constructor
          (map_loc this pcd_name)
          ~args:(List.map (this.typ this) pcd_args)
          ?res:(map_opt (this.typ this) pcd_res)
          ~loc:(this.location this pcd_loc)
          ~attrs:(this.attributes this pcd_attributes)
      );

    label_declaration =
      (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
         Type.field
           (map_loc this pld_name)
           (this.typ this pld_type)
           ~mut:pld_mutable
           ~loc:(this.location this pld_loc)
           ~attrs:(this.attributes this pld_attributes)
      );

    cases = (fun this l -> List.map (this.case this) l);
    case =
      (fun this {pc_lhs; pc_guard; pc_rhs} ->
         {
           pc_lhs = this.pat this pc_lhs;
           pc_guard = map_opt (this.expr this) pc_guard;
           pc_rhs = this.expr this pc_rhs;
         }
      );



    location = (fun this l -> l);

    extension = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attribute = (fun this (s, e) -> (map_loc this s, this.payload this e));
    attributes = (fun this l -> List.map (this.attribute this) l);
    payload =
      (fun this -> function
         | PStr x -> PStr (this.structure this x)
         | PTyp x -> PTyp (this.typ this x)
         | PPat (x, g) -> PPat (this.pat this x, map_opt (this.expr this) g)
      );
  }

end
module Ext_pervasives : sig 
#1 "ext_pervasives.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Extension to standard library [Pervavives] module, safe to open 
  *)

external reraise: exn -> 'a = "%reraise"

val finally : 'a -> ('a -> 'c) -> ('a -> 'b) -> 'b

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a

val is_pos_pow : Int32.t -> int

val failwithf : loc:string -> ('a, unit, string, 'b) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val bad_argf : ('a, unit, string, 'b) format4 -> 'a



val dump : 'a -> string 
val pp_any : Format.formatter -> 'a -> unit 
external id : 'a -> 'a = "%identity"

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
val hash_variant : string -> int

end = struct
#1 "ext_pervasives.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






external reraise: exn -> 'a = "%reraise"

let finally v action f   = 
  match f v with
  | exception e -> 
      action v ;
      reraise e 
  | e ->  action v ; e 

let with_file_as_chan filename f = 
  finally (open_out_bin filename) close_out f 

let with_file_as_pp filename f = 
  finally (open_out_bin filename) close_out
    (fun chan -> 
      let fmt = Format.formatter_of_out_channel chan in
      let v = f  fmt in
      Format.pp_print_flush fmt ();
      v
    ) 


let  is_pos_pow n = 
  let module M = struct exception E end in 
  let rec aux c (n : Int32.t) = 
    if n <= 0l then -2 
    else if n = 1l then c 
    else if Int32.logand n 1l =  0l then   
      aux (c + 1) (Int32.shift_right n 1 )
    else raise M.E in 
  try aux 0 n  with M.E -> -1

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s))
    fmt
    
let invalid_argf fmt = Format.ksprintf invalid_arg fmt

let bad_argf fmt = Format.ksprintf (fun x -> raise (Arg.Bad x ) ) fmt


let rec dump r =
  if Obj.is_int r then
    string_of_int (Obj.magic r : int)
  else (* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
        r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
        []
      else
        let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment.  *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
      let fields = get_list r in
      "[" ^ String.concat "; " (Ext_list.map dump fields) ^ "]"
    | 0 ->
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (Ext_list.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
      (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
      opaque "lazy"
    | x when x = Obj.closure_tag ->
      opaque "closure"
    | x when x = Obj.object_tag ->
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with
        | h::h'::t -> h, h', t
        | _ -> assert false
      in
      (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
      "Object #" ^ dump id ^ " (" ^ String.concat ", " (Ext_list.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
      opaque "infix"
    | x when x = Obj.forward_tag ->
      opaque "forward"
    | x when x < Obj.no_scan_tag ->
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (Ext_list.map dump fields) ^ ")"
    | x when x = Obj.string_tag ->
      "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag ->
      string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag ->
      opaque "abstract"
    | x when x = Obj.custom_tag ->
      opaque "custom"
    | x when x = Obj.custom_tag ->
      opaque "final"
    | x when x = Obj.double_array_tag ->
      "[|"^
      String.concat ";"
        (Array.to_list (Array.map string_of_float (Obj.magic r : float array))) ^
      "|]"
    | _ ->
      opaque (Printf.sprintf "unknown: tag %d size %d" t s)

let dump v = dump (Obj.repr v)

let pp_any fmt v = 
  Format.fprintf fmt "@[%s@]"
  (dump v )
external id : 'a -> 'a = "%identity"


let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


end
module Ext_array : sig 
#1 "ext_array.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






(** Some utilities for {!Array} operations *)
val reverse_range : 'a array -> int -> int -> unit
val reverse_in_place : 'a array -> unit
val reverse : 'a array -> 'a array 
val reverse_of_list : 'a list -> 'a array

val filter : ('a -> bool) -> 'a array -> 'a array

val filter_map : ('a -> 'b option) -> 'a array -> 'b array

val range : int -> int -> int array

val map2i : (int -> 'a -> 'b -> 'c ) -> 'a array -> 'b array -> 'c array

val to_list_f : ('a -> 'b) -> 'a array -> 'b list 
val to_list_map : ('a -> 'b option) -> 'a array -> 'b list 

val to_list_map_acc : 
  ('a -> 'b option) -> 
  'a array -> 
  'b list -> 
  'b list 

val of_list_map : ('a -> 'b) -> 'a list -> 'b array 

val rfind_with_index : 'a array -> ('a -> 'b -> bool) -> 'b -> int


type 'a split = [ `No_split | `Split of 'a array * 'a array ]

val rfind_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

val find_and_split : 
  'a array ->
  ('a -> 'b -> bool) ->
  'b -> 'a split

val exists : ('a -> bool) -> 'a array -> bool 

val is_empty : 'a array -> bool 

val for_all2_no_exn : 
  ('a -> 'b -> bool) -> 
  'a array ->
  'b array -> 
  bool
end = struct
#1 "ext_array.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





let reverse_range a i len =
  if len = 0 then ()
  else
    for k = 0 to (len-1)/2 do
      let t = Array.unsafe_get a (i+k) in
      Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
      Array.unsafe_set a (i+len-1-k) t;
    done


let reverse_in_place a =
  reverse_range a 0 (Array.length a)

let reverse a =
  let b_len = Array.length a in
  if b_len = 0 then [||] else  
    let b = Array.copy a in  
    for i = 0 to  b_len - 1 do
      Array.unsafe_set b i (Array.unsafe_get a (b_len - 1 -i )) 
    done;
    b  

let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map (f : _ -> _ option) a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

let range from to_ =
  if from > to_ then invalid_arg "Ext_array.range"  
  else Array.init (to_ - from + 1) (fun i -> i + from)

let map2i f a b = 
  let len = Array.length a in 
  if len <> Array.length b then 
    invalid_arg "Ext_array.map2i"  
  else
    Array.mapi (fun i a -> f i  a ( Array.unsafe_get b i )) a 

let rec tolist_f_aux a f  i res =
  if i < 0 then res else
    let v = Array.unsafe_get a i in
    tolist_f_aux a f  (i - 1)
      (f v :: res)
       
let to_list_f f a = tolist_f_aux a f (Array.length a  - 1) []

let rec tolist_aux a f  i res =
  if i < 0 then res else
    let v = Array.unsafe_get a i in
    tolist_aux a f  (i - 1)
      (match f v with
       | Some v -> v :: res
       | None -> res) 

let to_list_map f a = 
  tolist_aux a f (Array.length a - 1) []

let to_list_map_acc f a acc = 
  tolist_aux a f (Array.length a - 1) acc


let of_list_map f a = 
  match a with 
  | [] -> [||]
  | [a0] -> 
    let b0 = f a0 in
    [|b0|]
  | [a0;a1] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    [|b0;b1|]
  | [a0;a1;a2] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    [|b0;b1;b2|]
  | [a0;a1;a2;a3] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    [|b0;b1;b2;b3|]
  | [a0;a1;a2;a3;a4] -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    let b4 = f a4 in 
    [|b0;b1;b2;b3;b4|]

  | a0::a1::a2::a3::a4::tl -> 
    let b0 = f a0 in  
    let b1 = f a1 in 
    let b2 = f a2 in  
    let b3 = f a3 in 
    let b4 = f a4 in 
    let len = List.length tl + 5 in 
    let arr = Array.make len b0  in
    Array.unsafe_set arr 1 b1 ;  
    Array.unsafe_set arr 2 b2 ;
    Array.unsafe_set arr 3 b3 ; 
    Array.unsafe_set arr 4 b4 ; 
    let rec fill i = function
      | [] -> arr 
      | hd :: tl -> 
        Array.unsafe_set arr i (f hd); 
        fill (i + 1) tl in 
    fill 5 tl

(**
   {[
     # rfind_with_index [|1;2;3|] (=) 2;;
     - : int = 1
               # rfind_with_index [|1;2;3|] (=) 1;;
     - : int = 0
               # rfind_with_index [|1;2;3|] (=) 3;;
     - : int = 2
               # rfind_with_index [|1;2;3|] (=) 4;;
     - : int = -1
   ]}
*)
let rfind_with_index arr cmp v = 
  let len = Array.length arr in 
  let rec aux i = 
    if i < 0 then i
    else if  cmp (Array.unsafe_get arr i) v then i
    else aux (i - 1) in 
  aux (len - 1)

type 'a split = [ `No_split | `Split of 'a array * 'a array ]
let rfind_and_split arr cmp v : _ split = 
  let i = rfind_with_index arr cmp v in 
  if  i < 0 then 
    `No_split 
  else 
    `Split (Array.sub arr 0 i , Array.sub arr  (i + 1 ) (Array.length arr - i - 1 ))


let find_with_index arr cmp v = 
  let len  = Array.length arr in 
  let rec aux i len = 
    if i >= len then -1 
    else if cmp (Array.unsafe_get arr i ) v then i 
    else aux (i + 1) len in 
  aux 0 len

let find_and_split arr cmp v : _ split = 
  let i = find_with_index arr cmp v in 
  if i < 0 then 
    `No_split
  else
    `Split (Array.sub arr 0 i, Array.sub arr (i + 1 ) (Array.length arr - i - 1))        

(** TODO: available since 4.03, use {!Array.exists} *)

let exists p a =
  let n = Array.length a in
  let rec loop i =
    if i = n then false
    else if p (Array.unsafe_get a i) then true
    else loop (succ i) in
  loop 0


let is_empty arr =
  Array.length arr = 0


let rec unsafe_loop index len p xs ys  = 
  if index >= len then true
  else 
    p 
      (Array.unsafe_get xs index)
      (Array.unsafe_get ys index) &&
    unsafe_loop (succ index) len p xs ys 

let for_all2_no_exn p xs ys = 
  let len_xs = Array.length xs in 
  let len_ys = Array.length ys in 
  len_xs = len_ys &&    
  unsafe_loop 0 len_xs p xs ys
end
module Ext_json_types
= struct
#1 "ext_json_types.ml"
(* Copyright (C) 2015-2017 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type loc = Lexing.position
type json_str = 
  { str : string ; loc : loc}

type json_flo  =
  { flo : string ; loc : loc}
type json_array =
  { content : t array ; 
    loc_start : loc ; 
    loc_end : loc ; 
  }

and json_map = 
  { map : t String_map.t ; loc :  loc }
and t = 
  | True of loc 
  | False of loc 
  | Null of loc 
  | Flo of json_flo
  | Str of json_str
  | Arr  of json_array
  | Obj of json_map
   

end
module Ext_position : sig 
#1 "ext_position.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type t = Lexing.position = {
    pos_fname : string ;
    pos_lnum : int ;
    pos_bol : int ;
    pos_cnum : int
}

(** [offset pos newpos]
    return a new position
    here [newpos] is zero based, the use case is that
    at position [pos], we get a string and Lexing from that string,
    therefore, we get a [newpos] and we need rebase it on top of 
    [pos]
*)
val offset : t -> t -> t 

val lexbuf_from_channel_with_fname:
    in_channel -> string -> 
    Lexing.lexbuf

val print : Format.formatter -> t -> unit 
end = struct
#1 "ext_position.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type t = Lexing.position = {
    pos_fname : string ;
    pos_lnum : int ;
    pos_bol : int ;
    pos_cnum : int
}

let offset (x : t) (y:t) =
  {
    x with 
    pos_lnum =
       x.pos_lnum + y.pos_lnum - 1;
    pos_cnum = 
      x.pos_cnum + y.pos_cnum;
    pos_bol = 
      if y.pos_lnum = 1 then 
        x.pos_bol
      else x.pos_cnum + y.pos_bol
  }

let print fmt (pos : t) =
  Format.fprintf fmt "(line %d, column %d)" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)



let lexbuf_from_channel_with_fname ic fname = 
  let x = Lexing.from_function (fun buf n -> input ic buf 0 n) in 
  let pos : t = {
    pos_fname = fname ; 
    pos_lnum = 1; 
    pos_bol = 0;
    pos_cnum = 0 (* copied from zero_pos*)
  } in 
  x.lex_start_p <- pos;
  x.lex_curr_p <- pos ; 
  x


end
module Ext_json_parse : sig 
#1 "ext_json_parse.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type error

val report_error : Format.formatter -> error -> unit 

exception Error of Lexing.position * Lexing.position * error

val parse_json_from_string : string -> Ext_json_types.t 

val parse_json_from_chan :
  string ->  in_channel -> Ext_json_types.t 

val parse_json_from_file  : string -> Ext_json_types.t


end = struct
#1 "ext_json_parse.ml"
# 1 "ext/ext_json_parse.mll"
 
type error =
  | Illegal_character of char
  | Unterminated_string
  | Unterminated_comment
  | Illegal_escape of string
  | Unexpected_token 
  | Expect_comma_or_rbracket
  | Expect_comma_or_rbrace
  | Expect_colon
  | Expect_string_or_rbrace 
  | Expect_eof 
  (* | Trailing_comma_in_obj *)
  (* | Trailing_comma_in_array *)


let fprintf  = Format.fprintf
let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_string -> 
      fprintf ppf "Unterminated_string"
  | Expect_comma_or_rbracket ->
    fprintf ppf "Expect_comma_or_rbracket"
  | Expect_comma_or_rbrace -> 
    fprintf ppf "Expect_comma_or_rbrace"
  | Expect_colon -> 
    fprintf ppf "Expect_colon"
  | Expect_string_or_rbrace  -> 
    fprintf ppf "Expect_string_or_rbrace"
  | Expect_eof  -> 
    fprintf ppf "Expect_eof"
  | Unexpected_token 
    ->
    fprintf ppf "Unexpected_token"
  (* | Trailing_comma_in_obj  *)
  (*   -> fprintf ppf "Trailing_comma_in_obj" *)
  (* | Trailing_comma_in_array  *)
  (*   -> fprintf ppf "Trailing_comma_in_array" *)
  | Unterminated_comment 
    -> fprintf ppf "Unterminated_comment"
         

exception Error of Lexing.position * Lexing.position * error


let () = 
  Printexc.register_printer
    (function x -> 
     match x with 
     | Error (loc_start,loc_end,error) -> 
       Some (Format.asprintf 
          "@[%a:@ %a@ -@ %a)@]" 
          report_error  error
          Ext_position.print loc_start
          Ext_position.print loc_end
       )

     | _ -> None
    )





type token = 
  | Comma
  | Eof
  | False
  | Lbrace
  | Lbracket
  | Null
  | Colon
  | Number of string
  | Rbrace
  | Rbracket
  | String of string
  | True   
  
let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (lexbuf.lex_start_p, lexbuf.lex_curr_p, e))


let lexeme_len (x : Lexing.lexbuf) =
  x.lex_curr_pos - x.lex_start_pos

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let lf = '\010'

# 124 "ext/ext_json_parse.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\239\255\240\255\241\255\000\000\025\000\011\000\244\255\
    \245\255\246\255\247\255\248\255\249\255\000\000\000\000\000\000\
    \041\000\001\000\254\255\005\000\005\000\253\255\001\000\002\000\
    \252\255\000\000\000\000\003\000\251\255\001\000\003\000\250\255\
    \079\000\089\000\099\000\121\000\131\000\141\000\153\000\163\000\
    \001\000\253\255\254\255\023\000\255\255\006\000\246\255\189\000\
    \248\255\215\000\255\255\249\255\249\000\181\000\252\255\009\000\
    \063\000\075\000\234\000\251\255\032\001\250\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\013\000\013\000\016\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\016\000\016\000\016\000\
    \016\000\016\000\255\255\000\000\012\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\013\000\255\255\013\000\255\255\013\000\255\255\
    \255\255\255\255\255\255\001\000\255\255\255\255\255\255\008\000\
    \255\255\255\255\255\255\255\255\006\000\006\000\255\255\006\000\
    \001\000\002\000\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\020\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \042\000\000\000\000\000\255\255\000\000\047\000\000\000\047\000\
    \000\000\051\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\018\000\019\000\017\000\019\000\255\255\
    \048\000\019\000\255\255\057\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\000\000\000\000\019\000\000\000\000\000\
    \050\000\000\000\000\000\043\000\008\000\006\000\033\000\016\000\
    \004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\007\000\004\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\032\000\044\000\033\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\021\000\057\000\000\000\000\000\000\000\
    \020\000\000\000\000\000\012\000\000\000\011\000\032\000\056\000\
    \000\000\025\000\049\000\000\000\000\000\032\000\014\000\024\000\
    \028\000\000\000\000\000\057\000\026\000\030\000\013\000\031\000\
    \000\000\000\000\022\000\027\000\015\000\029\000\023\000\000\000\
    \000\000\000\000\039\000\010\000\039\000\009\000\032\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\037\000\000\000\037\000\000\000\
    \035\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \035\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\255\255\
    \000\000\056\000\000\000\000\000\055\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
    \000\000\054\000\000\000\000\000\000\000\000\000\054\000\000\000\
    \002\000\041\000\000\000\000\000\000\000\255\255\046\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\000\000\000\000\000\000\000\000\
    \000\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\054\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\000\000\000\000\000\000\000\000\054\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\000\000\000\000\000\000\052\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\061\000\061\000\061\000\061\000\061\000\061\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\017\000\000\000\000\000\019\000\020\000\
    \045\000\019\000\020\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\019\000\255\255\255\255\
    \045\000\255\255\255\255\040\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\004\000\043\000\005\000\
    \056\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\016\000\057\000\255\255\255\255\255\255\
    \016\000\255\255\255\255\000\000\255\255\000\000\005\000\056\000\
    \255\255\014\000\045\000\255\255\255\255\004\000\000\000\023\000\
    \027\000\255\255\255\255\057\000\025\000\029\000\000\000\030\000\
    \255\255\255\255\015\000\026\000\000\000\013\000\022\000\255\255\
    \255\255\255\255\032\000\000\000\032\000\000\000\005\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\035\000\255\255\035\000\255\255\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\047\000\
    \034\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\255\255\255\255\047\000\
    \255\255\049\000\255\255\255\255\049\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\049\000\255\255\255\255\255\255\255\255\049\000\255\255\
    \000\000\040\000\255\255\255\255\255\255\020\000\045\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\047\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\255\255\255\255\255\255\255\255\
    \255\255\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\049\000\255\255\255\255\255\255\255\255\
    \255\255\049\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
    \255\255\049\000\255\255\049\000\255\255\255\255\255\255\049\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\060\000\060\000\060\000\060\000\060\000\060\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\047\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\049\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lex_json buf lexbuf =
    __ocaml_lex_lex_json_rec buf lexbuf 0
and __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 142 "ext/ext_json_parse.mll"
          ( lex_json buf lexbuf)
# 314 "ext/ext_json_parse.ml"

  | 1 ->
# 143 "ext/ext_json_parse.mll"
                   ( 
    update_loc lexbuf 0;
    lex_json buf  lexbuf
  )
# 322 "ext/ext_json_parse.ml"

  | 2 ->
# 147 "ext/ext_json_parse.mll"
                ( comment buf lexbuf)
# 327 "ext/ext_json_parse.ml"

  | 3 ->
# 148 "ext/ext_json_parse.mll"
         ( True)
# 332 "ext/ext_json_parse.ml"

  | 4 ->
# 149 "ext/ext_json_parse.mll"
          (False)
# 337 "ext/ext_json_parse.ml"

  | 5 ->
# 150 "ext/ext_json_parse.mll"
         (Null)
# 342 "ext/ext_json_parse.ml"

  | 6 ->
# 151 "ext/ext_json_parse.mll"
       (Lbracket)
# 347 "ext/ext_json_parse.ml"

  | 7 ->
# 152 "ext/ext_json_parse.mll"
       (Rbracket)
# 352 "ext/ext_json_parse.ml"

  | 8 ->
# 153 "ext/ext_json_parse.mll"
       (Lbrace)
# 357 "ext/ext_json_parse.ml"

  | 9 ->
# 154 "ext/ext_json_parse.mll"
       (Rbrace)
# 362 "ext/ext_json_parse.ml"

  | 10 ->
# 155 "ext/ext_json_parse.mll"
       (Comma)
# 367 "ext/ext_json_parse.ml"

  | 11 ->
# 156 "ext/ext_json_parse.mll"
        (Colon)
# 372 "ext/ext_json_parse.ml"

  | 12 ->
# 157 "ext/ext_json_parse.mll"
                      (lex_json buf lexbuf)
# 377 "ext/ext_json_parse.ml"

  | 13 ->
# 159 "ext/ext_json_parse.mll"
         ( Number (Lexing.lexeme lexbuf))
# 382 "ext/ext_json_parse.ml"

  | 14 ->
# 161 "ext/ext_json_parse.mll"
      (
  let pos = Lexing.lexeme_start_p lexbuf in
  scan_string buf pos lexbuf;
  let content = (Buffer.contents  buf) in 
  Buffer.clear buf ;
  String content 
)
# 393 "ext/ext_json_parse.ml"

  | 15 ->
# 168 "ext/ext_json_parse.mll"
       (Eof )
# 398 "ext/ext_json_parse.ml"

  | 16 ->
let
# 169 "ext/ext_json_parse.mll"
       c
# 404 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 169 "ext/ext_json_parse.mll"
          ( error lexbuf (Illegal_character c ))
# 408 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state

and comment buf lexbuf =
    __ocaml_lex_comment_rec buf lexbuf 40
and __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 171 "ext/ext_json_parse.mll"
              (lex_json buf lexbuf)
# 420 "ext/ext_json_parse.ml"

  | 1 ->
# 172 "ext/ext_json_parse.mll"
     (comment buf lexbuf)
# 425 "ext/ext_json_parse.ml"

  | 2 ->
# 173 "ext/ext_json_parse.mll"
       (error lexbuf Unterminated_comment)
# 430 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
    __ocaml_lex_scan_string_rec buf start lexbuf 45
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 177 "ext/ext_json_parse.mll"
      ( () )
# 442 "ext/ext_json_parse.ml"

  | 1 ->
# 179 "ext/ext_json_parse.mll"
  (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 452 "ext/ext_json_parse.ml"

  | 2 ->
# 186 "ext/ext_json_parse.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 461 "ext/ext_json_parse.ml"

  | 3 ->
let
# 191 "ext/ext_json_parse.mll"
                                               c
# 467 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 192 "ext/ext_json_parse.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 474 "ext/ext_json_parse.ml"

  | 4 ->
let
# 196 "ext/ext_json_parse.mll"
                 c1
# 480 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 196 "ext/ext_json_parse.mll"
                               c2
# 485 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 196 "ext/ext_json_parse.mll"
                                             c3
# 490 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 196 "ext/ext_json_parse.mll"
                                                    s
# 495 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 197 "ext/ext_json_parse.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 506 "ext/ext_json_parse.ml"

  | 5 ->
let
# 205 "ext/ext_json_parse.mll"
                        c1
# 512 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 205 "ext/ext_json_parse.mll"
                                         c2
# 517 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 206 "ext/ext_json_parse.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 526 "ext/ext_json_parse.ml"

  | 6 ->
let
# 212 "ext/ext_json_parse.mll"
             c
# 532 "ext/ext_json_parse.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 213 "ext/ext_json_parse.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 541 "ext/ext_json_parse.ml"

  | 7 ->
# 220 "ext/ext_json_parse.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 551 "ext/ext_json_parse.ml"

  | 8 ->
# 227 "ext/ext_json_parse.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 562 "ext/ext_json_parse.ml"

  | 9 ->
# 235 "ext/ext_json_parse.mll"
      (
        error lexbuf Unterminated_string
      )
# 569 "ext/ext_json_parse.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 239 "ext/ext_json_parse.mll"
 






let rec parse_json lexbuf =
  let buf = Buffer.create 64 in 
  let look_ahead = ref None in
  let token () : token = 
    match !look_ahead with 
    | None ->  
      lex_json buf lexbuf 
    | Some x -> 
      look_ahead := None ;
      x 
  in
  let push e = look_ahead := Some e in 
  let rec json (lexbuf : Lexing.lexbuf) : Ext_json_types.t = 
    match token () with 
    | True -> True lexbuf.lex_start_p
    | False -> False lexbuf.lex_start_p
    | Null -> Null lexbuf.lex_start_p
    | Number s ->  Flo {flo = s; loc = lexbuf.lex_start_p}  
    | String s -> Str { str = s; loc =    lexbuf.lex_start_p}
    | Lbracket -> parse_array  lexbuf.lex_start_p lexbuf.lex_curr_p [] lexbuf
    | Lbrace -> parse_map lexbuf.lex_start_p String_map.empty lexbuf
    |  _ -> error lexbuf Unexpected_token
(** Note if we remove [trailing_comma] support 
    we should report errors (actually more work), for example 
    {[
    match token () with 
    | Rbracket ->
      if trailing_comma then
        error lexbuf Trailing_comma_in_array
      else
    ]} 
    {[
    match token () with 
    | Rbrace -> 
      if trailing_comma then
        error lexbuf Trailing_comma_in_obj
      else

    ]}   
 *)
  and parse_array   loc_start loc_finish acc lexbuf 
    : Ext_json_types.t =
    match token () with 
    | Rbracket ->
        Arr {loc_start ; content = Ext_array.reverse_of_list acc ; 
              loc_end = lexbuf.lex_curr_p }
    | x -> 
      push x ;
      let new_one = json lexbuf in 
      begin match token ()  with 
      | Comma -> 
          parse_array  loc_start loc_finish (new_one :: acc) lexbuf 
      | Rbracket 
        -> Arr {content = (Ext_array.reverse_of_list (new_one::acc));
                     loc_start ; 
                     loc_end = lexbuf.lex_curr_p }
      | _ -> 
        error lexbuf Expect_comma_or_rbracket
      end
  and parse_map loc_start  acc lexbuf : Ext_json_types.t = 
    match token () with 
    | Rbrace -> 
        Obj { map = acc ; loc = loc_start}
    | String key -> 
      begin match token () with 
      | Colon ->
        let value = json lexbuf in
        begin match token () with 
        | Rbrace -> Obj {map = String_map.add key value acc ; loc = loc_start}
        | Comma -> 
          parse_map loc_start  (String_map.add key value acc) lexbuf 
        | _ -> error lexbuf Expect_comma_or_rbrace
        end
      | _ -> error lexbuf Expect_colon
      end
    | _ -> error lexbuf Expect_string_or_rbrace
  in 
  let v = json lexbuf in 
  match token () with 
  | Eof -> v 
  | _ -> error lexbuf Expect_eof

let parse_json_from_string s = 
  parse_json (Lexing.from_string s )

let parse_json_from_chan fname in_chan = 
  let lexbuf = 
    Ext_position.lexbuf_from_channel_with_fname
    in_chan fname in 
  parse_json lexbuf 

let parse_json_from_file s = 
  let in_chan = open_in s in 
  let lexbuf = 
    Ext_position.lexbuf_from_channel_with_fname
    in_chan s in 
  match parse_json lexbuf with 
  | exception e -> close_in in_chan ; raise e
  | v  -> close_in in_chan;  v





# 688 "ext/ext_json_parse.ml"

end
module External_arg_spec : sig 
#1 "external_arg_spec.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type cst = private
  | Arg_int_lit of int 
  | Arg_string_lit of string 
  | Arg_js_null
  | Arg_js_true
  | Arg_js_false
  | Arg_js_json of string


type label = private
  | Label of string * cst option 
  | Empty of cst option
  | Optional of string 
  (* it will be ignored , side effect will be recorded *)

type attr = 
  | NullString of (int * string) list (* `a does not have any value*)
  | NonNullString of (int * string) list (* `a of int *)
  | Int of (int * int ) list (* ([`a | `b ] [@bs.int])*)
  | Arg_cst of cst
  | Fn_uncurry_arity of int (* annotated with [@bs.uncurry ] or [@bs.uncurry 2]*)
  (* maybe we can improve it as a combination of {!Asttypes.constant} and tuple *)
  | Array 
  | Extern_unit
  | Nothing
  | Ignore
  | Unwrap

type t = 
  {
    arg_type : attr;
    arg_label :label
  }

val cst_json : Location.t -> string -> cst 
val cst_int : int -> cst 
val cst_string : string -> cst 

val empty_label : label
val empty_lit : cst -> label 
val label :  string -> cst option -> label
val optional  : string -> label
val empty_kind : attr -> t

end = struct
#1 "external_arg_spec.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** type definitions for external argument *)

type cst = 
  | Arg_int_lit of int 
  | Arg_string_lit of string 

  | Arg_js_null
  | Arg_js_true
  | Arg_js_false
  | Arg_js_json of string
type label = 
  | Label of string * cst option 
  | Empty of cst option
  | Optional of string 
  (* it will be ignored , side effect will be recorded *)

type attr = 
  | NullString of (int * string) list (* `a does not have any value*)
  | NonNullString of (int * string) list (* `a of int *)
  | Int of (int * int ) list (* ([`a | `b ] [@bs.int])*)
  | Arg_cst of cst
  | Fn_uncurry_arity of int (* annotated with [@bs.uncurry ] or [@bs.uncurry 2]*)
    (* maybe we can improve it as a combination of {!Asttypes.constant} and tuple *)
  | Array 
  | Extern_unit
  | Nothing
  | Ignore
  | Unwrap

type t = 
  {
    arg_type : attr;
    arg_label : label
  }


exception Error of Location.t * Ext_json_parse.error

let pp_invaild_json fmt err = 
  Format.fprintf fmt "@[Invalid json literal:  %a@]@." 
    Ext_json_parse.report_error err

let () = 
  Location.register_error_of_exn (function 
    | Error (loc,err) ->       
      Some (Location.error_of_printer loc pp_invaild_json err)
    | _ -> None
    )


let cst_json (loc : Location.t) s : cst  =
  match Ext_json_parse.parse_json_from_string s with 
  | True _ -> Arg_js_true
  | False _ -> Arg_js_false 
  | Null _ -> Arg_js_null 
  | _ -> Arg_js_json s 
  | exception Ext_json_parse.Error (start,finish,error_info)
    ->
    let loc1 = {
      loc with
       loc_start = 
        Ext_position.offset loc.loc_start start; 
       loc_end =   
       Ext_position.offset loc.loc_start finish;
    } in 
     raise (Error (loc1 , error_info))

let cst_int i = Arg_int_lit i 
let cst_string s = Arg_string_lit s 
let empty_label = Empty None 
let empty_lit s = Empty (Some s) 
let label s cst = Label(s,cst)
let optional s = Optional s 

let empty_kind arg_type = { arg_label = empty_label ; arg_type }

end
module Ast_polyvar : sig 
#1 "ast_polyvar.mli"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** side effect: it will mark used attributes `bs.as`  *)
val map_row_fields_into_ints:
  Location.t -> 
  Parsetree.row_field list -> 
  (int * int ) list 

val map_constructor_declarations_into_ints:
  Parsetree.constructor_declaration list ->
  [ `Offset of int | `New  of int list ]

val map_row_fields_into_strings:
  Location.t -> 
  Parsetree.row_field list -> 
  External_arg_spec.attr


val is_enum :   
  Parsetree.row_field list -> 
  bool

val is_enum_polyvar :   
  Parsetree.type_declaration ->
  Parsetree.row_field list option 

val is_enum_constructors :   
  Parsetree.constructor_declaration list ->
  bool 
end = struct
#1 "ast_polyvar.ml"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let map_row_fields_into_ints ptyp_loc
    (row_fields : Parsetree.row_field list) 
  = 
  let _, acc
    = 
    (List.fold_left 
       (fun (i,acc) rtag -> 
          match rtag with 
          | Parsetree.Rtag (label, attrs, true,  [])
            -> 
            begin match Ast_attributes.iter_process_bs_int_as attrs with 
              | Some i -> 
                i + 1, 
                ((Ext_pervasives.hash_variant label , i):: acc ) 
              | None -> 
                i + 1 , 
                ((Ext_pervasives.hash_variant label , i):: acc )
            end
          | _ -> 
            Bs_syntaxerr.err ptyp_loc Invalid_bs_int_type
       ) (0, []) row_fields) in 
  List.rev acc

(** Note this is okay with enums, for variants,
    the underlying representation may change due to       
    unbox
*)
let map_constructor_declarations_into_ints 
    (row_fields : Parsetree.constructor_declaration list)
  = 
  let mark = ref `nothing in 
  let _, acc
    = 
    (List.fold_left 
       (fun (i,acc) (rtag : Parsetree.constructor_declaration) -> 

          let attrs = rtag.pcd_attributes in 
          begin match Ast_attributes.iter_process_bs_int_as attrs with 
            | Some j -> 
              if j <> i then 
                (
                  if i = 0 then mark := `offset j
                  else mark := `complex
                )
              ;
              (j + 1, 
               (j:: acc ) )
            | None -> 
              i + 1 , 
              ( i:: acc )
          end

       ) (0, []) row_fields) in 
  match !mark with 
  | `nothing -> `Offset 0
  | `offset j -> `Offset j 
  | `complex -> `New (List.rev acc)

(** It also check in-consistency of cases like 
    {[ [`a  | `c of int ] ]}       
*)  
let map_row_fields_into_strings ptyp_loc 
    (row_fields : Parsetree.row_field list) = 
  let case, result = 
    (Ext_list.fold_right (fun tag (nullary, acc) -> 
         match nullary, tag with 
         | (`Nothing | `Null), 
           Parsetree.Rtag (label, attrs, true,  [])
           -> 
           begin match Ast_attributes.iter_process_bs_string_as attrs with 
             | Some name -> 
               `Null, ((Ext_pervasives.hash_variant label, name) :: acc )

             | None -> 
               `Null, ((Ext_pervasives.hash_variant label, label) :: acc )
           end
         | (`Nothing | `NonNull), Parsetree.Rtag(label, attrs, false, ([ _ ])) 
           -> 
           begin match Ast_attributes.iter_process_bs_string_as attrs with 
             | Some name -> 
               `NonNull, ((Ext_pervasives.hash_variant label, name) :: acc)
             | None -> 
               `NonNull, ((Ext_pervasives.hash_variant label, label) :: acc)
           end
         | _ -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type

       ) row_fields (`Nothing, [])) in 
  (match case with 
   | `Nothing -> Bs_syntaxerr.err ptyp_loc Invalid_bs_string_type
   | `Null -> External_arg_spec.NullString result 
   | `NonNull -> NonNullString result)


let is_enum row_fields = 
  List.for_all (fun (x : Parsetree.row_field) -> 
      match x with 
      | Rtag(_label,_attrs,true, []) -> true 
      | _ -> false
    ) row_fields


let is_enum_polyvar (ty : Parsetree.type_declaration) =      
  match ty.ptype_manifest with 
  | Some {ptyp_desc = Ptyp_variant(row_fields,Closed,None)}
    when is_enum row_fields ->
    Some row_fields 
  | _ -> None 

let is_enum_constructors 
    (constructors : Parsetree.constructor_declaration list) =   
  List.for_all 
    (fun (x : Parsetree.constructor_declaration) ->
       match x with 
       | {pcd_args = []} -> true 
       | _ -> false 
    )
    constructors
end
module Bs_loc : sig 
#1 "bs_loc.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position ; 
  loc_ghost : bool
} 

val is_ghost : t -> bool
val merge : t -> t -> t 
val none : t 


end = struct
#1 "bs_loc.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type t = Location.t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position ; 
  loc_ghost : bool
} 

let is_ghost x = x.loc_ghost

let merge (l: t) (r : t) = 
  if is_ghost l then r 
  else if is_ghost r then l 
  else match l,r with 
  | {loc_start ; }, {loc_end; _} (* TODO: improve*)
    -> 
    {loc_start ;loc_end; loc_ghost = false}

let none = Location.none

end
module Bs_version : sig 
#1 "bs_version.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

val version : string

val header : string 

val package_name : string
end = struct
#1 "bs_version.ml"

(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)    
let version = "3.1.5"
let header = 
   "// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE"  
let package_name = "bs-platform"   
    
end
module External_ffi_types : sig 
#1 "external_ffi_types.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type module_bind_name =
  | Phint_name of string
  (* explicit hint name *)
  | Phint_nothing

type external_module_name =
  { bundle : string ;
    module_bind_name : module_bind_name
  }

type pipe = bool
type js_call = {
  name : string;
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list
}

type js_send = {
  name : string ;
  splice : bool ;
  pipe : pipe  ;
  js_send_scopes : string list;
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_global_val = {
  name : string ;
  external_module_name : external_module_name option;
  scopes : string list
}

type js_new_val = {
  name : string ;
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list;
}

type js_module_as_fn =
  { external_module_name : external_module_name;
    splice : bool
  }

type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label


type obj_create = External_arg_spec.t list

type js_get =
  { js_get_name : string   ;
    js_get_scopes :  string list;
  }

type js_set =
  { js_set_name : string  ;
    js_set_scopes : string list
  }


type js_get_index =   {
  js_get_index_scopes : string list
}

type js_set_index = {
  js_set_index_scopes : string list
}



type attr  =
  | Js_global of js_global_val
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of js_module_as_fn
  | Js_module_as_class of external_module_name
  | Js_call of js_call
  | Js_send of js_send
  | Js_new of js_new_val
  | Js_set of js_set
  | Js_get of js_get
  | Js_get_index of js_get_index
  | Js_set_index of js_set_index

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit

type t  =
  | Ffi_bs of
      External_arg_spec.t list  *
      return_wrapper *
      attr
  | Ffi_obj_create of obj_create
  | Ffi_normal
  (* When it's normal, it is handled as normal c functional ffi call *)


val name_of_ffi : attr -> string

val check_ffi : ?loc:Location.t ->  attr -> unit

val to_string : t -> string

(** Note *)
val from_string : string -> t


end = struct
#1 "external_ffi_types.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type module_bind_name =
  | Phint_name of string
    (* explicit hint name *)

  | Phint_nothing


type external_module_name =
  { bundle : string ;
    module_bind_name : module_bind_name
  }

type pipe = bool
type js_call = {
  name : string;
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list ;
}

type js_send = {
  name : string ;
  splice : bool ;
  pipe : pipe   ;
  js_send_scopes : string list;
} (* we know it is a js send, but what will happen if you pass an ocaml objct *)

type js_global_val = {
  name : string ;
  external_module_name : external_module_name option;
  scopes : string list ;
}

type js_new_val = {
  name : string ;
  external_module_name : external_module_name option;
  splice : bool ;
  scopes : string list;
}

type js_module_as_fn =
  { external_module_name : external_module_name;
    splice : bool ;

  }
type js_get =
  { js_get_name : string   ;
    js_get_scopes :  string list;
  }

type js_set =
  { js_set_name : string  ;
    js_set_scopes : string list
  }

type js_get_index =   {
  js_get_index_scopes : string list
}

type js_set_index = {
  js_set_index_scopes : string list
}
(** TODO: information between [arg_type] and [arg_label] are duplicated,
  design a more compact representation so that it is also easy to seralize by hand
*)
type arg_type = External_arg_spec.attr

type arg_label = External_arg_spec.label


(**TODO: maybe we can merge [arg_label] and [arg_type] *)
type obj_create = External_arg_spec.t list

type attr =
  | Js_global of js_global_val
  | Js_module_as_var of  external_module_name
  | Js_module_as_fn of js_module_as_fn
  | Js_module_as_class of external_module_name
  | Js_call of js_call
  | Js_send of js_send
  | Js_new of js_new_val
  | Js_set of js_set
  | Js_get of js_get
  | Js_get_index of js_get_index
  | Js_set_index of js_set_index

let name_of_ffi ffi =
  match ffi with
  | Js_get_index _scope -> "[@@bs.get_index ..]"
  | Js_set_index _scope -> "[@@bs.set_index ..]"
  | Js_get { js_get_name = s} -> Printf.sprintf "[@@bs.get %S]" s
  | Js_set { js_set_name = s} -> Printf.sprintf "[@@bs.set %S]" s
  | Js_call v  -> Printf.sprintf "[@@bs.val %S]" v.name
  | Js_send v  -> Printf.sprintf "[@@bs.send %S]" v.name
  | Js_module_as_fn v  -> Printf.sprintf "[@@bs.val %S]" v.external_module_name.bundle
  | Js_new v  -> Printf.sprintf "[@@bs.new %S]" v.name
  | Js_module_as_class v
    -> Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_module_as_var v
    ->
    Printf.sprintf "[@@bs.module] %S " v.bundle
  | Js_global v
    ->
    Printf.sprintf "[@@bs.val] %S " v.name

type return_wrapper =
  | Return_unset
  | Return_identity
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
  | Return_replaced_with_unit
type t  =
  | Ffi_bs of External_arg_spec.t list  *
     return_wrapper * attr
  (**  [Ffi_bs(args,return,attr) ]
       [return] means return value is unit or not,
        [true] means is [unit]
  *)
  | Ffi_obj_create of obj_create
  | Ffi_normal
  (* When it's normal, it is handled as normal c functional ffi call *)



let valid_js_char =
  let a = Array.init 256 (fun i ->
      let c = Char.chr i in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '$'
    ) in
  (fun c -> Array.unsafe_get a (Char.code c))

let valid_first_js_char =
  let a = Array.init 256 (fun i ->
      let c = Char.chr i in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
    ) in
  (fun c -> Array.unsafe_get a (Char.code c))

(** Approximation could be improved *)
let valid_ident (s : string) =
  let len = String.length s in
  len > 0 && valid_js_char s.[0] && valid_first_js_char s.[0] &&
  (let module E = struct exception E end in
   try
     for i = 1 to len - 1 do
       if not (valid_js_char (String.unsafe_get s i)) then
         raise E.E
     done ;
     true
   with E.E -> false )

let valid_global_name ?loc txt =
  if not (valid_ident txt) then
    let v = Ext_string.split_by ~keep_empty:true (fun x -> x = '.') txt in
    List.iter
      (fun s ->
         if not (valid_ident s) then
           Location.raise_errorf ?loc "Not a valid global name %s"  txt
      ) v

(*
  We loose such check (see #2583),
  it also helps with the implementation deriving abstract [@bs.as]
*)

let valid_method_name ?loc txt =
    ()
  (* if not (valid_ident txt) then
    Location.raise_errorf ?loc "Not a valid method name %s"  txt *)



let check_external_module_name ?loc x =
  match x with
  | {bundle = ""; _ }
  | { module_bind_name = Phint_name "" } ->
    Location.raise_errorf ?loc "empty name encountered"
  | _ -> ()
let check_external_module_name_opt ?loc x =
  match x with
  | None -> ()
  | Some v -> check_external_module_name ?loc v


let check_ffi ?loc ffi =
  match ffi with
  | Js_global {name} -> valid_global_name ?loc  name
  | Js_send {name }
  | Js_set  {js_set_name = name}
  | Js_get { js_get_name = name}
    ->  valid_method_name ?loc name
  | Js_get_index  _ (* TODO: check scopes *)
  | Js_set_index _
    -> ()

  | Js_module_as_var external_module_name
  | Js_module_as_fn {external_module_name; _}
  | Js_module_as_class external_module_name
    -> check_external_module_name external_module_name
  | Js_new {external_module_name ;  name}
  | Js_call {external_module_name ;  name ; _}
    ->
    check_external_module_name_opt ?loc external_module_name ;
    valid_global_name ?loc name

let bs_prefix = "BS:"
let bs_prefix_length = String.length bs_prefix


(** TODO: Make sure each version is not prefix of each other
    Solution:
    1. fixed length
    2. non-prefix approach
*)
let bs_external = bs_prefix ^ Bs_version.version


let bs_external_length = String.length bs_external


let to_string  t =
  bs_external ^ Marshal.to_string t []


(* TODO:  better error message when version mismatch *)
let from_string s : t =
  let s_len = String.length s in
  if s_len >= bs_prefix_length &&
     String.unsafe_get s 0 = 'B' &&
     String.unsafe_get s 1 = 'S' &&
     String.unsafe_get s 2 = ':' then
    if Ext_string.starts_with s bs_external then
      Marshal.from_string s bs_external_length
    else
      Ext_pervasives.failwithf
        ~loc:__LOC__
        "Compiler version mismatch. The project might have been built with one version of BuckleScript, and then with another. Please wipe the artifacts and do a clean build."
  else Ffi_normal

end
module Bs_hash_stubs
= struct
#1 "bs_hash_stubs.ml"


external hash_string :  string -> int = "caml_bs_hash_string" "noalloc";;

external hash_string_int :  string -> int  -> int = "caml_bs_hash_string_and_int" "noalloc";;

external hash_string_small_int :  string -> int  -> int = "caml_bs_hash_string_and_small_int" "noalloc";;

external hash_stamp_and_name : int -> string -> int = "caml_bs_hash_stamp_and_name" "noalloc";;

external hash_small_int : int -> int = "caml_bs_hash_small_int" "noalloc";;

external hash_int :  int  -> int = "caml_bs_hash_int" "noalloc";;

external string_length_based_compare : string -> string -> int  = "caml_string_length_based_compare" "noalloc";;


external    
    int_unsafe_blit : 
    int array -> int -> int array -> int -> int -> unit = "caml_int_array_blit" "noalloc";;
    

end
module String_hash_set : sig 
#1 "string_hash_set.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


include Hash_set_gen.S with type key = string

end = struct
#1 "string_hash_set.ml"
# 1 "ext/hash_set.cppo.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
# 31
type key = string 
let key_index (h :  _ Hash_set_gen.t ) (key : key) =
  (Bs_hash_stubs.hash_string  key) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 
type  t = key  Hash_set_gen.t 


# 62
let create = Hash_set_gen.create
let clear = Hash_set_gen.clear
let reset = Hash_set_gen.reset
let copy = Hash_set_gen.copy
let iter = Hash_set_gen.iter
let fold = Hash_set_gen.fold
let length = Hash_set_gen.length
let stats = Hash_set_gen.stats
let elements = Hash_set_gen.elements



let remove (h : _ Hash_set_gen.t) key =  
  let i = key_index h key in
  let h_data = h.data in
  let old_h_size = h.size in 
  let new_bucket = Hash_set_gen.remove_bucket eq_key key h (Array.unsafe_get h_data i) in
  if old_h_size <> h.size then  
    Array.unsafe_set h_data i new_bucket



let add (h : _ Hash_set_gen.t) key =
  let i = key_index h key  in 
  let h_data = h.data in 
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i (key :: old_bucket);
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h
    end

let of_array arr = 
  let len = Array.length arr in 
  let tbl = create len in 
  for i = 0 to len - 1  do
    add tbl (Array.unsafe_get arr i);
  done ;
  tbl 
  
    
let check_add (h : _ Hash_set_gen.t) key =
  let i = key_index h key  in 
  let h_data = h.data in  
  let old_bucket = (Array.unsafe_get h_data i) in
  if not (Hash_set_gen.small_bucket_mem eq_key key old_bucket) then 
    begin 
      Array.unsafe_set h_data i  (key :: old_bucket);
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_set_gen.resize key_index h;
      true 
    end
  else false 


let mem (h :  _ Hash_set_gen.t) key =
  Hash_set_gen.small_bucket_mem eq_key key (Array.unsafe_get h.data (key_index h key)) 

  

end
module Lam_methname : sig 
#1 "lam_methname.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



val translate : ?loc:Location.t -> string -> string

end = struct
#1 "lam_methname.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


(**
  {[
    _open -> open 
    _in -> in 
    _MAX_LENGTH -> MAX_LENGTH
    _Capital -> Capital 
    
    _open__ ->  _open
    open__ -> open 
    
    _'x -> 'x 

    _Capital__ -> _Capital 
    _MAX__ -> _MAX
    __ -> __ 
    __x -> __x 
    ___ -> _     
    ____ -> __
    _ -> _  (* error *)   
    

  ]}
  First we scan '__' from end to start, 
  If found, discard it.
  Otherwise, check if it is [_ + keyword] or followed by capital letter,
  If so, discard [_].

  Limitations: user can not have [_Capital__, _Capital__other] to 
  make it all compile to [Capital].
  Keyword is fine [open__, open__other].
  So we loose polymorphism over capital letter. 
  It is okay, otherwise, if [_Captial__] is interpreted as [Capital], then
  there is no way to express [_Capital]
*)

(* Copied from [ocaml/parsing/lexer.mll] *)
let key_words = String_hash_set.of_array [|
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
(*  "parser", PARSER; *)
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";

    "mod";
    "land";
    "lor";
    "lxor";
    "lsl";
    "lsr";
    "asr";
|]
let double_underscore = "__"

(*https://caml.inria.fr/pub/docs/manual-ocaml/lex.html
{[

  label-name	::=	 lowercase-ident 
]}
*)
let valid_start_char x =
  match x with 
  | '_' | 'a' .. 'z' -> true 
  | _ -> false 
let translate ?loc name = 
  assert (not @@ Ext_string.is_empty name);
  let i = Ext_string.rfind ~sub:double_underscore name in 
  if i < 0 then 
    let name_len = String.length name in 
    if name.[0] = '_' then  begin 
      let try_key_word = (String.sub name 1 (name_len - 1)) in 
      if name_len > 1 && 
        (not (valid_start_char try_key_word.[0])
        || String_hash_set.mem key_words try_key_word)  then 
        try_key_word
      else 
        name 
    end
    else name 
  else if i = 0 then name 
  else  String.sub name 0 i 


end
module External_process : sig 
#1 "external_process.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)





(**
  [handle_attributes_as_string
  loc pval_name.txt pval_type pval_attributes pval_prim]
  [pval_name.txt] is the name of identifier
  [pval_prim] is the name of string literal

  return value is of [pval_type, pval_prims, new_attrs]
*)
val handle_attributes_as_string :
  Bs_loc.t ->
  string  ->
  Ast_core_type.t ->
  Ast_attributes.t ->
  string   ->
  Ast_core_type.t * string list * Ast_attributes.t




(** [pval_prim_of_labels labels]
    return [pval_prims] for FFI, it is specialized for
    external object which is used in
    {[ [%obj { x = 2; y = 1} ] ]}
*)
val pval_prim_of_labels : string Asttypes.loc list -> string list


val pval_prim_of_option_labels :
  (bool * string Asttypes.loc) list ->
  bool ->
  string list

end = struct
#1 "external_process.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


[@@@ocaml.warning "+9"]
(* record pattern match complete checker*)


let variant_can_bs_unwrap_fields (row_fields : Parsetree.row_field list) : bool =
  let validity =
    List.fold_left
      begin fun st row ->
        match st, row with
        | (* we've seen no fields or only valid fields so far *)
          (`No_fields | `Valid_fields),
          (* and this field has one constructor arg that we can unwrap to *)
          Parsetree.Rtag (label, attrs, false, ([ _ ]))
          ->
          `Valid_fields
        | (* otherwise, this field or a previous field was invalid *)
          _ ->
          `Invalid_field
      end
      `No_fields
      row_fields
  in
  match validity with
  | `Valid_fields -> true
  | `No_fields
  | `Invalid_field -> false


(** Given the type of argument, process its [bs.] attribute and new type,
    The new type is currently used to reconstruct the external type
    and result type in [@@bs.obj]
    They are not the same though, for example
    {[
      external f : hi:([ `hi | `lo ] [@bs.string]) -> unit -> _ = "" [@@bs.obj]
    ]}
    The result type would be [ hi:string ]
*)
let get_arg_type 
    ~nolabel optional
    (ptyp : Ast_core_type.t) :
  External_arg_spec.attr * Ast_core_type.t  =
  let ptyp =
    if optional then
      Ast_core_type.extract_option_type_exn ptyp
    else ptyp in
  if Ast_core_type.is_any ptyp then (* (_[@bs.as ])*)
    if optional then
      Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external
    else begin
      let ptyp_attrs = ptyp.ptyp_attributes in
      let result = Ast_attributes.iter_process_bs_string_or_int_as ptyp_attrs in
      (* when ppx start dropping attributes
        we should warn, there is a trade off whether
        we should warn dropped non bs attribute or not
      *)
      Bs_ast_invariant.warn_unused_attributes ptyp_attrs;
      match result with
      |  None ->
        Bs_syntaxerr.err ptyp.ptyp_loc Invalid_underscore_type_in_external
      | Some (`Int i) ->
        Arg_cst(External_arg_spec.cst_int i), Ast_literal.type_int ~loc:ptyp.ptyp_loc ()
      | Some (`Str i)->
        Arg_cst (External_arg_spec.cst_string i), Ast_literal.type_string ~loc:ptyp.ptyp_loc ()
      | Some (`Json_str s) ->
        Arg_cst (External_arg_spec.cst_json ptyp.ptyp_loc s),
        Ast_literal.type_string ~loc:ptyp.ptyp_loc ()

    end
  else (* ([`a|`b] [@bs.string]) *)
    let ptyp_desc = ptyp.ptyp_desc in
    (match Ast_attributes.iter_process_bs_string_int_unwrap_uncurry ptyp.ptyp_attributes with
    | `String ->
      begin match ptyp_desc with
        | Ptyp_variant ( row_fields, Closed, None)
          ->          
          Ast_polyvar.map_row_fields_into_strings ptyp.ptyp_loc row_fields
        | _ ->
          Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_string_type
      end
    | `Ignore ->
      Ignore
    | `Int ->
      begin match ptyp_desc with
        | Ptyp_variant ( row_fields, Closed, None) ->
          let int_lists =
            Ast_polyvar.map_row_fields_into_ints ptyp.ptyp_loc row_fields in
          Int int_lists
        | _ -> Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_int_type
      end
    | `Unwrap ->
      begin match ptyp_desc with
        | Ptyp_variant (row_fields, Closed, _)
          when variant_can_bs_unwrap_fields row_fields ->
          Unwrap
        | _ ->
          Bs_syntaxerr.err ptyp.ptyp_loc Invalid_bs_unwrap_type
      end
    | `Uncurry opt_arity ->
      let real_arity =  Ast_core_type.get_uncurry_arity ptyp in
      (begin match opt_arity, real_arity with
         | Some arity, `Not_function ->
           Fn_uncurry_arity arity
         | None, `Not_function  ->
           Bs_syntaxerr.err ptyp.ptyp_loc Canot_infer_arity_by_syntax
         | None, `Arity arity  ->
           Fn_uncurry_arity arity
         | Some arity, `Arity n ->
           if n <> arity then
             Bs_syntaxerr.err ptyp.ptyp_loc (Inconsistent_arity (arity,n))
           else Fn_uncurry_arity arity
       end)
    | `Nothing ->
      begin match ptyp_desc with
        | Ptyp_constr ({txt = Lident "unit"; _}, [])
          -> if nolabel then Extern_unit else  Nothing
        | Ptyp_constr ({txt = Lident "array"; _}, [_])
          -> Array
        | Ptyp_variant _ ->
          Bs_warnings.prerr_bs_ffi_warning ptyp.ptyp_loc Unsafe_poly_variant_type;
          Nothing
        | _ ->
          Nothing
      end), ptyp



(**
   [@@bs.module "react"]
   [@@bs.module "react"]
   ---
   [@@bs.module "@" "react"]
   [@@bs.module "@" "react"]

   They should have the same module name

   TODO: we should emit an warning if we bind
   two external files to the same module name
*)
type bundle_source =
  [`Nm_payload of string (* from payload [@@bs.val "xx" ]*)
  |`Nm_external of string (* from "" in external *)
  | `Nm_val of string   (* from function name *)
  ]

let string_of_bundle_source (x : bundle_source) =
  match x with
  | `Nm_payload x
  | `Nm_external x
  | `Nm_val x -> x
type name_source =
  [ bundle_source
  | `Nm_na

  ]




type st =
  { val_name : name_source;
    external_module_name : External_ffi_types.external_module_name option;
    module_as_val : External_ffi_types.external_module_name option;
    val_send : name_source ;
    val_send_pipe : Ast_core_type.t option;
    splice : bool ; (* mutable *)
    scopes : string list ;
    set_index : bool; (* mutable *)
    get_index : bool;
    new_name : name_source ;
    call_name : name_source ;
    set_name : name_source ;
    get_name : name_source ;

    mk_obj : bool ;
    return_wrapper : External_ffi_types.return_wrapper ;

  }

let init_st =
  {
    val_name = `Nm_na;
    external_module_name = None ;
    module_as_val = None;
    val_send = `Nm_na;
    val_send_pipe = None;
    splice = false;
    scopes = [];
    set_index = false;
    get_index = false;
    new_name = `Nm_na;
    call_name = `Nm_na;
    set_name = `Nm_na ;
    get_name = `Nm_na ;
    mk_obj = false ;
    return_wrapper = Return_unset;

  }





let process_external_attributes
    no_arguments
    (prim_name_or_pval_prim: [< bundle_source ] as 'a)
    pval_prim
    (prim_attributes : Ast_attributes.t) : _ * Ast_attributes.t =

  (* shared by `[@@bs.val]`, `[@@bs.send]`,
     `[@@bs.set]`, `[@@bs.get]` , `[@@bs.new]`
     `[@@bs.send.pipe]` does not use it
  *)
  let name_from_payload_or_prim ~loc (payload : Parsetree.payload) : name_source =
    match payload with
    | PStr [] ->
      (prim_name_or_pval_prim :> name_source)
    (* It is okay to have [@@bs.val] without payload *)
    | _ ->
      begin match Ast_payload.is_single_string payload with
        | Some  (val_name, _) ->  `Nm_payload val_name
        | None ->
          Location.raise_errorf ~loc "Invalid payload"
      end

  in
  List.fold_left
    (fun (st, attrs)
      (({txt ; loc}, payload) as attr : Ast_attributes.attr)
      ->
        if Ext_string.starts_with txt "bs." then
          begin match txt with
            | "bs.val" ->
              if no_arguments then
                {st with val_name = name_from_payload_or_prim ~loc payload}
              else
                {st with call_name = name_from_payload_or_prim ~loc  payload}

            | "bs.module" ->
              begin match Ast_payload.assert_strings loc payload with
                | [bundle] ->
                  {st with external_module_name =
                             Some {bundle; module_bind_name = Phint_nothing}}
                | [bundle;bind_name] ->
                  {st with external_module_name =
                             Some {bundle; module_bind_name = Phint_name bind_name}}
                | [] ->
                  { st with
                    module_as_val =
                      Some
                        { bundle =
                            string_of_bundle_source
                              (prim_name_or_pval_prim :> bundle_source) ;
                          module_bind_name = Phint_nothing}
                  }
                | _  ->
                  Bs_syntaxerr.err loc Illegal_attribute
              end
            | "bs.scope" ->
              begin match Ast_payload.assert_strings loc payload with
                | [] ->
                  Bs_syntaxerr.err loc Illegal_attribute
                (* We need err on empty scope, so we can tell the difference
                   between unset/set
                *)
                | scopes ->  { st with scopes = scopes }
              end
            | "bs.splice" -> {st with splice = true}
            | "bs.send" ->
              { st with val_send = name_from_payload_or_prim ~loc payload}
            | "bs.send.pipe"
              ->
              { st with val_send_pipe = Some (Ast_payload.as_core_type loc payload)}
            | "bs.set" ->
              {st with set_name = name_from_payload_or_prim ~loc  payload}
            | "bs.get" -> {st with get_name = name_from_payload_or_prim ~loc payload}

            | "bs.new" -> {st with new_name = name_from_payload_or_prim ~loc payload}
            | "bs.set_index" -> {st with set_index = true}
            | "bs.get_index"-> {st with get_index = true}
            | "bs.obj" -> {st with mk_obj = true}
            | "bs.return" ->
              let aux loc txt : External_ffi_types.return_wrapper =
                begin match txt with
                  | "undefined_to_opt" -> Return_undefined_to_opt
                  | "null_to_opt" -> Return_null_to_opt
                  | "nullable"
                  | "null_undefined_to_opt" -> Return_null_undefined_to_opt
                  | "identity" -> Return_identity
                  | _ ->
                    Bs_syntaxerr.err loc Not_supported_directive_in_bs_return
                end in
              let actions =
                Ast_payload.ident_or_record_as_config loc payload
              in
              begin match actions with
                | [ ({txt; _ },None) ] ->
                  { st with return_wrapper = aux loc txt}
                | _ ->
                  Bs_syntaxerr.err loc Not_supported_directive_in_bs_return
              end
            | _ -> (Location.prerr_warning loc (Bs_unused_attribute txt); st)
          end, attrs
        else (st , attr :: attrs)
    )
    (init_st, []) prim_attributes


let rec has_bs_uncurry (attrs : Ast_attributes.t) =
  match attrs with
  | ({txt = "bs.uncurry"; _ }, _) :: attrs ->
    true
  | _ :: attrs -> has_bs_uncurry attrs
  | [] -> false


let check_return_wrapper
    loc (wrapper : External_ffi_types.return_wrapper)
    result_type =
  match wrapper with
  | Return_identity -> wrapper
  | Return_unset  ->
    if Ast_core_type.is_unit result_type then
      Return_replaced_with_unit
    else
      wrapper
  | Return_undefined_to_opt
  | Return_null_to_opt
  | Return_null_undefined_to_opt
    ->
    if Ast_core_type.is_user_option result_type then
      wrapper
    else
      Bs_syntaxerr.err loc Expect_opt_in_bs_return_to_opt
  | Return_replaced_with_unit ->
    assert false (* Not going to happen from user input*)




(** Note that the passed [type_annotation] is already processed by visitor pattern before
*)
let handle_attributes
    (loc : Bs_loc.t)
    (pval_prim : string )
    (type_annotation : Parsetree.core_type)
    (prim_attributes : Ast_attributes.t) (prim_name : string)
  : Ast_core_type.t * string * External_ffi_types.t * Ast_attributes.t =
  (** sanity check here
      {[ int -> int -> (int -> int -> int [@bs.uncurry])]}
      It does not make sense
  *)
  if has_bs_uncurry type_annotation.ptyp_attributes then
    begin
      Location.raise_errorf
        ~loc "[@@bs.uncurry] can not be applied to the whole definition"
    end;

  let prim_name_or_pval_prim =
    if String.length prim_name = 0 then  `Nm_val pval_prim
    else  `Nm_external prim_name  (* need check name *)
  in
  let result_type, arg_types_ty = 
    (* Note this assumes external type is syntatic (no abstraction)*)
    Ast_core_type.list_of_arrow type_annotation in
  if has_bs_uncurry result_type.ptyp_attributes then
    begin
      Location.raise_errorf
        ~loc:result_type.ptyp_loc
        "[@@bs.uncurry] can not be applied to tailed position"
    end ;
  let (st, left_attrs) =
    process_external_attributes
      (arg_types_ty = [])
      prim_name_or_pval_prim pval_prim prim_attributes in


  if st.mk_obj then
    begin match st with
      | {
        val_name = `Nm_na;
        external_module_name = None ;
        module_as_val = None;
        val_send = `Nm_na;
        val_send_pipe = None;
        splice = false;
        new_name = `Nm_na;
        call_name = `Nm_na;
        set_name = `Nm_na ;
        get_name = `Nm_na ;
        get_index = false ;
        return_wrapper = Return_unset ;
        set_index = false ;
        mk_obj = _;
        scopes = [];
        (* wrapper does not work with [bs.obj]
           TODO: better error message *)
      } ->
        if String.length prim_name <> 0 then
          Location.raise_errorf ~loc "[@@bs.obj] expect external names to be empty string";
        let arg_kinds, new_arg_types_ty, result_types =
          Ext_list.fold_right
            (fun (label,ty,attr,loc) ( arg_labels, arg_types, result_types) ->
               let arg_label = Ast_core_type.label_name label in
               let new_arg_label, new_arg_types,  output_tys =
                 match arg_label with
                 | Empty ->
                   let arg_type, new_ty = get_arg_type ~nolabel:true false ty in
                   begin match arg_type with
                     | Extern_unit ->
                       External_arg_spec.empty_kind arg_type, (label,new_ty,attr,loc)::arg_types, result_types
                     | _ ->
                       Location.raise_errorf ~loc "expect label, optional, or unit here"
                   end
                 | Label name ->
                   let arg_type, new_ty = get_arg_type ~nolabel:false false ty in
                   begin match arg_type with
                     | Ignore ->
                       External_arg_spec.empty_kind arg_type,
                       (label,new_ty,attr,loc)::arg_types, result_types
                     | Arg_cst  i  ->
                       let s = (Lam_methname.translate ~loc name) in
                       {arg_label = External_arg_spec.label s (Some i);
                        arg_type },
                       arg_types, (* ignored in [arg_types], reserved in [result_types] *)
                       ((name , [], new_ty) :: result_types)
                     | Nothing | Array ->
                       let s = (Lam_methname.translate ~loc name) in
                       {arg_label = External_arg_spec.label s None ; arg_type },
                       (label,new_ty,attr,loc)::arg_types,
                       ((name , [], new_ty) :: result_types)
                     | Int _  ->
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.label s None; arg_type},
                       (label,new_ty,attr,loc)::arg_types,
                       ((name, [], Ast_literal.type_int ~loc ()) :: result_types)
                     | NullString _ ->
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.label s None; arg_type},
                       (label,new_ty,attr,loc)::arg_types,
                       ((name, [], Ast_literal.type_string ~loc ()) :: result_types)
                     | Fn_uncurry_arity _ ->
                       Location.raise_errorf ~loc
                         "The combination of [@@bs.obj], [@@bs.uncurry] is not supported yet"
                     | Extern_unit -> assert false
                     | NonNullString _
                       ->
                       Location.raise_errorf ~loc
                         "bs.obj label %s does not support such arg type" name
                     | Unwrap ->
                       Location.raise_errorf ~loc
                         "bs.obj label %s does not support [@bs.unwrap] arguments" name
                   end
                 | Optional name ->
                   let arg_type, new_ty_extract = get_arg_type ~nolabel:false true ty in
                   let new_ty = Ast_core_type.lift_option_type new_ty_extract in
                   begin match arg_type with
                     | Ignore ->
                       External_arg_spec.empty_kind arg_type,
                       (label,new_ty,attr,loc)::arg_types, result_types

                     | Nothing | Array ->
                       let s = (Lam_methname.translate ~loc name) in
                       {arg_label = External_arg_spec.optional s; arg_type},
                       (label,new_ty,attr,loc)::arg_types,
                       ( (name, [], Ast_comb.to_undefined_type loc new_ty_extract) ::  result_types)
                     | Int _  ->
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.optional s ; arg_type },
                       (label,new_ty,attr,loc)::arg_types,
                       ((name, [], Ast_comb.to_undefined_type loc @@ Ast_literal.type_int ~loc ()) :: result_types)
                     | NullString _  ->
                       let s = Lam_methname.translate ~loc name in
                       {arg_label = External_arg_spec.optional s ; arg_type },
                       (label,new_ty,attr,loc)::arg_types,
                       ((name, [], Ast_comb.to_undefined_type loc @@ Ast_literal.type_string ~loc ()) :: result_types)
                     | Arg_cst _
                       ->
                       Location.raise_errorf ~loc "bs.as is not supported with optional yet"
                     | Fn_uncurry_arity _ ->
                       Location.raise_errorf ~loc
                         "The combination of [@@bs.obj], [@@bs.uncurry] is not supported yet"
                     | Extern_unit   -> assert false
                     | NonNullString _
                       ->
                       Location.raise_errorf ~loc
                         "bs.obj label %s does not support such arg type" name
                     | Unwrap ->
                       Location.raise_errorf ~loc
                         "bs.obj label %s does not support [@bs.unwrap] arguments" name
                   end
               in
               (
                 new_arg_label::arg_labels,
                 new_arg_types,
                 output_tys)) arg_types_ty
            ( [], [], []) in

        let result =
          if Ast_core_type.is_any  result_type then
            Ast_core_type.make_obj ~loc result_types
          else
            snd @@ get_arg_type ~nolabel:true false result_type (* result type can not be labeled *)

        in
        begin
          (
            Ext_list.fold_right (fun (label,ty,attrs,loc) acc ->
                Ast_helper.Typ.arrow ~loc  ~attrs label ty acc
              ) new_arg_types_ty result
          ) ,
          prim_name,
          Ffi_obj_create arg_kinds,
          left_attrs
        end

      | _ -> Location.raise_errorf ~loc "Attribute found that conflicts with [@@bs.obj]"

    end

  else
    let splice = st.splice in
    let arg_type_specs, new_arg_types_ty, arg_type_specs_length   =
      Ext_list.fold_right
        (fun (label,ty,attr,loc) (arg_type_specs, arg_types, i) ->
           let arg_label = Ast_core_type.label_name label in
           let arg_label, arg_type, new_arg_types =
             match arg_label with
             | Optional s  ->

               let arg_type , new_ty = get_arg_type ~nolabel:false true ty in
               begin match arg_type with
                 | NonNullString _ ->
                   (* ?x:([`x of int ] [@bs.string]) does not make sense *)
                   Location.raise_errorf
                     ~loc
                     "[@@bs.string] does not work with optional when it has arities in label %s" label
                 | _ ->
                   External_arg_spec.optional s, arg_type,
                   ((label, Ast_core_type.lift_option_type new_ty , attr,loc) :: arg_types) end
             | Label s  ->
               begin match get_arg_type ~nolabel:false false  ty with
                 | (Arg_cst ( i) as arg_type), new_ty ->
                   External_arg_spec.label s (Some i), arg_type, arg_types
                 | arg_type, new_ty ->
                   External_arg_spec.label s None, arg_type, (label, new_ty,attr,loc) :: arg_types
               end
             | Empty ->
               begin match get_arg_type ~nolabel:true false  ty with
                 | (Arg_cst ( i) as arg_type), new_ty ->
                   External_arg_spec.empty_lit i , arg_type,  arg_types
                 | arg_type, new_ty ->
                   External_arg_spec.empty_label, arg_type, (label, new_ty,attr,loc) :: arg_types
               end
           in
           (if i = 0 && splice  then
              match arg_type with
              | Array  -> ()
              | _ ->  Location.raise_errorf ~loc "[@@@@bs.splice] expect the last type to be an array");
           ({ External_arg_spec.arg_label  ;
              arg_type
            } :: arg_type_specs,
            new_arg_types,
            if arg_type = Ignore then i
            else i + 1
           )
        ) arg_types_ty
        (match st with
         | {val_send_pipe = Some obj; _ } ->
           let arg_type, new_ty = get_arg_type ~nolabel:true false obj in
           begin match arg_type with
             | Arg_cst _ ->
               Location.raise_errorf ~loc:obj.ptyp_loc "[@bs.as] is not supported in bs.send type "
             | _ ->
               (* more error checking *)
               [External_arg_spec.empty_kind arg_type]
               ,
               ["", new_ty, [], obj.ptyp_loc]
               ,0
           end

         | {val_send_pipe = None ; _ } -> [],[], 0) in

    let ffi : External_ffi_types.attr  = match st with
      | {set_index = true;

         val_name = `Nm_na;
         external_module_name = None ;
         module_as_val = None;
         val_send = `Nm_na;
         val_send_pipe = None;
         splice = false;
         scopes ;
         get_index = false;
         new_name = `Nm_na;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;

         return_wrapper = _;
         mk_obj = _ ;

        }
        ->
        if String.length prim_name <> 0 then
          Location.raise_errorf ~loc "[@@bs.set_index] expect external names to be empty string";
        if arg_type_specs_length = 3 then
          Js_set_index {js_set_index_scopes = scopes}
        else
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.set_index](arity of 3)"

      | {set_index = true; _}
        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.set_index]")


      | {get_index = true;

         val_name = `Nm_na;
         external_module_name = None ;
         module_as_val = None;
         val_send = `Nm_na;
         val_send_pipe = None;

         splice = false;
         scopes ;
         new_name = `Nm_na;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         set_index = false;
         mk_obj;
         return_wrapper ;
        } ->
        if String.length prim_name <> 0 then
          Location.raise_errorf ~loc "[@@bs.get_index] expect external names to be empty string";
        if arg_type_specs_length = 2 then
          Js_get_index {js_get_index_scopes = scopes}
        else Location.raise_errorf ~loc
            "Ill defined attribute [@@bs.get_index] (arity expected 2 : while %d)" arg_type_specs_length

      | {get_index = true; _}

        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.get_index]")




      | {module_as_val = Some external_module_name ;

         get_index = false;
         val_name ;
         new_name ;

         external_module_name = None ;
         val_send = `Nm_na;
         val_send_pipe = None;
         scopes = []; (* module as var does not need scopes *)
         splice;
         call_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         set_index = false;
         return_wrapper = _;
         mk_obj = _ ;
        } ->
        begin match arg_types_ty, new_name, val_name  with
          | [], `Nm_na,  _ -> Js_module_as_var external_module_name
          | _, `Nm_na, _ -> Js_module_as_fn {splice; external_module_name }
          | _, #bundle_source, #bundle_source ->
            Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.module].")

          | _, (`Nm_val _ | `Nm_external _) , `Nm_na
            -> Js_module_as_class external_module_name
          | _, `Nm_payload _ , `Nm_na
            ->
            Location.raise_errorf ~loc
              "Incorrect FFI attribute found: (bs.new should not carry a payload here)"
        end
      | {module_as_val = Some x; _}
        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.module].")

      | {call_name = (`Nm_val name | `Nm_external name | `Nm_payload name) ;
         splice;
         scopes ;
         external_module_name;

         val_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = None;

         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         mk_obj = _ ;
         return_wrapper = _ ;
        } ->
        Js_call {splice; name; external_module_name; scopes }
      | {call_name = #bundle_source ; _ }
        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.val]")


      | {val_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
         external_module_name;

         call_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na;
         mk_obj = _;
         return_wrapper = _;
         splice = false ;
         scopes ;
        }
        ->
        Js_global { name; external_module_name; scopes}
      | {val_name = #bundle_source ; _ }
        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.val]")

      | {splice ;
         scopes ;
         external_module_name = (Some _ as external_module_name);

         val_name = `Nm_na ;
         call_name = `Nm_na ;
         module_as_val = None;
         val_send = `Nm_na ;
         val_send_pipe = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         mk_obj = _ ;
         return_wrapper= _ ;
        }
        ->
        let name = string_of_bundle_source prim_name_or_pval_prim in
        if arg_type_specs_length  = 0 then
          Js_global { name; external_module_name; scopes}
        else  Js_call {splice; name; external_module_name; scopes}
      | {val_send = (`Nm_val name | `Nm_external name | `Nm_payload name);
         splice;
         scopes;
         val_send_pipe = None;
         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None ;
         mk_obj = _ ;
         return_wrapper = _ ;
        } ->

        (* PR #2162 - since when we assemble arguments the first argument in
           [@@bs.send] is ignored
        *)
        begin match arg_type_specs with
          | [] ->
            Location.raise_errorf
              ~loc "Ill defined attribute [@@bs.send] (at least one argument)"
          |  {arg_type = Arg_cst _ ; arg_label = _} :: _
            ->
            Location.raise_errorf
              ~loc "Ill defined attribute [@@bs.send] (first argument can not be const)"
          | _ :: _  ->
            Js_send {splice ; name; js_send_scopes = scopes ;  pipe = false}
        end

      | {val_send = #bundle_source; _ }
        -> Location.raise_errorf ~loc "You used an FFI attribute that can't be used with [@@bs.send]"

      | {val_send_pipe = Some typ;
         (* splice = (false as splice); *)
         val_send = `Nm_na;
         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         new_name = `Nm_na;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None ;
         mk_obj = _;
         return_wrapper = _;
         scopes;
         splice ;
        } ->
        (** can be one argument *)
        Js_send {splice  ;
                 name = string_of_bundle_source prim_name_or_pval_prim;
                 js_send_scopes = scopes;
                 pipe = true}

      | {val_send_pipe = Some _ ; _}
        -> Location.raise_errorf ~loc "conflict attributes found with [@@bs.send.pipe]"

      | {new_name = (`Nm_val name | `Nm_external name | `Nm_payload name);
         external_module_name;

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = None;
         set_name = `Nm_na ;
         get_name = `Nm_na ;
         splice ;
         scopes;
         mk_obj = _ ;
         return_wrapper = _ ;

        }
        -> Js_new {name; external_module_name; splice; scopes}
      | {new_name = #bundle_source ; _ }
        ->
        Bs_syntaxerr.err loc (Conflict_ffi_attribute "Attribute found that conflicts with [@@bs.new]")


      | {set_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = None;
         new_name = `Nm_na ;
         get_name = `Nm_na ;
         external_module_name = None;
         splice = false;
         mk_obj = _ ;
         return_wrapper = _;
         scopes ;
        }
        ->
        if arg_type_specs_length = 2 then
          Js_set { js_set_scopes = scopes ; js_set_name = name}
        else  Location.raise_errorf ~loc "Ill defined attribute [@@bs.set] (two args required)"

      | {set_name = #bundle_source; _}
        -> Location.raise_errorf ~loc "conflict attributes found with [@@bs.set]"

      | {get_name = (`Nm_val name | `Nm_external name | `Nm_payload name);

         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = None;
         new_name = `Nm_na ;
         set_name = `Nm_na ;
         external_module_name = None;
         splice = false ;
         mk_obj = _;
         return_wrapper = _;
         scopes
        }
        ->
        if arg_type_specs_length = 1 then
          Js_get { js_get_name = name; js_get_scopes = scopes }
        else
          Location.raise_errorf ~loc "Ill defined attribute [@@bs.get] (only one argument)"
      | {get_name = #bundle_source; _}
        -> Location.raise_errorf ~loc "Attribute found that conflicts with [@@bs.get]"

      | {get_name = `Nm_na;
         val_name = `Nm_na  ;
         call_name = `Nm_na ;
         module_as_val = None;
         set_index = false;
         get_index = false;
         val_send = `Nm_na ;
         val_send_pipe = None;
         new_name = `Nm_na ;
         set_name = `Nm_na ;
         external_module_name = None;
         splice = _ ;
         scopes = _;
         mk_obj = _;
         return_wrapper = _;

        }
        ->  Location.raise_errorf ~loc "Could not infer which FFI category it belongs to, maybe you forgot [%@%@bs.val]? "  in
    begin
      External_ffi_types.check_ffi ~loc ffi;
      (* result type can not be labeled *)
      (* currently we don't process attributes of
         return type, in the future we may  *)
      let  new_result_type  =  result_type in
      (* get_arg_type ~nolabel:true false result_type in *)
      let return_wrapper : External_ffi_types.return_wrapper =
        check_return_wrapper loc st.return_wrapper new_result_type
      in
      (
        Ext_list.fold_right (fun (label,ty,attrs,loc) acc ->
            Ast_helper.Typ.arrow ~loc  ~attrs label ty acc
          ) new_arg_types_ty new_result_type
      ) ,

      prim_name,
      (Ffi_bs (arg_type_specs,return_wrapper ,  ffi)), left_attrs
    end

let handle_attributes_as_string
    pval_loc
    pval_prim
    (typ : Ast_core_type.t) attrs v =
  let pval_type, prim_name, ffi, processed_attrs  =
    handle_attributes pval_loc pval_prim typ attrs v  in
  pval_type, [prim_name; External_ffi_types.to_string ffi], processed_attrs



let pval_prim_of_labels (labels : string Asttypes.loc list)
   =
  let arg_kinds =
    Ext_list.fold_right
      (fun {Asttypes.loc ; txt } arg_kinds
        ->
          let arg_label =
            External_arg_spec.label
              (Lam_methname.translate ~loc txt) None in
          {External_arg_spec.arg_type = Nothing ;
           arg_label  } :: arg_kinds
      )
      labels [] in
  let encoding =
    External_ffi_types.to_string (Ffi_obj_create arg_kinds) in
  [""; encoding]

let pval_prim_of_option_labels
(labels : (bool * string Asttypes.loc) list)
(ends_with_unit : bool)
  =
  let arg_kinds =
    Ext_list.fold_right
      (fun (is_option,{Asttypes.loc ; txt }) arg_kinds
        ->
          let label_name = (Lam_methname.translate ~loc txt) in
          let arg_label =
            if is_option then
              External_arg_spec.optional label_name
            else External_arg_spec.label label_name None
          in
          {External_arg_spec.arg_type = Nothing ;
           arg_label  } :: arg_kinds
      )
      labels
      (if ends_with_unit then
         [External_arg_spec.empty_kind Extern_unit]
       else [])
  in
  let encoding =
    External_ffi_types.to_string (Ffi_obj_create arg_kinds) in
  [""; encoding]


end
module Ast_util : sig 
#1 "ast_util.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type args = (string * Parsetree.expression) list
type loc = Location.t 
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type 'a cxt = loc -> Bs_ast_mapper.mapper -> 'a

(** In general three kinds of ast generation.
    - convert a curried to type to uncurried 
    - convert a curried fun to uncurried fun
    - convert a uncuried application to normal 
*)
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt
type uncurry_type_gen = 
  (string -> (* label for error checking *)
   Parsetree.core_type ->
   Parsetree.core_type  ->
   Parsetree.core_type) cxt

(** TODO: the interface is not reusable, it depends on too much context *)
(** syntax: {[f arg0 arg1 [@bs]]}*)
val uncurry_fn_apply : 
  (Parsetree.expression ->
  args ->
  Parsetree.expression_desc ) cxt 

(** syntax : {[f## arg0 arg1 ]}*)
val method_apply : 
  (Parsetree.expression ->
  string ->
  args ->
  Parsetree.expression_desc) cxt 

(** syntax {[f#@ arg0 arg1 ]}*)
val property_apply : 
  (Parsetree.expression ->
  string ->
  args ->
  Parsetree.expression_desc) cxt 


(** 
    [function] can only take one argument, that is the reason we did not adopt it
    syntax:
    {[ fun [@bs] pat pat1-> body ]}
    [to_uncurry_fn (fun pat -> (fun pat1 -> ...  body))]

*)
val to_uncurry_fn : uncurry_expression_gen


(** syntax: 
    {[fun [@bs.this] obj pat pat1 -> body]}    
*)
val to_method_callback : uncurry_expression_gen


(** syntax : 
    {[ int -> int -> int [@bs]]}
*)
val to_uncurry_type : uncurry_type_gen
  

(** syntax
    {[ method : int -> itn -> int ]}
*)
val to_method_type : uncurry_type_gen

(** syntax:
    {[ 'obj -> int -> int [@bs.this] ]}
*)
val to_method_callback_type : uncurry_type_gen





val record_as_js_object : 
  (label_exprs ->
   Parsetree.expression_desc) cxt 

val js_property : 
  loc ->
  Parsetree.expression -> string -> Parsetree.expression_desc

val handle_debugger : 
  loc -> Ast_payload.t -> Parsetree.expression_desc

val handle_raw : 
  check_js_regex: bool -> loc -> Ast_payload.t -> Parsetree.expression

val handle_external :
  loc -> string -> Parsetree.expression 
  
val handle_raw_structure : 
  loc -> Ast_payload.t -> Parsetree.structure_item

val ocaml_obj_as_js_object :
  (Parsetree.pattern ->
   Parsetree.class_field list ->
   Parsetree.expression_desc) cxt   


 val convertBsErrorFunction : 
   
   (Ast_helper.attrs -> Parsetree.case list -> Parsetree.expression) cxt

end = struct
#1 "ast_util.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper 
type 'a cxt = Ast_helper.loc -> Bs_ast_mapper.mapper -> 'a
type loc = Location.t 
type args = (string * Parsetree.expression) list
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt
type uncurry_type_gen = 
  (string ->
   Parsetree.core_type ->
   Parsetree.core_type  ->
   Parsetree.core_type) cxt

let uncurry_type_id = 
  Ast_literal.Lid.js_fn

let method_id  = 
  Ast_literal.Lid.js_meth

let method_call_back_id  = 
  Ast_literal.Lid.js_meth_callback

let arity_lit = "Arity_"

let mk_args loc n tys = 
  Typ.variant ~loc 
    [ Rtag (arity_lit ^ string_of_int n, [], (n = 0),  tys)] Closed None

let generic_lift txt loc args result  = 
  let xs =
    match args with 
    | [ ] -> [mk_args loc 0   [] ; result ]
    | [ x ] -> [ mk_args loc 1 [x] ; result ] 
    | _ -> 
      [mk_args loc (List.length args ) [Typ.tuple ~loc args] ; result ]
  in 
  Typ.constr ~loc {txt ; loc} xs

let lift_curry_type  loc   = 
  generic_lift   uncurry_type_id loc

let lift_method_type loc  = 
  generic_lift  method_id loc

let lift_js_method_callback loc
  = 
  generic_lift method_call_back_id loc 
(** Note that currently there is no way to consume [Js.meth_callback]
    so it is fine to encode it with a freedom, 
    but we need make it better for error message.
    - all are encoded as 
    {[ 
      type fn =  (`Args_n of _ , 'result ) Js.fn
      type method = (`Args_n of _, 'result) Js.method
      type method_callback = (`Args_n of _, 'result) Js.method_callback
    ]}
    For [method_callback], the arity is never zero, so both [method] 
    and  [fn] requires (unit -> 'a) to encode arity zero
*)



let arrow = Typ.arrow


let js_property loc obj name =
  Parsetree.Pexp_send
    ((Exp.apply ~loc
        (Exp.ident ~loc
           {loc;
            txt = Ldot (Ast_literal.Lid.js_unsafe, Literals.unsafe_downgrade)})
        ["",obj]), name)

(* TODO: 
   have a final checking for property arities 
     [#=], 
*)


let generic_apply  kind loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : args ) cb   =
  let obj = self.expr self obj in
  let args =
    Ext_list.map (fun (label,e) ->
        if label <> "" then
          Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;
        self.expr self e
      ) args in
  let len = List.length args in 
  let arity, fn, args  = 
    match args with 
    | [ {pexp_desc =
           Pexp_construct ({txt = Lident "()"}, None)}]
      -> 
      0, cb loc obj, []
    | _ -> 
      len,  cb loc obj, args in
  if arity < 10 then 
    let txt = 
      match kind with 
      | `Fn | `PropertyFn ->  
        Longident.Ldot (Ast_literal.Lid.js_unsafe, 
                        Literals.fn_run ^ string_of_int arity)
      | `Method -> 
        Longident.Ldot(Ast_literal.Lid.js_unsafe,
                       Literals.method_run ^ string_of_int arity
                      ) in 
    Parsetree.Pexp_apply (Exp.ident {txt ; loc}, ("",fn) :: Ext_list.map (fun x -> "",x) args)
  else 
    let fn_type, args_type, result_type = Ast_comb.tuple_type_pair ~loc `Run arity  in 
    let string_arity = string_of_int arity in
    let pval_prim, pval_type = 
      match kind with 
      | `Fn | `PropertyFn -> 
        ["#fn_run"; string_arity], 
        arrow ~loc ""  (lift_curry_type loc args_type result_type ) fn_type
      | `Method -> 
        ["#method_run" ; string_arity], 
        arrow ~loc "" (lift_method_type loc args_type result_type) fn_type
    in
    Ast_external_mk.local_external loc ~pval_prim ~pval_type 
      (("", fn) :: Ext_list.map (fun x -> "",x) args )


let uncurry_fn_apply loc self fn args = 
  generic_apply `Fn loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name (args : args) 
  =  generic_apply `PropertyFn loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let method_apply loc self obj name args = 
  generic_apply `Method loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let generic_to_uncurry_type  kind loc (mapper : Bs_ast_mapper.mapper) label
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  if label <> "" then
    Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;

  let rec aux acc (typ : Parsetree.core_type) = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    | `Nothing, _   -> 
      begin match typ.ptyp_desc with 
        | Ptyp_arrow (label, arg, body)
          -> 
          if label <> "" then
            Bs_syntaxerr.err typ.ptyp_loc Label_in_uncurried_bs_attribute;
          aux (mapper.typ mapper arg :: acc) body 
        | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = aux  [first_arg] typ in 
  let args  = List.rev rev_extra_args in 
  let filter_args args  =  
    match args with 
    | [{Parsetree.ptyp_desc = 
          (Ptyp_constr ({txt = Lident "unit"}, []) 
          )}]
      -> []
    | _ -> args in
  match kind with 
  | `Fn ->
    let args = filter_args args in
    lift_curry_type loc args result 
  | `Method -> 
    let args = filter_args args in
    lift_method_type loc args result 

  | `Method_callback
    -> lift_js_method_callback loc args result 


let to_uncurry_type  = 
  generic_to_uncurry_type `Fn
let to_method_type  =
  generic_to_uncurry_type  `Method
let to_method_callback_type  = 
  generic_to_uncurry_type `Method_callback 

let generic_to_uncurry_exp kind loc (self : Bs_ast_mapper.mapper)  pat body 
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | `Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (label,_, arg, body)
          -> 
          if label <> "" then
            Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;
          aux (self.pat self arg :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  
  let () = 
    match kind with 
    | `Method_callback -> 
      if not @@ Ast_pat.is_single_variable_pattern_conservative first_arg then
        Bs_syntaxerr.err first_arg.ppat_loc  Bs_this_simple_pattern
    | _ -> ()
  in 

  let result, rev_extra_args = aux [first_arg] body in 
  let body = 
    List.fold_left (fun e p -> Ast_comb.fun_no_label ~loc p e )
      result rev_extra_args in
  let len = List.length rev_extra_args in 
  let arity = 
    match kind with 
    | `Fn  ->
      begin match rev_extra_args with 
        | [ p]
          ->
          Ast_pat.is_unit_cont ~yes:0 ~no:len p           

        | _ -> len 
      end
    | `Method_callback -> len  in 
  if arity < 10  then 
    let txt = 
      match kind with 
      | `Fn -> 
        Longident.Ldot ( Ast_literal.Lid.js_unsafe, Literals.fn_mk ^ string_of_int arity)
      | `Method_callback -> 
        Longident.Ldot (Ast_literal.Lid.js_unsafe,  Literals.fn_method ^ string_of_int arity) in
    Parsetree.Pexp_apply (Exp.ident {txt;loc} , ["",body])

  else 
    let pval_prim =
      [ (match kind with 
            | `Fn -> "#fn_mk"
            | `Method_callback -> "#fn_method"); 
        string_of_int arity]  in
    let fn_type , args_type, result_type  = Ast_comb.tuple_type_pair ~loc `Make arity  in 
    let pval_type = arrow ~loc "" fn_type (
        match kind with 
        | `Fn -> 
          lift_curry_type loc args_type result_type
        | `Method_callback -> 
          lift_js_method_callback loc args_type result_type
      ) in
    Ast_external_mk.local_extern_cont loc ~pval_prim ~pval_type 
      (fun prim -> Exp.apply ~loc prim ["", body]) 

let to_uncurry_fn   = 
  generic_to_uncurry_exp `Fn
let to_method_callback  = 
  generic_to_uncurry_exp `Method_callback 


let handle_debugger loc payload = 
  if Ast_payload.as_empty_structure payload then
    Parsetree.Pexp_apply
      (Exp.ident {txt = Ldot(Ast_literal.Lid.js_unsafe, Literals.debugger ); loc}, 
       ["", Ast_literal.val_unit ~loc ()])
  else Location.raise_errorf ~loc "bs.raw can only be applied to a string"


let handle_raw ~check_js_regex loc payload =
  begin match Ast_payload.as_string_exp ~check_js_regex payload with
    | Not_String_Lteral ->
      Location.raise_errorf ~loc
        "bs.raw can only be applied to a string"
    | Ast_payload.JS_Regex_Check_Failed ->
      Location.raise_errorf ~loc "this is an invalid js regex"
    | Correct exp ->
      let pexp_desc = 
        Parsetree.Pexp_apply (
          Exp.ident {loc; 
                     txt = 
                       Ldot (Ast_literal.Lid.js_unsafe, 
                             Literals.raw_expr)},
          ["",exp]
        )
      in
      { exp with pexp_desc }
  end

let handle_external loc x = 
  let raw_exp : Ast_exp.t = 
    Ast_helper.Exp.apply 
    (Exp.ident ~loc 
         {loc; txt = Ldot (Ast_literal.Lid.js_unsafe, 
                           Literals.raw_expr)})
      ~loc 
      [Ext_string.empty, 
        Exp.constant ~loc (Const_string (x,Some Ext_string.empty))] in 
  let empty = 
    Exp.ident ~loc 
    {txt = Ldot (Ldot(Lident"Js", "Undefined"), "empty");loc}    
  in 
  let undefined_typeof = 
    Exp.ident {loc ; txt = Ldot(Lident "Js","undefinedToOption")} in 
  let typeof = 
    Exp.ident {loc ; txt = Ldot(Lident "Js","typeof")} in 

  Exp.apply ~loc undefined_typeof [
    Ext_string.empty,
    Exp.ifthenelse ~loc
    (Exp.apply ~loc 
      (Exp.ident ~loc {loc ; txt = Ldot (Lident "Pervasives", "=")} )
      [ 
        Ext_string.empty,
        (Exp.apply ~loc typeof [Ext_string.empty,raw_exp]);
        Ext_string.empty, 
        Exp.constant ~loc (Const_string ("undefined",None))  
        ])      
      (empty)
      (Some raw_exp)
      ]


let handle_raw_structure loc payload = 
  begin match Ast_payload.as_string_exp ~check_js_regex:false payload with 
    | Correct exp 
      -> 
      let pexp_desc = 
        Parsetree.Pexp_apply(
          Exp.ident {txt = Ldot (Ast_literal.Lid.js_unsafe,  Literals.raw_stmt); loc},
          ["",exp]) in 
      Ast_helper.Str.eval 
        { exp with pexp_desc }

    | Not_String_Lteral
      -> 
      Location.raise_errorf ~loc "bs.raw can only be applied to a string"
    | JS_Regex_Check_Failed 
      ->
      Location.raise_errorf ~loc "this is an invalid js regex"
  end


let ocaml_obj_as_js_object
    loc (mapper : Bs_ast_mapper.mapper)
    (self_pat : Parsetree.pattern)
    (clfs : Parsetree.class_field list) =
  let self_type_lit = "self_type"   in 

  (** Attention: we should avoid type variable conflict for each method  
      Since the method name is unique, there would be no conflict 
      OCaml does not allow duplicate instance variable and duplicate methods, 
      but it does allow duplicates between instance variable and method name, 
      we should enforce such rules 
      {[
        object 
          val x = 3
          method x = 3 
        end [@bs]
      ]} should not compile with a meaningful error message
  *)

  let generate_val_method_pair 
      loc (mapper : Bs_ast_mapper.mapper)
      val_name  is_mutable = 

    let result = Typ.var ~loc val_name in 
    result , 
    ((val_name , [], result ) ::
     (if is_mutable then 
        [val_name ^ Literals.setter_suffix,[],
         to_method_type loc mapper "" result (Ast_literal.type_unit ~loc ()) ]
      else 
        []) )
  in 
  (* Note mapper is only for API compatible 
   * TODO: we should check label name to avoid conflict 
  *)  
  let self_type loc = Typ.var ~loc self_type_lit in 

  let generate_arg_type loc (mapper  : Bs_ast_mapper.mapper)
      method_name arity : Ast_core_type.t = 
    let result = Typ.var ~loc method_name in   
    if arity = 0 then
      to_method_type loc mapper "" (Ast_literal.type_unit ~loc ()) result 

    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right (fun v acc -> Typ.arrow ~loc "" v acc)
              rest result in         
          to_method_type loc mapper "" x method_rest
        | _ -> assert false
      end in          

  let generate_method_type
      loc
      (mapper : Bs_ast_mapper.mapper)
      ?alias_type method_name arity =
    let result = Typ.var ~loc method_name in   

    let self_type =
      let v = self_type loc  in
      match alias_type with 
      | None -> v 
      | Some ty -> Typ.alias ~loc ty self_type_lit
    in  
    if arity = 0 then
      to_method_callback_type loc mapper  "" self_type result      
    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right (fun v acc -> Typ.arrow ~loc "" v acc)
              rest result in         
          (to_method_callback_type loc mapper  "" self_type
             (Typ.arrow ~loc "" x method_rest))
        | _ -> assert false
      end in          


  (** we need calculate the real object type 
      and exposed object type, in some cases there are equivalent

      for public object type its [@bs.meth] it does not depend on itself
      while for label argument it is [@bs.this] which depends internal object
  *)
  let internal_label_attr_types, public_label_attr_types  = 
    Ext_list.fold_right
      (fun ({pcf_loc  = loc} as x  : Parsetree.class_field) 
        (label_attr_types, public_label_attr_types) ->
        match x.pcf_desc with
        | Pcf_method (
            label,
            public_flag,
            Cfk_concrete
              (Fresh, e))
          ->
          begin match e.pexp_desc with
            | Pexp_poly
                (({pexp_desc = Pexp_fun ("", None, pat, e)} ),
                 None) ->  
              let arity = Ast_pat.arity_of_fun pat e in
              let method_type =
                generate_arg_type x.pcf_loc mapper label.txt arity in 
              ((label.Asttypes.txt, [], method_type) :: label_attr_types),
              (if public_flag = Public then
                 (label.Asttypes.txt, [], method_type) :: public_label_attr_types
               else 
                 public_label_attr_types)

            | Pexp_poly( _, Some _)
              ->
              Location.raise_errorf ~loc "polymorphic type annotation not supported yet"
            | Pexp_poly (_, None) ->
              Location.raise_errorf ~loc
                "Unsupported syntax, expect syntax like `method x () = x ` "
            | _ ->
              Location.raise_errorf ~loc "Unsupported syntax in js object"               
          end
        | Pcf_val (label, mutable_flag, Cfk_concrete(Fresh, val_exp)) ->
          let  label_type, label_attr  = 
            generate_val_method_pair x.pcf_loc mapper label.txt  
              (mutable_flag = Mutable )
          in
          (Ext_list.append label_attr  label_attr_types, public_label_attr_types)
        | Pcf_val (label, mutable_flag, Cfk_concrete(Override, val_exp)) -> 
          Location.raise_errorf ~loc "override flag not support currently"
        | Pcf_val (label, mutable_flag, Cfk_virtual _) -> 
          Location.raise_errorf ~loc "virtual flag not support currently"

        | Pcf_method (_, _, Cfk_concrete(Override, _) ) -> 
          Location.raise_errorf ~loc "override flag not supported"

        | Pcf_method (_, _, Cfk_virtual _ )
          ->
          Location.raise_errorf ~loc "virtural method not supported"

        | Pcf_inherit _ 
        | Pcf_initializer _
        | Pcf_attribute _
        | Pcf_extension _
        | Pcf_constraint _ ->
          Location.raise_errorf ~loc "Only method support currently"
      ) clfs ([], []) in
  let internal_obj_type = Ast_core_type.make_obj ~loc internal_label_attr_types in
  let public_obj_type = Ast_core_type.make_obj ~loc public_label_attr_types in
  let (labels,  label_types, exprs, _) =
    Ext_list.fold_right
      (fun (x  : Parsetree.class_field)
        (labels,
         label_types,
         exprs, aliased ) ->
        match x.pcf_desc with
        | Pcf_method (
            label,
            _public_flag,
            Cfk_concrete
              (Fresh, e))
          ->
          begin match e.pexp_desc with
            | Pexp_poly
                (({pexp_desc = Pexp_fun ("", None, pat, e)} as f),
                 None) ->  
              let arity = Ast_pat.arity_of_fun pat e in
              let alias_type = 
                if aliased then None 
                else Some internal_obj_type in
              let  label_type =
                generate_method_type ?alias_type
                  x.pcf_loc mapper label.txt arity in 
              (label::labels,
               label_type::label_types,
               {f with
                pexp_desc =
                  let f = Ast_pat.is_unit_cont pat ~yes:e ~no:f in                       
                  to_method_callback loc mapper self_pat f
               } :: exprs, 
               true
              )
            | Pexp_poly( _, Some _)
              ->
              Location.raise_errorf ~loc
                "polymorphic type annotation not supported yet"

            | Pexp_poly (_, None) ->
              Location.raise_errorf
                ~loc "Unsupported syntax, expect syntax like `method x () = x ` "
            | _ ->
              Location.raise_errorf ~loc "Unsupported syntax in js object"               
          end
        | Pcf_val (label, mutable_flag, Cfk_concrete(Fresh, val_exp)) ->
          let  label_type, label_attr  = 
            generate_val_method_pair x.pcf_loc mapper label.txt  
              (mutable_flag = Mutable )
          in
          (label::labels,
           label_type :: label_types, 
           (mapper.expr mapper val_exp :: exprs), 
           aliased 
          )

        | Pcf_val (label, mutable_flag, Cfk_concrete(Override, val_exp)) -> 
          Location.raise_errorf ~loc "override flag not support currently"
        | Pcf_val (label, mutable_flag, Cfk_virtual _) -> 
          Location.raise_errorf ~loc "virtual flag not support currently"

        | Pcf_method (_, _, Cfk_concrete(Override, _) ) -> 
          Location.raise_errorf ~loc "override flag not supported"

        | Pcf_method (_, _, Cfk_virtual _ )
          ->
          Location.raise_errorf ~loc "virtural method not supported"


        | Pcf_inherit _ 
        | Pcf_initializer _
        | Pcf_attribute _
        | Pcf_extension _
        | Pcf_constraint _ ->
          Location.raise_errorf ~loc "Only method support currently"
      ) clfs  ([], [], [], false) in
  let pval_type =
    Ext_list.fold_right2
      (fun label label_type acc ->
         Typ.arrow
           ~loc:label.Asttypes.loc
           label.Asttypes.txt
           label_type acc           
      ) labels label_types public_obj_type in
  Ast_external_mk.local_extern_cont
    loc
    ~pval_prim:(External_process.pval_prim_of_labels labels)
    (fun e ->
       Exp.apply ~loc e
         (Ext_list.map2 (fun l expr -> l.Asttypes.txt, expr) labels exprs) )
    ~pval_type


let record_as_js_object 
    loc 
    (self : Bs_ast_mapper.mapper)
    (label_exprs : label_exprs)
  : Parsetree.expression_desc = 

  let labels,args, arity =
    Ext_list.fold_right (fun ({Location.txt ; loc}, e) (labels,args,i) -> 
        match txt with
        | Longident.Lident x ->
          ({Asttypes.loc = loc ; txt = x} :: labels, (x, self.expr self e) :: args, i + 1)
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label ") label_exprs ([],[],0) in
  Ast_external_mk.local_external loc 
    ~pval_prim:(External_process.pval_prim_of_labels labels)
    ~pval_type:(Ast_core_type.from_labels ~loc arity labels) 
    args 



let isCamlExceptionOrOpenVariant = Longident.parse "Caml_exceptions.isCamlExceptionOrOpenVariant"
let obj_magic = Longident.parse "Obj.magic"

let rec checkCases (cases : Parsetree.case list) = 
  List.iter check_case cases 
and check_case case = 
  check_pat case.pc_lhs 
and check_pat (pat : Parsetree.pattern) = 
  match pat.ppat_desc with 
  | Ppat_construct _ -> ()
  | Ppat_or (l,r) -> 
    check_pat l; check_pat r 
  | _ ->  Location.raise_errorf ~loc:pat.ppat_loc "Unsupported pattern in `bs.open`" 

let convertBsErrorFunction loc  (self : Bs_ast_mapper.mapper) attrs (cases : Parsetree.case list ) =
  let txt  = "match" in 
  let txt_expr = Exp.ident ~loc {txt = Lident txt; loc} in 
  let none = Exp.constraint_ ~loc 
      (Exp.construct ~loc {txt = Lident "None" ; loc} None) 
      (Ast_core_type.lift_option_type (Typ.any ~loc ())) in
  let () = checkCases cases in  
  let cases = self.cases self cases in 
  Exp.fun_ ~attrs ~loc ""  None ( Pat.var ~loc  {txt; loc })
    (Exp.ifthenelse
    ~loc 
    (Exp.apply ~loc (Exp.ident ~loc {txt = isCamlExceptionOrOpenVariant ; loc}) ["", txt_expr ])
    (Exp.match_ ~loc 
       (Exp.constraint_ ~loc 
          (Exp.apply  ~loc (Exp.ident ~loc {txt =  obj_magic; loc}) ["", txt_expr])
          (Ast_literal.type_exn ~loc ())
       )
      (Ext_list.map_append (fun (x :Parsetree.case ) ->
           let pc_rhs = x.pc_rhs in 
           let  loc  = pc_rhs.pexp_loc in
           {
             x with pc_rhs = 
                      Exp.constraint_ ~loc 
                        (Exp.construct ~loc {txt = Lident "Some";loc} (Some pc_rhs))
                        (Ast_core_type.lift_option_type (Typ.any ~loc ())  )
           }

         ) cases 
      [
       Exp.case  (Pat.any ~loc ()) none
     ])
    )
    (Some none))
    
                       

end
module Ext_ref : sig 
#1 "ext_ref.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** [non_exn_protect ref value f] assusme [f()] 
    would not raise
*)

val non_exn_protect : 'a ref -> 'a -> (unit -> 'b) -> 'b
val protect : 'a ref -> 'a -> (unit -> 'b) -> 'b

val protect2 : 'a ref -> 'b ref -> 'a -> 'b -> (unit -> 'c) -> 'c

(** [non_exn_protect2 refa refb va vb f ]
    assume [f ()] would not raise
*)
val non_exn_protect2 : 'a ref -> 'b ref -> 'a -> 'b -> (unit -> 'c) -> 'c

val protect_list : ('a ref * 'a) list -> (unit -> 'b) -> 'b

end = struct
#1 "ext_ref.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let non_exn_protect r v body = 
  let old = !r in
  r := v;
  let res = body() in
  r := old;
  res

let protect r v body =
  let old = !r in
  try
    r := v;
    let res = body() in
    r := old;
    res
  with x ->
    r := old;
    raise x

let non_exn_protect2 r1 r2 v1 v2 body = 
  let old1 = !r1 in
  let old2 = !r2 in  
  r1 := v1;
  r2 := v2;
  let res = body() in
  r1 := old1;
  r2 := old2;
  res

let protect2 r1 r2 v1 v2 body =
  let old1 = !r1 in
  let old2 = !r2 in  
  try
    r1 := v1;
    r2 := v2;
    let res = body() in
    r1 := old1;
    r2 := old2;
    res
  with x ->
    r1 := old1;
    r2 := old2;
    raise x

let protect_list rvs body = 
  let olds =  Ext_list.map (fun (x,y) -> !x)  rvs in 
  let () = List.iter (fun (x,y) -> x:=y) rvs in 
  try 
    let res = body () in 
    List.iter2 (fun (x,_) old -> x := old) rvs olds;
    res 
  with e -> 
    List.iter2 (fun (x,_) old -> x := old) rvs olds;
    raise e 

end
module Ast_core_type_class_type : sig 
#1 "ast_core_type_class_type.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



val handle_class_type_fields :  
  Bs_ast_mapper.mapper ->
  Parsetree.class_type_field list -> 
  Parsetree.class_type_field list 

val handle_core_type :
  Bs_ast_mapper.mapper ->
  Parsetree.core_type ->  
  bool ref ->
  Parsetree.core_type
end = struct
#1 "ast_core_type_class_type.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
open Ast_helper
let process_getter_setter ~no ~get ~set
    loc name
    (attrs : Ast_attributes.t)
    (ty : Parsetree.core_type) acc  =
  match Ast_attributes.process_method_attributes_rev attrs with 
  | {get = None; set = None}, _  ->  no ty :: acc 
  | st , pctf_attributes
    -> 
    let get_acc = 
      match st.set with 
      | Some `No_get -> acc 
      | None 
      | Some `Get -> 
        let lift txt = 
          Typ.constr ~loc {txt ; loc} [ty] in
        let (null,undefined) =                
          match st with 
          | {get = Some (null, undefined) } -> (null, undefined)
          | {get = None} -> (false, false ) in 
        let ty = 
          match (null,undefined) with 
          | false, false -> ty
          | true, false -> lift Ast_literal.Lid.js_null
          | false, true -> lift Ast_literal.Lid.js_undefined
          | true , true -> lift Ast_literal.Lid.js_null_undefined in
        get ty name pctf_attributes
        :: acc  
    in 
    if st.set = None then get_acc 
    else
      set ty (name ^ Literals.setter_suffix) pctf_attributes         
      :: get_acc 


let handle_class_type_field self
    ({pctf_loc = loc } as ctf : Parsetree.class_type_field)
    acc =
  match ctf.pctf_desc with 
  | Pctf_method 
      (name, private_flag, virtual_flag, ty) 
    ->
    let no (ty : Parsetree.core_type) =
      let ty = 
        match ty.ptyp_desc with 
        | Ptyp_arrow (label, args, body) 
          ->
          Ast_util.to_method_type
            ty.ptyp_loc  self label args body

        | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow (label, args, body);
                            ptyp_loc})
          ->
          {ty with ptyp_desc = 
                     Ptyp_poly(strs,             
                               Ast_util.to_method_type
                                 ptyp_loc  self label args body  )}
        | _ -> 
          self.typ self ty
      in 
      {ctf with 
       pctf_desc = 
         Pctf_method (name , private_flag, virtual_flag, ty)}
    in
    let get ty name pctf_attributes =
      {ctf with 
       pctf_desc =  
         Pctf_method (name , 
                      private_flag, 
                      virtual_flag, 
                      self.typ self ty
                     );
       pctf_attributes} in
    let set ty name pctf_attributes =
      {ctf with 
       pctf_desc =
         Pctf_method (name, 
                      private_flag,
                      virtual_flag,
                      Ast_util.to_method_type
                        loc self "" ty
                        (Ast_literal.type_unit ~loc ())
                     );
       pctf_attributes} in
    process_getter_setter ~no ~get ~set loc name ctf.pctf_attributes ty acc     

  | Pctf_inherit _ 
  | Pctf_val _ 
  | Pctf_constraint _
  | Pctf_attribute _ 
  | Pctf_extension _  -> 
    Bs_ast_mapper.default_mapper.class_type_field self ctf :: acc 
      

(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let handle_core_type 
    ~(super : Bs_ast_mapper.mapper) 
    ~(self : Bs_ast_mapper.mapper)
    (ty : Parsetree.core_type) 
    record_as_js_object
  = 
  match ty with
  | {ptyp_desc = Ptyp_extension({txt = ("bs.obj"|"obj")}, PTyp ty)}
    -> 
    Ext_ref.non_exn_protect record_as_js_object true 
      (fun _ -> self.typ self ty )
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     (* let it go without regard label names, 
        it will report error later when the label is not empty
     *)     
     ptyp_loc = loc
    } ->
    begin match  Ast_attributes.process_attributes_rev ptyp_attributes with 
      | `Uncurry , ptyp_attributes ->
        Ast_util.to_uncurry_type loc self label args body 
      |  `Meth_callback, ptyp_attributes ->
        Ast_util.to_method_callback_type loc self label args body
      | `Method, ptyp_attributes ->
        Ast_util.to_method_type loc self label args body
      | `Nothing , _ -> 
        Bs_ast_mapper.default_mapper.typ self ty
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_loc = loc 
  } -> 
    let (+>) attr (typ : Parsetree.core_type) =
      {typ with ptyp_attributes = attr :: typ.ptyp_attributes} in           
    let new_methods =
      Ext_list.fold_right (fun (label, ptyp_attrs, core_type) acc ->
          let get ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, ty (* #1678 *)
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty
              | `Method, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty 
            in 
            name , attrs, self.typ self core_type in
          let set ty name attrs =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev attrs with
              | `Nothing, attrs -> attrs, ty
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty 
              | `Method, _
                -> Location.raise_errorf ~loc "bs.get/set conflicts with bs.meth"
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty
            in               
            name, attrs, Ast_util.to_method_type loc self "" core_type 
              (Ast_literal.type_unit ~loc ()) in
          let no ty =
            let attrs, core_type =
              match Ast_attributes.process_attributes_rev ptyp_attrs with
              | `Nothing, attrs -> attrs, ty
              | `Uncurry, attrs ->
                attrs, Ast_attributes.bs +> ty 
              | `Method, attrs -> 
                attrs, Ast_attributes.bs_method +> ty 
              | `Meth_callback, attrs ->
                attrs, Ast_attributes.bs_this +> ty  in            
            label, attrs, self.typ self core_type in
          process_getter_setter ~no ~get ~set
            loc label ptyp_attrs core_type acc
        ) methods [] in      
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(new_methods, closed_flag);
      } in 
    if !record_as_js_object then 
      Ast_comb.to_js_type loc inner_type          
    else inner_type
  | _ -> super.typ self ty
    
let handle_class_type_fields self fields = 
  Ext_list.fold_right 
  (handle_class_type_field self)
  fields []
  
let handle_core_type self typ record_as_js_object =
  handle_core_type 
  ~super:Bs_ast_mapper.default_mapper
  ~self typ record_as_js_object
end
module Ast_signature : sig 
#1 "ast_signature.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type item = Parsetree.signature_item
type t = item list 


val fuseAll : ?loc:Ast_helper.loc ->  t -> item
end = struct
#1 "ast_signature.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type item = Parsetree.signature_item
type t = item list 

open Ast_helper

let fuseAll ?(loc=Location.none)  (t : t) : item = 
  Sig.include_ ~loc (Incl.mk ~loc (Mty.signature ~loc t))
  
end
module Ast_structure : sig 
#1 "ast_structure.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type item = Parsetree.structure_item

type t = item list 


val fuseAll: ?loc:Ast_helper.loc ->  t -> item

(* val fuse_with_constraint:
  ?loc:Ast_helper.loc ->
  Parsetree.type_declaration list ->
  t   ->
  Ast_signature.t -> 
  item *)

val constraint_ : ?loc:Ast_helper.loc -> t -> Ast_signature.t -> item

val dummy_item : Location.t -> item 
end = struct
#1 "ast_structure.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type item = Parsetree.structure_item

type t = item list 

open Ast_helper


let fuseAll ?(loc=Location.none)  (t : t) : item = 
  Str.include_ ~loc 
    (Incl.mk ~loc (Mod.structure ~loc t ))
    
(* let fuse_with_constraint
    ?(loc=Location.none) 
    (item : Parsetree.type_declaration list ) (t : t) (coercion) = 
  Str.include_ ~loc 
    (Incl.mk ~loc 
       (Mod.constraint_
         (Mod.structure ~loc 
         ({pstr_loc = loc; pstr_desc = Pstr_type item} :: t) )
         (
           Mty.signature ~loc 
           ({psig_loc = loc; psig_desc = Psig_type item} :: coercion)
         )
         )
    )      *)
let constraint_ ?(loc=Location.none) (stru : t) (sign : Ast_signature.t) = 
  Str.include_ ~loc
    (Incl.mk ~loc 
       (Mod.constraint_ ~loc (Mod.structure ~loc stru) (Mty.signature ~loc sign)))

let dummy_item  loc : item =        
  Str.eval ~loc (Ast_literal.val_unit ~loc ())
end
module Ast_derive : sig 
#1 "ast_derive.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type tdcls = Parsetree.type_declaration list 

type gen = {
  structure_gen : tdcls -> bool -> Ast_structure.t ;
  signature_gen : tdcls -> bool -> Ast_signature.t ; 
  expression_gen : (Parsetree.core_type -> Parsetree.expression) option ; 
}

(**
   [register name cb]
   example: [register "accessors" cb]
*)
val register : 
  string -> 
  (Parsetree.expression option -> gen) -> 
  unit

(* val gen_structure: 
  tdcls  ->
  Ast_payload.action list ->
  bool -> 
  Ast_structure.t *)

val gen_signature: 
  tdcls ->
  Ast_payload.action list -> 
  bool -> 
  Ast_signature.t


val gen_expression : 
  string Asttypes.loc -> 
  Parsetree.core_type -> 
  Parsetree.expression



val gen_structure_signature :  
  Location.t -> 
  Parsetree.type_declaration list ->
  Ast_payload.action -> 
  bool -> 
  Parsetree.structure_item
end = struct
#1 "ast_derive.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type tdcls = Parsetree.type_declaration list

type gen = {
  structure_gen : tdcls -> bool -> Ast_structure.t ;
  signature_gen : tdcls -> bool -> Ast_signature.t ; 
  expression_gen : (Parsetree.core_type -> Parsetree.expression) option ; 
}

(* the first argument is [config] payload
   {[
     { x = {uu} }
   ]}
*)
type derive_table  = 
  (Parsetree.expression option -> gen) String_map.t

let derive_table : derive_table ref = ref String_map.empty

let register key value = 
  derive_table := String_map.add key value !derive_table 



(* let gen_structure 
    (tdcls : tdcls)
    (actions :  Ast_payload.action list ) 
    (explict_nonrec : bool )
  : Ast_structure.t = 
  Ext_list.flat_map
    (fun action -> 
       (Ast_payload.table_dispatch !derive_table action).structure_gen 
         tdcls explict_nonrec) actions *)

let gen_signature
    tdcls
    (actions :  Ast_payload.action list ) 
    (explict_nonrec : bool )
  : Ast_signature.t = 
  Ext_list.flat_map
    (fun action -> 
       (Ast_payload.table_dispatch !derive_table action).signature_gen
         tdcls explict_nonrec) actions

(** used for cases like [%sexp] *)         
let gen_expression ({Asttypes.txt ; loc}) typ =
  let txt = Ext_string.tail_from txt (String.length Literals.bs_deriving_dot) in 
  match (Ast_payload.table_dispatch !derive_table 
           ({txt ; loc}, None)).expression_gen with 
  | None ->
    Bs_syntaxerr.err loc (Unregistered txt)

  | Some f -> f typ

open Ast_helper  
let gen_structure_signature 
    loc
    (tdcls : tdcls)   
    (action : Ast_payload.action)
    (explicit_nonrec : bool) = 
  let derive_table = !derive_table in  
  let u = 
    Ast_payload.table_dispatch derive_table action in  

  let a = u.structure_gen tdcls explicit_nonrec in
  let b = u.signature_gen tdcls explicit_nonrec in
  Str.include_ ~loc  
    (Incl.mk ~loc 
       (Mod.constraint_ ~loc
          (Mod.structure ~loc a)
          (Mty.signature ~loc b )
       )
    )
end
module Ast_derive_util : sig 
#1 "ast_derive_util.mli"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** Given a type declaration, extaract the type expression, mostly 
  used in code gen later
 *)
 val core_type_of_type_declaration :
  Parsetree.type_declaration -> Parsetree.core_type

val new_type_of_type_declaration : 
  Parsetree.type_declaration -> 
  string -> 
  Parsetree.core_type * Parsetree.type_declaration

val lift_string_list_to_array : string list -> Parsetree.expression
val lift_int : int -> Parsetree.expression
val lift_int_list_to_array : int list -> Parsetree.expression
val mk_fun :
  loc:Location.t ->
  Parsetree.core_type ->
  string -> Parsetree.expression -> Parsetree.expression
val destruct_label_declarations :
  loc:Location.t ->
  string ->
  Parsetree.label_declaration list ->
  (Parsetree.core_type * Parsetree.expression) list * string list

val notApplicable:   
  Location.t ->
  string -> 
  unit 

val invalid_config : Parsetree.expression -> 'a   
end = struct
#1 "ast_derive_util.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper

let core_type_of_type_declaration 
    (tdcl : Parsetree.type_declaration) = 
  match tdcl with 
  | {ptype_name = {txt ; loc};
     ptype_params ;
    } -> 
    Typ.constr 
      {txt = Lident txt ; loc}
      (Ext_list.map fst ptype_params)

let new_type_of_type_declaration 
    (tdcl : Parsetree.type_declaration) newName = 
  match tdcl with 
  | {ptype_name = { loc};
     ptype_params ;
    } -> 
    (Typ.constr 
      {txt = Lident newName ; loc}
      (Ext_list.map fst ptype_params),
      { Parsetree.ptype_params = tdcl.ptype_params;
        ptype_name = {txt = newName;loc};
        ptype_kind = Ptype_abstract; 
        ptype_attributes = [];
        ptype_loc = tdcl.ptype_loc;
        ptype_cstrs = []; ptype_private = Public; ptype_manifest = None}
    )

      
let lift_string_list_to_array (labels : string list) = 
  Exp.array
    (Ext_list.map (fun s -> Exp.constant (Const_string (s, None)))
       labels)

let lift_int i = Exp.constant (Const_int i)
let lift_int_list_to_array (labels : int list) = 
  Exp.array (Ext_list.map lift_int labels)


let mk_fun ~loc (typ : Parsetree.core_type) 
    (value : string) body
  : Parsetree.expression = 
  Exp.fun_ 
    "" None
    (Pat.constraint_ (Pat.var {txt = value ; loc}) typ)
    body

let destruct_label_declarations ~loc
    (arg_name : string)
    (labels : Parsetree.label_declaration list) : 
  (Parsetree.core_type * Parsetree.expression) list * string list 
  =
  Ext_list.fold_right
    (fun   ({pld_name = {txt}; pld_type} : Parsetree.label_declaration) 
      (core_type_exps, labels) -> 
      ((pld_type, 
        Exp.field (Exp.ident {txt = Lident arg_name ; loc}) 
          {txt = Lident txt ; loc}) :: core_type_exps),
      txt :: labels 
    ) labels ([], [])

let notApplicable 
  loc derivingName = 
  Location.prerr_warning 
    loc
    (Warnings.Bs_derive_warning ( derivingName ^ " not applicable to this type"))
    
let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"
    
end
module Ast_derive_js_mapper : sig 
#1 "ast_derive_js_mapper.mli"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



val init : unit -> unit
end = struct
#1 "ast_derive_js_mapper.ml"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper
module U = Ast_derive_util
type tdcls = Parsetree.type_declaration list 

let js_field (o : Parsetree.expression) m = 
  Exp.apply 
    (Exp.ident {txt = Lident "##"; loc = o.pexp_loc})
    [ 
      "",o; 
      "", Exp.ident m
    ]
let const_int i = Exp.constant (Const_int i)
let const_string s = Exp.constant (Const_string (s,None))


let handle_config (config : Parsetree.expression option) = 
  match config with 
  | Some config -> 
    (match config.pexp_desc with 
     | Pexp_record (
         [ 
           {txt = Lident "newType"}, 
           {pexp_desc = 
              (Pexp_construct 
                 (
                   {txt =
                      Lident ("true" 
                             | "false" 
                               as x)}, None)
              | Pexp_ident {txt = Lident ("newType" as x)}
              )
           }
         ],None)
       ->  not (x = "false")
     | Pexp_ident {txt = Lident ("newType")} 
       -> true
     | _ -> U.invalid_config config)
  | None -> false
let noloc = Location.none
(* [eraseType] will be instrumented, be careful about the name conflict*)  
let eraseTypeLit = "jsMapperEraseType"
let eraseTypeExp = Exp.ident {loc = noloc; txt = Lident eraseTypeLit}
let eraseType x = 
  Exp.apply eraseTypeExp ["", x]
let eraseTypeStr = 
  let any = Typ.any () in 
  Str.primitive 
    (Val.mk ~prim:["%identity"] {loc = noloc; txt = eraseTypeLit}
       (Typ.arrow "" any any)
    )

let app2 f arg1 arg2 = 
  Exp.apply f ["",arg1; "", arg2]
let app3 f arg1 arg2 arg3 = 
  Exp.apply f ["", arg1; "", arg2; "", arg3]
let (<=~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Lident "<="}) a b 
let (-~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","-")})
    a b 
let (+~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","+")})
    a b 
let (&&~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","&&")})
    a b 
let (->~) a b = Typ.arrow "" a b 
let jsMapperRt =     
  Longident.Ldot (Lident "Js", "MapperRt")

let search upper polyvar array = 
  app3
    (Exp.ident ({loc = noloc; 
                 txt = Longident.Ldot (jsMapperRt,"binarySearch") })
    )                                 
    upper
    (eraseType polyvar)
    array

let revSearch len constantArray exp =   
  app3 
    (Exp.ident 
       {loc= noloc; 
        txt = Longident.Ldot (jsMapperRt, "revSearch")})
    len
    constantArray
    exp

let revSearchAssert  len constantArray exp =   
  app3 
    (Exp.ident 
       {loc= noloc; 
        txt = Longident.Ldot (jsMapperRt, "revSearchAssert")})
    len
    constantArray
    exp

let toInt exp array =     
  app2
    (Exp.ident 
       { loc=noloc; 
         txt = Longident.Ldot (jsMapperRt, "toInt")})
    (eraseType exp)
    array
let fromInt len array exp = 
  app3
    (Exp.ident 
       {loc = noloc; 
        txt = Longident.Ldot (jsMapperRt,"fromInt")})
    len
    array
    exp

let fromIntAssert len array exp = 
  app3
    (Exp.ident 
       {loc = noloc; 
        txt = Longident.Ldot (jsMapperRt,"fromIntAssert")})
    len
    array
    exp


let assertExp e = 
  Exp.extension 
    ({Asttypes.loc = noloc; txt = "assert"},
     (PStr 
        [Str.eval e ]
     )
    )
let derivingName = "jsConverter"

(* let notApplicable loc = 
  Location.prerr_warning 
    loc
    (Warnings.Bs_derive_warning ( derivingName ^ " not applicable to this type")) *)

let init () =      
  Ast_derive.register
    derivingName
    (fun ( x : Parsetree.expression option) -> 
       let createType = handle_config x in 

       {
         structure_gen = (fun (tdcls : tdcls) _ -> 
             let handle_tdcl (tdcl: Parsetree.type_declaration) =
               let core_type = U.core_type_of_type_declaration tdcl
               in 
               let name = tdcl.ptype_name.txt in 
               let toJs = name ^ "ToJs" in 
               let fromJs = name ^ "FromJs" in 
               let constantArray = "jsMapperConstantArray" in 
               let loc = tdcl.ptype_loc in 
               let patToJs = {Asttypes.loc; txt = toJs} in 
               let patFromJs = {Asttypes.loc; txt = fromJs} in 
               let param = "param" in 

               let ident_param = {Asttypes.txt = Longident.Lident param; loc} in 
               let pat_param = {Asttypes.loc; txt = param} in 
               let exp_param = Exp.ident ident_param in 
               let newType,newTdcl =
                 U.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
               let newTypeStr = Str.type_ [newTdcl] in   
               let toJsBody body = 
                 Ast_comb.single_non_rec_value patToJs
                   (Exp.fun_ "" None (Pat.constraint_ (Pat.var pat_param) core_type) 
                      body )
               in 
               let (+>) a ty = 
                 Exp.constraint_ (eraseType a) ty in 
               let (+:) a ty =                  
                 eraseType (Exp.constraint_ a ty) in 
               let coerceResultToNewType e =
                 if createType then 
                   e +> newType
                 else e    
               in                  
               match tdcl.ptype_kind with  
               | Ptype_record label_declarations -> 
                 let exp = 
                   coerceResultToNewType
                     (Exp.extension 
                        (
                          {Asttypes.loc; txt = "bs.obj"},
                          (PStr
                             [Str.eval  
                                (Exp.record
                                   (List.map 
                                      (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                                         let label = 
                                           {Asttypes.loc; txt = Longident.Lident txt } in 
                                         label,Exp.field exp_param label
                                      ) label_declarations) None)]))) in 
                 let toJs = 
                   toJsBody exp
                 in 
                 let obj_exp = 
                   Exp.record
                     (List.map 
                        (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                           let label = 
                             {Asttypes.loc; txt = Longident.Lident txt } in 
                           label,
                           js_field exp_param  label
                        ) label_declarations) None in 
                 let fromJs = 
                   Ast_comb.single_non_rec_value patFromJs
                     (Exp.fun_ "" None (Pat.var pat_param)
                        (if createType then                                             
                           (Exp.let_ Nonrecursive
                              [Vb.mk 
                                 (Pat.var pat_param) 
                                 (exp_param +: newType)]
                              (Exp.constraint_ obj_exp core_type) )
                         else 
                           (Exp.constraint_ obj_exp core_type) ))
                 in
                 let rest = 
                   [
                     toJs;
                     fromJs
                   ] in 
                 if createType then eraseTypeStr:: newTypeStr :: rest else rest 
               | Ptype_abstract -> 
                 (match Ast_polyvar.is_enum_polyvar tdcl with 
                  | Some row_fields -> 
                    let attr = 
                      Ast_polyvar.map_row_fields_into_strings loc row_fields 
                    in 
                    let expConstantArray =   
                      Exp.ident {loc; txt = Longident.Lident constantArray} in 
                    begin match attr with 
                      | NullString result -> 
                        let result_len = List.length result in 
                        let exp_len = const_int result_len in 
                        let v = [
                          eraseTypeStr;
                          Ast_comb.single_non_rec_value 
                            {loc; txt = constantArray}
                            (Exp.array
                               (List.map (fun (i,str) -> 
                                    Exp.tuple 
                                      [
                                        const_int i;
                                        const_string str
                                      ]
                                  ) (List.sort (fun (a,_) (b,_) -> compare (a:int) b) result)));
                          (
                            toJsBody
                              (coerceResultToNewType 
                                 (search
                                    exp_len
                                    exp_param
                                    expConstantArray 
                                 ))
                          );
                          Ast_comb.single_non_rec_value
                            patFromJs
                            (Exp.fun_ "" None 
                               (Pat.var pat_param)
                               (if createType then 
                                  revSearchAssert
                                    exp_len
                                    expConstantArray
                                    (exp_param +: newType)
                                  +>
                                  core_type
                                else 
                                  revSearch                                      
                                    exp_len
                                    expConstantArray
                                    exp_param                                      
                                  +>
                                  Ast_core_type.lift_option_type core_type
                               )
                            )
                        ] in 
                        if createType then 
                          newTypeStr :: v 
                        else v 
                      | _ -> assert false 
                    end 
                  | None -> 
                    U.notApplicable 
                      tdcl.Parsetree.ptype_loc 
                      derivingName;
                    []
                 )

               | Ptype_variant ctors -> 
                 if Ast_polyvar.is_enum_constructors ctors then 
                   let xs = Ast_polyvar.map_constructor_declarations_into_ints ctors in 
                   match xs with 
                   | `New xs ->
                     let constantArrayExp = Exp.ident {loc; txt = Lident constantArray} in
                     let exp_len = const_int (List.length ctors) in
                     let v = [
                       eraseTypeStr;
                       Ast_comb.single_non_rec_value 
                         {loc; txt = constantArray}
                         (Exp.array (List.map (fun i -> const_int i) xs ))
                       ;
                       toJsBody                        
                         (
                           coerceResultToNewType @@
                           toInt
                             exp_param
                             constantArrayExp
                         )                       
                       ;
                       Ast_comb.single_non_rec_value
                         patFromJs
                         (Exp.fun_ "" None 
                            (Pat.var pat_param)
                            (
                              if createType then 
                                fromIntAssert
                                  exp_len
                                  constantArrayExp
                                  (exp_param +: newType)
                                +>
                                core_type
                              else 
                                fromInt                                 
                                  exp_len
                                  constantArrayExp
                                  exp_param
                                +>
                                Ast_core_type.lift_option_type core_type

                            )
                         )
                     ] in 
                     if createType then newTypeStr :: v else v 
                   | `Offset offset  ->                      
                     let v = 
                       [  eraseTypeStr;
                          toJsBody (
                            coerceResultToNewType
                              (eraseType exp_param +~ const_int offset)
                          )
                          ;
                          let len = List.length ctors in 
                          let range_low = const_int (offset + 0) in 
                          let range_upper = const_int (offset + len - 1) in 

                          Ast_comb.single_non_rec_value
                            {loc ; txt = fromJs}
                            (Exp.fun_ "" None 
                               (Pat.var pat_param)
                               (if createType then 
                                  (Exp.let_ Nonrecursive
                                     [Vb.mk
                                        (Pat.var pat_param)
                                        (exp_param +: newType)
                                     ]
                                     (
                                       Exp.sequence
                                         (assertExp 
                                            ((exp_param <=~ range_upper) &&~ (range_low <=~ exp_param))
                                         )
                                         (exp_param  -~ const_int offset))
                                  )
                                  +>
                                  core_type
                                else
                                  (Exp.ifthenelse
                                     ( (exp_param <=~ range_upper) &&~ (range_low <=~ exp_param))
                                     (Exp.construct {loc; txt = Lident "Some"} 
                                        ( Some (exp_param -~ const_int offset)))
                                     (Some (Exp.construct {loc; txt = Lident "None"} None)))
                                  +>
                                  Ast_core_type.lift_option_type core_type
                               )
                            )
                       ] in 
                     if createType then newTypeStr :: v else v 
                 else 
                   begin 
                     U.notApplicable 
                     tdcl.Parsetree.ptype_loc 
                     derivingName;
                     []  
                   end
               | Ptype_open -> 
                 U.notApplicable tdcl.Parsetree.ptype_loc 
                 derivingName;
                 [] in 
             Ext_list.flat_map handle_tdcl tdcls 
           );
         signature_gen = 
           (fun (tdcls : tdcls) _ -> 
              let handle_tdcl tdcl =
                let core_type = U.core_type_of_type_declaration tdcl 
                in 
                let name = tdcl.ptype_name.txt in 
                let toJs = name ^ "ToJs" in 
                let fromJs = name ^ "FromJs" in 
                let loc = tdcl.ptype_loc in 
                let patToJs = {Asttypes.loc; txt = toJs} in 
                let patFromJs = {Asttypes.loc; txt = fromJs} in 
                let toJsType result = 
                  Ast_comb.single_non_rec_val patToJs (Typ.arrow "" core_type result) in
                let newType,newTdcl =
                  U.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
                let newTypeStr = Sig.type_ [newTdcl] in                     
                let (+?) v rest = if createType then v :: rest else rest in 
                match tdcl.ptype_kind with  
                | Ptype_record label_declarations ->            

                  let objType flag =                     
                    Ast_comb.to_js_type loc @@  
                    Typ.object_
                      (List.map 
                         (fun ({pld_name = {loc; txt }; pld_type } : Parsetree.label_declaration) -> 
                            txt, [], pld_type
                         ) label_declarations) 
                      flag in                   
                  newTypeStr +? 
                  [
                    toJsType (if createType then newType else  objType Closed);
                    Ast_comb.single_non_rec_val patFromJs 
                      ( (if createType then  newType else objType Open)->~ core_type)
                  ] 

                | Ptype_abstract ->   
                  (match Ast_polyvar.is_enum_polyvar tdcl with 
                   | Some _ ->                     
                     let ty1 =  
                       if createType then newType else 
                         (Ast_literal.type_string ()) in 
                     let ty2 = 
                       if createType then core_type
                       else Ast_core_type.lift_option_type core_type in 
                     newTypeStr +? 
                     [
                       toJsType ty1;
                       Ast_comb.single_non_rec_val     
                         patFromJs
                         (ty1 ->~ ty2)
                     ] 

                   | None -> 
                     U.notApplicable tdcl.Parsetree.ptype_loc 
                     derivingName;
                     [])

                | Ptype_variant ctors 
                  -> 

                  if Ast_polyvar.is_enum_constructors ctors then 
                    let ty1 = 
                      if createType then newType 
                      else Ast_literal.type_int() in 
                    let ty2 = 
                      if createType then core_type
                      else Ast_core_type.lift_option_type core_type in 
                    newTypeStr +? 
                    [
                      toJsType ty1;
                      Ast_comb.single_non_rec_val
                        patFromJs
                        (ty1 ->~ ty2)
                    ] 

                  else 
                  begin
                    U.notApplicable tdcl.Parsetree.ptype_loc 
                    derivingName;
                    []
                  end
                | Ptype_open -> 
                  U.notApplicable tdcl.Parsetree.ptype_loc 
                  derivingName;
                  [] in 
              Ext_list.flat_map handle_tdcl tdcls 

           );
         expression_gen = None 
       } 
    )
;

end
module Ast_derive_projector : sig 
#1 "ast_derive_projector.mli"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



val init : unit -> unit

end = struct
#1 "ast_derive_projector.ml"
open Ast_helper

let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"



type tdcls = Parsetree.type_declaration list 

let derivingName = "accessors" 
let init () =
  
  Ast_derive.register
    derivingName
    (fun (x : Parsetree.expression option) ->
       (match x with 
        | Some config -> invalid_config config
        | None -> ());
       {structure_gen = 
          begin fun (tdcls : tdcls) _explict_nonrec ->
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                -> 
                label_declarations 
                |> Ext_list.map (
                  fun ({pld_name = {loc; txt = pld_label} as pld_name} : Parsetree.label_declaration) -> 
                    let txt = "param" in
                    Ast_comb.single_non_rec_value pld_name
                      (Exp.fun_ "" None
                         (Pat.constraint_ (Pat.var {txt ; loc}) core_type )
                         (Exp.field (Exp.ident {txt = Lident txt ; loc}) 
                            {txt = Longident.Lident pld_label ; loc}) )
                )
              | Ptype_variant constructor_declarations 
                -> 
                constructor_declarations
                |> Ext_list.map 
                  (fun
                    ( {pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                        Parsetree.constructor_declaration)
                    -> (* TODO: add type annotations *)
                      let little_con_name = String.uncapitalize con_name  in
                      let arity = List.length pcd_args in 
                      Ast_comb.single_non_rec_value {loc ; txt = little_con_name}
                        (
                          if arity = 0 then (*TODO: add a prefix, better inter-op with FFI *)
                            (Exp.constraint_
                               (Exp.construct {loc ; txt = Longident.Lident con_name } None)
                               core_type
                            )
                          else 
                            begin 
                              let vars = 
                                Ext_list.init  arity (fun x -> "param_" ^ string_of_int x ) in 
                              let exp = 
                                Exp.constraint_
                                  ( 
                                    Exp.construct {loc ; txt = Longident.Lident con_name} @@ 
                                    Some
                                      ( 
                                        if  arity = 1 then 
                                          Exp.ident { loc ; txt = Longident.Lident (List.hd vars )}
                                        else 
                                          Exp.tuple (Ext_list.map 
                                                       (fun x -> Exp.ident {loc ; txt = Longident.Lident x})
                                                       vars 
                                                    ) )) core_type
                              in 
                              Ext_list.fold_right  (fun var b -> 
                                  Exp.fun_ "" None  (Pat.var {loc ; txt = var}) b 
                                ) vars exp  

                            end)
                  )
              | Ptype_abstract | Ptype_open ->
                Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
               []
              (* Location.raise_errorf "projector only works with record" *)
            in Ext_list.flat_map handle_tdcl tdcls


          end;
        signature_gen = 
          begin fun (tdcls : Parsetree.type_declaration list) _explict_nonrec -> 
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                -> 
                label_declarations 
                |> Ext_list.map 
                  (fun 
                    ({pld_name ;
                      pld_type
                     } : 
                       Parsetree.label_declaration) -> 
                    Ast_comb.single_non_rec_val pld_name (Typ.arrow "" core_type pld_type )
                  )
              | Ptype_variant constructor_declarations 
                -> 
                constructor_declarations
                |>
                Ext_list.map
                  (fun  ({pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                           Parsetree.constructor_declaration)
                    -> 
                      Ast_comb.single_non_rec_val {loc ; txt = (String.uncapitalize con_name)}
                        (Ext_list.fold_right 
                           (fun x acc -> Typ.arrow "" x acc) 
                           pcd_args
                           core_type))
              | Ptype_open | Ptype_abstract -> 
              Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
              [] 
            in 
            Ext_list.flat_map handle_tdcl tdcls
          end;
        expression_gen = None
       }
    )


end
module Ast_tuple_pattern_flatten : sig 
#1 "ast_tuple_pattern_flatten.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val map_open_tuple:
  Parsetree.expression ->
  (Parsetree.expression list ->
    Parsetree.attributes ->
   Parsetree.expression) ->
  Parsetree.expression option


val handle_value_bindings :
  Bs_ast_mapper.mapper ->
  Parsetree.value_binding list ->
  Parsetree.value_binding list

end = struct
#1 "ast_tuple_pattern_flatten.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type loc = Location.t

type exp = Parsetree.expression

type pat = Parsetree.pattern


type whole =
  | Let_open of
      (Asttypes.override_flag * Longident.t Asttypes.loc * loc *
       Parsetree.attributes)

type wholes = whole list

let rec is_simple_pattern (p : Parsetree.pattern) =
  match p.ppat_desc with
  | Ppat_any -> true
  | Ppat_var _ -> true
  | Ppat_constraint(p,_) -> is_simple_pattern p
  | _ -> false

type destruct_output =
  exp list
  
(**
   destruct such pattern
   {[ A.B.let open C in (a,b)]}
*)
let rec destruct_open_tuple
    (e : Parsetree.expression)
    (acc : whole list)
  : (wholes * destruct_output * _) option =
  match e.pexp_desc with
  | Pexp_open (flag, lid, cont)
    ->
    destruct_open_tuple
      cont
      (Let_open (flag, lid, e.pexp_loc, e.pexp_attributes) :: acc)
  | Pexp_tuple es -> Some (acc, es, e.pexp_attributes)
  | _ -> None

let map_open_tuple
    (e : Parsetree.expression)
    (f : Parsetree.expression list -> _ -> Parsetree.expression) =
  match destruct_open_tuple e [] with
  | None ->  None (** not an open tuple *)
  | Some (qualifiers, es, attrs ) ->
    Some (List.fold_left (fun x hole  ->
        match hole with
        | Let_open (flag, lid,loc,attrs) ->
          {Parsetree.
            pexp_desc = Pexp_open (flag,lid,x);
            pexp_attributes = attrs;
            pexp_loc = loc
          }
      ) (f es attrs) qualifiers)
(*
  [let (a,b) = M.N.(c,d) ]
  =>
  [ let a = M.N.c
    and b = M.N.d ]
*)
let flattern_tuple_pattern_vb
    (self : Bs_ast_mapper.mapper)
    ({pvb_loc } as vb :  Parsetree.value_binding)
    acc : Parsetree.value_binding list =
  let pvb_pat = self.pat self vb.pvb_pat in
  let pvb_expr = self.expr self vb.pvb_expr in
  let pvb_attributes = self.attributes self vb.pvb_attributes in
  match pvb_pat.ppat_desc with
  | Ppat_tuple xs when List.for_all is_simple_pattern xs ->
    begin match destruct_open_tuple pvb_expr []  with
      | Some (wholes, es, tuple_attributes)
        when
          List.for_all is_simple_pattern xs &&
          Ext_list.same_length es xs
        ->
        Bs_ast_invariant.warn_unused_attributes tuple_attributes ; (* will be dropped*)
        (Ext_list.fold_right2 (fun pat exp acc->
             {Parsetree.
               pvb_pat =
                 pat;
               pvb_expr =
                 ( match wholes with
                   | [] -> exp
                   | _ ->
                     List.fold_left (fun x  whole ->
                         match whole with
                         | Let_open (flag,lid,loc,attrs) ->
                           {Parsetree.
                             pexp_desc = Pexp_open(flag,lid,x);
                             pexp_attributes = attrs;
                             pexp_loc = loc
                           }
                       ) exp wholes) ;
               pvb_attributes;
               pvb_loc ;
             } :: acc
           ) xs es) acc
      | _ ->
        {pvb_pat ;
         pvb_expr ;
         pvb_loc ;
         pvb_attributes} :: acc
    end
  | _ ->
    {pvb_pat ;
     pvb_expr ;
     pvb_loc ;
     pvb_attributes} :: acc


let handle_value_bindings  =
  fun self (vbs : Parsetree.value_binding list) ->
    (* Bs_ast_mapper.default_mapper.value_bindings self  vbs   *)
    List.fold_right (fun vb acc ->
        flattern_tuple_pattern_vb self vb acc
      ) vbs []

end
module Ast_exp_apply : sig 
#1 "ast_exp_apply.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val handle_exp_apply :
  Parsetree.expression ->
  Bs_ast_mapper.mapper ->
  Parsetree.expression ->
  (Asttypes.label * Parsetree.expression) list ->
  Parsetree.expression

end = struct
#1 "ast_exp_apply.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper
type exp = Parsetree.expression

let rec no_need_bound (exp : exp) =
  match exp.pexp_desc with
  | Pexp_ident { txt = Lident _} -> true
  | Pexp_constraint(e,_) -> no_need_bound e
  | _ -> false

let ocaml_obj_id = "__ocaml_internal_obj"

let bound (e : exp) (cb : exp -> _) =
  if no_need_bound e then cb e
  else
    let loc = e.pexp_loc in
    Exp.let_ ~loc Nonrecursive
      [ Vb.mk ~loc (Pat.var ~loc {txt = ocaml_obj_id; loc}) e ]
      (cb (Exp.ident ~loc {txt = Lident ocaml_obj_id; loc}))

let handle_exp_apply
    (e  : exp)
    (self : Bs_ast_mapper.mapper)
    (fn : exp)
    (args : (Asttypes.label * Parsetree.expression) list)
  =
  let loc = e.pexp_loc in
  begin match fn.pexp_desc with
    | Pexp_apply (
        {pexp_desc =
           Pexp_ident  {txt = Lident "##"  ; loc} ; _},
        [("", obj) ;
         ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
        ])
      ->  (* f##paint 1 2 *)
      {e with pexp_desc = Ast_util.method_apply loc self obj name args }
    | Pexp_apply (
        {pexp_desc =
           Pexp_ident  {txt = Lident "#@"  ; loc} ; _},
        [("", obj) ;
         ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
        ])
      ->  (* f##paint 1 2 *)
      {e with pexp_desc = Ast_util.property_apply loc self obj name args  }
    | Pexp_ident {txt = Lident "|."} ->
      (*
        a |. f
        a |. f b c [@bs]  --> f a b c [@bs]
      *)
      begin match args with
        | [ "", obj_arg ;
            "", fn
          ] ->
          let new_obj_arg = self.expr self obj_arg in
          begin match fn with
            | {pexp_desc = Pexp_apply (fn, args); pexp_loc; pexp_attributes} ->
              let fn = self.expr self fn in
              let args = Ext_list.map (fun (lab,exp) -> lab, self.expr self exp ) args in
              Bs_ast_invariant.warn_unused_attributes pexp_attributes;
              { pexp_desc = Pexp_apply(fn, ("", new_obj_arg) :: args);
                pexp_attributes = [];
                pexp_loc = pexp_loc}
            | {pexp_desc = Pexp_construct(ctor,None); pexp_loc; pexp_attributes} -> 
              {fn with pexp_desc = Pexp_construct(ctor, Some new_obj_arg)}
            | _ ->
              let try_dispatch_by_tuple =
                Ast_tuple_pattern_flatten.map_open_tuple fn (fun xs tuple_attrs ->
                    bound new_obj_arg @@  fun bounded_obj_arg ->
                    {
                      pexp_desc =
                        Pexp_tuple (
                          Ext_list.map (fun (fn : Parsetree.expression) ->
                              match fn with
                              | {pexp_desc = Pexp_apply (fn,args); pexp_loc; pexp_attributes }
                                ->
                                let fn = self.expr self fn in
                                let args = Ext_list.map (fun (lab,exp) -> lab, self.expr self exp ) args in
                                Bs_ast_invariant.warn_unused_attributes pexp_attributes;
                                { Parsetree.pexp_desc = Pexp_apply(fn, ("", bounded_obj_arg) :: args);
                                  pexp_attributes = [];
                                  pexp_loc = pexp_loc}
                              | {pexp_desc = Pexp_construct(ctor,None); pexp_loc; pexp_attributes}    
                                -> 
                                {fn with pexp_desc = Pexp_construct(ctor, Some bounded_obj_arg)}
                              | _ ->
                                Exp.apply ~loc:fn.pexp_loc
                                  (self.expr self fn )
                                  ["", bounded_obj_arg]
                            ) xs );
                      pexp_attributes = tuple_attrs;
                      pexp_loc = fn.pexp_loc;
                    }
                  ) in
              begin match try_dispatch_by_tuple  with
                | Some x -> x
                | None ->
                  Exp.apply ~loc (self.expr self fn) ["", new_obj_arg]
              end
          end
        | _ ->
          Location.raise_errorf ~loc
            "invalid |. syntax "
      end

    | Pexp_ident  {txt = Lident "##" ; loc}
      ->
      begin match args with
        | [("", obj) ;
           ("", {pexp_desc = Pexp_apply(
                {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                args
              ); pexp_attributes = attrs }
           (* we should warn when we discard attributes *)
           )
          ] -> (* f##(paint 1 2 ) *)
          (* gpr#1063 foo##(bar##baz) we should rewrite (bar##baz)
             first  before pattern match.
             currently the pattern match is written in a top down style.
             Another corner case: f##(g a b [@bs])
          *)
          Bs_ast_invariant.warn_unused_attributes attrs ;
          {e with pexp_desc = Ast_util.method_apply loc self obj name args}
        | [("", obj) ;
           ("",
            {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
           )  (* f##paint  *)
          ] ->
          { e with pexp_desc =
                     Ast_util.js_property loc (self.expr self obj) name
          }

        | _ ->
          Location.raise_errorf ~loc
            "Js object ## expect syntax like obj##(paint (a,b)) "
      end
    (* we can not use [:=] for precedece cases
       like {[i @@ x##length := 3 ]}
       is parsed as {[ (i @@ x##length) := 3]}
       since we allow user to create Js objects in OCaml, it can be of
       ref type
       {[
         let u = object (self)
           val x = ref 3
           method setX x = self##x := 32
           method getX () = !self##x
         end
       ]}
    *)
    | Pexp_ident {txt = Lident "#=" } ->
      begin match args with
        | ["",
           {pexp_desc =
              Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "##"}},
                          ["", obj;
                           "", {pexp_desc = Pexp_ident {txt = Lident name}}
                          ]
                         )};
           "", arg
          ] ->
          Exp.constraint_ ~loc
            { e with
              pexp_desc =
                Ast_util.method_apply loc self obj
                  (name ^ Literals.setter_suffix) ["", arg ]  }
            (Ast_literal.type_unit ~loc ())
        | _ -> Bs_ast_mapper.default_mapper.expr self e
      end
    | _ ->
      begin match
          Ext_list.exclude_with_val
            Ast_attributes.is_bs e.pexp_attributes with
      | false, _ -> Bs_ast_mapper.default_mapper.expr self e
      | true, pexp_attributes ->
        {e with pexp_desc = Ast_util.uncurry_fn_apply loc self fn args ;
                pexp_attributes }
      end
  end

end
module Ast_exp_extension : sig 
#1 "ast_exp_extension.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val handle_extension :
  bool ref ->
  Parsetree.expression ->
  Bs_ast_mapper.mapper ->
  Parsetree.extension ->
  Parsetree.expression


type t = { args : string list ; block :  string }

val fromString : string -> t 
  
end = struct
#1 "ast_exp_extension.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)
open Ast_helper

let rec unroll_function_aux 
  (acc : string list)
  (body : Parsetree.expression) : string list * string =
  match body.pexp_desc with
  | Pexp_constant(Const_string(block,_)) -> acc, block
  | Pexp_fun("",None,{ppat_desc = Ppat_var s},cont) -> 
    unroll_function_aux (s.txt::acc) cont
  | _ -> 
    Location.raise_errorf ~loc:body.pexp_loc  
    "bs.raw can only be applied to a string or a special function form "

type t = { args : string list ; block :  string }

let toString (x : t) = 
  Bs_version.version ^ Marshal.to_string x []

(* exception handling*)
let fromString (x : string) : t = 
  if Ext_string.starts_with x Bs_version.version then 
    Marshal.from_string x (String.length Bs_version.version)
  else 
     Ext_pervasives.failwithf
        ~loc:__LOC__
        "Compiler version mismatch. The project might have been built with one version of BuckleScript, and then with another. Please wipe the artifacts and do a clean build."

let handle_extension record_as_js_object e (self : Bs_ast_mapper.mapper)
    (({txt ; loc} as lid , payload) : Parsetree.extension) = 
  begin match txt with
    | "bs.raw" | "raw" -> 
      begin match payload with 
      | PStr [{pstr_desc = Pstr_eval({pexp_desc = Pexp_fun("",None,pat,body)},_)}]
         -> 
         begin match pat.ppat_desc, body.pexp_desc with 
         | Ppat_construct ({txt = Lident "()"}, None), Pexp_constant(Const_string(block,_))
           -> 
            Exp.apply ~loc 
            (Exp.ident ~loc {txt = Ldot (Ast_literal.Lid.js_unsafe, Literals.raw_function);loc})
            [ "", 
              Exp.constant ~loc (Const_string (toString {args = [] ; block }, None))            
            ]
            
         | Ppat_var ({txt;}), _ -> 
            let acc, block = unroll_function_aux [txt] body in 
            (Exp.apply ~loc 
            (Exp.ident ~loc {txt = Ldot (Ast_literal.Lid.js_unsafe, Literals.raw_function);loc})
            [ "", Exp.constant ~loc (Const_string (toString {args = List.rev acc ; block },None))]            
            )
         | _ -> Location.raise_errorf ~loc "bs.raw can only be applied to a string or a special function form "
         end 
      | _ ->   Ast_util.handle_raw ~check_js_regex:false loc payload
      end
    | "bs.re" | "re" ->
      Exp.constraint_ ~loc
        (Ast_util.handle_raw ~check_js_regex:true loc payload)
        (Ast_comb.to_js_re_type loc)
    | "bs.external" | "external" ->
      begin match Ast_payload.as_ident payload with 
        | Some {txt = Lident x}
          -> Ast_util.handle_external loc x
        (* do we need support [%external gg.xx ] 

           {[ Js.Undefined.to_opt (if Js.typeof x == "undefined" then x else Js.Undefined.empty ) ]}
        *)

        | None | Some _ -> 
          Location.raise_errorf ~loc 
            "external expects a single identifier"
      end
    | "bs.time"| "time" ->
      (
        match payload with 
        | PStr [{pstr_desc = Pstr_eval (e,_)}] -> 
          let locString = 
            if loc.loc_ghost then 
              "GHOST LOC"
            else 
              let loc_start = loc.loc_start in 
              let (file, lnum, __) = Location.get_pos_info loc_start in                  
              Printf.sprintf "%s %d"
                file lnum in   
          let e = self.expr self e in 
          Exp.sequence ~loc
            (Exp.apply ~loc     
               (Exp.ident ~loc {loc; 
                                txt = 
                                  Ldot (Ldot (Lident "Js", "Console"), "timeStart")   
                               })
               ["", Exp.constant ~loc (Const_string (locString,None))]
            )     
            ( Exp.let_ ~loc Nonrecursive
                [Vb.mk ~loc (Pat.var ~loc {loc; txt = "timed"}) e ;
                ]
                (Exp.sequence ~loc
                   (Exp.apply ~loc     
                      (Exp.ident ~loc {loc; 
                                       txt = 
                                         Ldot (Ldot (Lident "Js", "Console"), "timeEnd")   
                                      })
                      ["", Exp.constant ~loc (Const_string (locString,None))]
                   )    
                   (Exp.ident ~loc {loc; txt = Lident "timed"})
                )
            )
        | _ -> 
          Location.raise_errorf 
            ~loc "expect a boolean expression in the payload"
      )
    | "bs.assert" | "assert" ->
      (
        match payload with 
        | PStr [ {pstr_desc = Pstr_eval( e,_)}] -> 

          let locString = 
            if loc.loc_ghost then 
              "ASSERT FAILURE"
            else 
              let loc_start = loc.loc_start in 
              let (file, lnum, cnum) = Location.get_pos_info loc_start in
              let enum = 
                loc.Location.loc_end.Lexing.pos_cnum -
                loc_start.Lexing.pos_cnum + cnum in
              Printf.sprintf "File %S, line %d, characters %d-%d"
                file lnum cnum enum in   
          let raiseWithString  locString =      
            (Exp.apply ~loc 
               (Exp.ident ~loc {loc; txt = 
                                       Ldot(Ldot (Lident "Js","Exn"),"raiseError")})
               ["",

                Exp.constant (Const_string (locString,None))    
               ])
          in 
          (match e.pexp_desc with
           | Pexp_construct({txt = Lident "false"},None) -> 
             (* The backend will convert [assert false] into a nop later *)
             if !Clflags.no_assert_false  then 
               Exp.assert_ ~loc 
                 (Exp.construct ~loc {txt = Lident "false";loc} None)
             else 
               (raiseWithString locString)
           | Pexp_constant (Const_string (r, _)) -> 
             if !Clflags.noassert then 
               Exp.assert_ ~loc (Exp.construct ~loc {txt = Lident "true"; loc} None)
               (* Need special handling to make it type check*)
             else   
               raiseWithString r
           | _ ->    
             let e = self.expr self  e in 
             if !Clflags.noassert then 
               (* pass down so that it still type check, but the backend will
                  make it a nop
               *)
               Exp.assert_ ~loc e
             else 
               Exp.ifthenelse ~loc
                 (Exp.apply ~loc
                    (Exp.ident {loc ; txt = Ldot(Lident "Pervasives","not")})
                    ["", e]
                 )
                 (raiseWithString locString)
                 None
          )
        | _ -> 
          Location.raise_errorf 
            ~loc "expect a boolean expression in the payload"
      )
    | "bs.node" | "node" ->
      let strip s =
        match s with 
        | "_module" -> "module" 
        | x -> x  in 
      begin match Ast_payload.as_ident payload with
        | Some {txt = Lident
                    ( "__filename"
                    | "__dirname"
                    | "_module"
                    | "require" as name); loc}
          ->
          let exp =
            Ast_util.handle_external loc (strip name)  in
          let typ =
            Ast_core_type.lift_option_type  
            @@                 
            if name = "_module" then
              Typ.constr ~loc
                { txt = Ldot (Lident "Node", "node_module") ;
                  loc} []   
            else if name = "require" then
              (Typ.constr ~loc
                 { txt = Ldot (Lident "Node", "node_require") ;
                   loc} [] )  
            else
              Ast_literal.type_string ~loc () in                  
          Exp.constraint_ ~loc exp typ                
        | Some _ | None ->
          begin match payload with 
            | PTyp _ -> 
              Location.raise_errorf 
                ~loc "Illegal payload, expect an expression payload instead of type payload"              
            | PPat _ ->
              Location.raise_errorf 
                ~loc "Illegal payload, expect an expression payload instead of pattern  payload"        
            | _ -> 
              Location.raise_errorf 
                ~loc "Illegal payload"
          end

      end             
    | "bs.debugger"|"debugger" ->
      {e with pexp_desc = Ast_util.handle_debugger loc payload}
    | "bs.obj" | "obj" ->
      begin match payload with 
        | PStr [{pstr_desc = Pstr_eval (e,_)}]
          -> 
          Ext_ref.non_exn_protect record_as_js_object true
            (fun () -> self.expr self e ) 
        | _ -> Location.raise_errorf ~loc "Expect an expression here"
      end
    | _ ->
      match payload with
      | PTyp typ when Ext_string.starts_with txt Literals.bs_deriving_dot ->
        self.expr self (Ast_derive.gen_expression lid typ)
      | _ ->  
        e (* For an unknown extension, we don't really need to process further*)
        (* Exp.extension ~loc ~attrs:e.pexp_attributes (
            self.extension self extension) *)
        (* Bs_ast_mapper.default_mapper.expr self e   *)
  end 

end
module Ast_primitive : sig 
#1 "ast_primitive.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val handlePrimitiveInSig:
  Bs_ast_mapper.mapper ->
  Parsetree.value_description ->
  Parsetree.signature_item ->
  Parsetree.signature_item

val handlePrimitiveInStru:
  Bs_ast_mapper.mapper ->
  Parsetree.value_description ->
  Parsetree.structure_item ->
  Parsetree.structure_item
  

end = struct
#1 "ast_primitive.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let handlePrimitiveInSig
    (self : Bs_ast_mapper.mapper)
    ({pval_attributes;
      pval_type;
      pval_loc;
      pval_prim;
      pval_name ;
     } as prim : Parsetree.value_description)
    (sigi : Parsetree.signature_item)
  : Parsetree.signature_item
  =
  let pval_type = self.typ self pval_type in
  let pval_attributes = self.attributes self pval_attributes in
  let pval_type, pval_prim, pval_attributes =
    match pval_prim with
    | [ v ] ->
      External_process.handle_attributes_as_string
        pval_loc
        pval_name.txt
        pval_type
        pval_attributes v
    | _ ->
      Location.raise_errorf
        ~loc:pval_loc
        "only a single string is allowed in bs external" in
  {sigi with
   psig_desc =
     Psig_value
       {prim with
        pval_type ;
        pval_prim ;
        pval_attributes
       }}

let handlePrimitiveInStru
    (self : Bs_ast_mapper.mapper)
    ({pval_attributes;
      pval_prim;
      pval_type;
      pval_name;
      pval_loc} as prim : Parsetree.value_description)
    (str : Parsetree.structure_item)
    : Parsetree.structure_item =
  let pval_type = self.typ self pval_type in
  let pval_attributes = self.attributes self pval_attributes in
  let pval_type, pval_prim, pval_attributes =
    match pval_prim with
    | [ v] ->
      External_process.handle_attributes_as_string
        pval_loc
        pval_name.txt
        pval_type pval_attributes v

    | _ -> Location.raise_errorf
             ~loc:pval_loc "only a single string is allowed in bs external" in
  {str with
   pstr_desc =
     Pstr_primitive
       {prim with
        pval_type ;
        pval_prim;
        pval_attributes
       }}

end
module Ast_derive_abstract : sig 
#1 "ast_derive_abstract.mli"
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

val handleTdclsInStr : 
  Parsetree.type_declaration list -> Parsetree.structure

val handleTdclsInSig:  
  Parsetree.type_declaration list -> Parsetree.signature
end = struct
#1 "ast_derive_abstract.ml"
(* Copyright (C) 2017 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


let derivingName = "abstract"
module U = Ast_derive_util
open Ast_helper
type tdcls = Parsetree.type_declaration list

let handle_config (config : Parsetree.expression option) =
  match config with
  | Some config ->
    U.invalid_config config
  | None -> ()



let get_optional_attrs =
  [Ast_attributes.bs_get; Ast_attributes.bs_return_undefined]
(** For this attributes, its type was wrapped as an option,
   so we can still reuse existing frame work
*)  

let get_attrs = [ Ast_attributes.bs_get_arity]
let set_attrs = [Ast_attributes.bs_set]
let handleTdcl (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      List.exists (fun ({pld_type; pld_attributes} : Parsetree.label_declaration) ->
          Ast_attributes.has_bs_optional pld_attributes
        ) label_declarations in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          let prim_as_name, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              label_name, pld_name
            | Some new_name ->
              new_name, {pld_name with txt = new_name}
          in
          let prim = [prim_as_name] in 
          let is_optional = Ast_attributes.has_bs_optional pld_attributes in
          let maker, getter_declaration =
            if is_optional then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              (Ast_core_type.opt_arrow pld_loc label_name optional_type maker,
              Val.mk ~loc:pld_loc pld_name 
                ~attrs:get_optional_attrs ~prim
                (Typ.arrow ~loc "" core_type optional_type)
                )
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
              Val.mk ~loc:pld_loc pld_name ~attrs:get_attrs
              ~prim:(
                ["" ; (* Not needed actually*)
                External_ffi_types.to_string 
                (Ffi_bs (
                  [{arg_type = Nothing; arg_label = External_arg_spec.empty_label}],
                  Return_identity,
                  Js_get {js_get_name = prim_as_name; js_get_scopes = []}
                  ))] )
               (Typ.arrow ~loc "" core_type pld_type)
          in
          let acc =
           getter_declaration :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              Val.mk ~loc:pld_loc
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:set_attrs
                ~prim setter_type
              :: acc
            else acc in
          acc,
          maker,
          (is_optional, newLabel)::labels
        ) label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       let myPrims =
        External_process.pval_prim_of_option_labels
          labels
          has_optional_field
        in
       let myMaker =
         Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:myPrims makeType in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclsInStr tdcls =
  let tdcls, code =
    List.fold_right (fun tdcl (tdcls, sts)  ->
        match handleTdcl tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append (fun x -> Str.primitive x) value_descriptions sts

      ) tdcls ([],[])  in
  Str.type_ tdcls :: code
(* still need perform transformation for non-abstract type*)

let handleTdclsInSig tdcls =
  let tdcls, code =
    List.fold_right (fun tdcl (tdcls, sts)  ->
        match handleTdcl tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append (fun x -> Sig.value x) value_descriptions sts

      ) tdcls ([],[])  in
  Sig.type_ tdcls :: code

end
module Ast_tdcls : sig 
#1 "ast_tdcls.mli"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


val handleTdclsInSigi :
  Bs_ast_mapper.mapper ->
  Parsetree.signature_item ->
  Parsetree.type_declaration list -> Ast_signature.item


val handleTdclsInStru :
  Bs_ast_mapper.mapper ->
  Parsetree.structure_item ->
  Parsetree.type_declaration list -> Ast_structure.item

end = struct
#1 "ast_tdcls.ml"
(* Copyright (C) 2018 Authors of BuckleScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

open Ast_helper

(**
   [newTdcls tdcls newAttrs]
   functional update attributes of last declaration *)
let newTdcls
    (tdcls : Parsetree.type_declaration list)
    (newAttrs : Parsetree.attributes)
  : Parsetree.type_declaration list
  =
  match tdcls with
  | [ x ] ->
    [{ x with Parsetree.ptype_attributes = newAttrs}]
  | _ ->
    Ext_list.map_last
      (fun last x ->
         if last then
           { x with
             Parsetree.ptype_attributes = newAttrs}
         else x )
      tdcls


let handleTdclsInSigi
    (self : Bs_ast_mapper.mapper)
    (sigi : Parsetree.signature_item)
    (tdcls : Parsetree.type_declaration list)
  : Ast_signature.item =
  begin match Ast_attributes.process_derive_type
                (Ext_list.last tdcls).ptype_attributes  with
  | {bs_deriving = Some actions; explict_nonrec}, newAttrs
    ->
    let loc = sigi.psig_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in (* remove the processed attr*)
    let newTdclsNewAttrs = self.type_declaration_list self originalTdclsNewAttrs in
    if Ast_payload.isAbstract actions then
      let  codes = Ast_derive_abstract.handleTdclsInSig originalTdclsNewAttrs in
      Ast_signature.fuseAll ~loc
        (
          Sig.include_ ~loc
            (Incl.mk ~loc
               (Mty.typeof_ ~loc
                  (Mod.constraint_ ~loc
                     (Mod.structure ~loc [
                         { pstr_loc = loc;
                           pstr_desc =
                             Pstr_type newTdclsNewAttrs
                         }] )
                     (Mty.signature ~loc [])) ) )
          :: (* include module type of struct [processed_code for checking like invariance ]end *)
          self.signature self  codes
        )
    else
      Ast_signature.fuseAll ~loc
        ( {psig_desc = Psig_type newTdclsNewAttrs; psig_loc = loc}::
         self.signature
           self
           (Ast_derive.gen_signature tdcls actions explict_nonrec))
  | {bs_deriving = None }, _  ->
    Bs_ast_mapper.default_mapper.signature_item self sigi

  end


let handleTdclsInStru
    (self : Bs_ast_mapper.mapper)
    (str : Parsetree.structure_item)
    (tdcls : Parsetree.type_declaration list)
  : Ast_structure.item =
  begin match
      Ast_attributes.process_derive_type
        ((Ext_list.last tdcls).ptype_attributes) with
  | {bs_deriving = Some actions;
     explict_nonrec
    }, newAttrs ->
    let loc = str.pstr_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in
    let newStr : Parsetree.structure_item =
      { pstr_desc = Pstr_type (self.type_declaration_list self originalTdclsNewAttrs);
        pstr_loc = loc}
    in
    if Ast_payload.isAbstract actions then
      let codes = Ast_derive_abstract.handleTdclsInStr originalTdclsNewAttrs in
      (* use [tdcls2] avoid nonterminating *)
      Ast_structure.fuseAll ~loc
        (
          Ast_structure.constraint_ ~loc [newStr] []
          :: (* [include struct end : sig end] for error checking *)
          self.structure self codes)
    else
      Ast_structure.fuseAll ~loc
        (newStr ::
         self.structure self
           (
             List.map
               (fun action ->
                  Ast_derive.gen_structure_signature
                    loc
                    tdcls action explict_nonrec
               )    actions
           ))
  | {bs_deriving = None }, _  ->
    Bs_ast_mapper.default_mapper.structure_item self str
  end



end
module Ext_char : sig 
#1 "ext_char.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






(** Extension to Standard char module, avoid locale sensitivity *)

val escaped : char -> string


val valid_hex : char -> bool

val is_lower_case : char -> bool
end = struct
#1 "ext_char.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







external string_unsafe_set : string -> int -> char -> unit
                           = "%string_unsafe_set"

external string_create: int -> string = "caml_create_string"

external unsafe_chr: int -> char = "%identity"

(** {!Char.escaped} is locale sensitive in 4.02.3, fixed in the trunk,
    backport it here
 *)
let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | ' ' .. '~' as c ->
      let s = string_create 1 in
      string_unsafe_set s 0 c;
      s
  | c ->
      let n = Char.code c in
      let s = string_create 4 in
      string_unsafe_set s 0 '\\';
      string_unsafe_set s 1 (unsafe_chr (48 + n / 100));
      string_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
      string_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
      s


let valid_hex x = 
    match x with 
    | '0' .. '9'
    | 'a' .. 'f'
    | 'A' .. 'F' -> true
    | _ -> false 



let is_lower_case c =
  (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')    
end
module Ast_utf8_string : sig 
#1 "ast_utf8_string.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type error 


type exn += Error of int  (* offset *) * error 

val pp_error :  Format.formatter -> error -> unit  


  
(* module Interp : sig *)
(*   val check_and_transform : int -> string -> int -> cxt -> unit *)
(*   val transform_test : string -> segments *)
(* end *)
val transform_test : string -> string 

val transform : Location.t -> string -> string      


end = struct
#1 "ast_utf8_string.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



type error = 
  | Invalid_code_point 
  | Unterminated_backslash
  | Invalid_escape_code of char 
  | Invalid_hex_escape
  | Invalid_unicode_escape

let pp_error fmt err = 
  Format.pp_print_string fmt @@  match err with 
  | Invalid_code_point -> "Invalid code point"
  | Unterminated_backslash -> "\\ ended unexpectedly"
  | Invalid_escape_code c -> "Invalid escape code: " ^ String.make 1 c 
  | Invalid_hex_escape -> 
    "Invalid \\x escape"
  | Invalid_unicode_escape -> "Invalid \\u escape"



type exn += Error of int  (* offset *) * error 




let error ~loc error = 
  raise (Error (loc, error))

(** Note the [loc] really should be the utf8-offset, it has nothing to do with our 
    escaping mechanism
*)
(* we can not just print new line in ES5 
   seems we don't need 
   escape "\b" "\f" 
   we need escape "\n" "\r" since 
   ocaml multiple-line allows [\n]
   visual input while es5 string 
   does not*)

let rec check_and_transform (loc : int ) buf s byte_offset s_len =
  if byte_offset = s_len then ()
  else 
    let current_char = s.[byte_offset] in 
    match Ext_utf8.classify current_char with 
    | Single 92 (* '\\' *) -> 
      escape_code (loc + 1) buf s (byte_offset+1) s_len
    | Single 34 ->
      Buffer.add_string buf "\\\"";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len
    | Single 39 -> 
      Buffer.add_string buf "\\'";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len 
    | Single 10 ->          
      Buffer.add_string buf "\\n";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len 
    | Single 13 -> 
      Buffer.add_string buf "\\r";
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len 
    | Single _ -> 
      Buffer.add_char buf current_char;
      check_and_transform (loc + 1) buf s (byte_offset + 1) s_len 

    | Invalid 
    | Cont _ -> error ~loc Invalid_code_point
    | Leading (n,_) -> 
      let i' = Ext_utf8.next s ~remaining:n  byte_offset in
      if i' < 0 then 
        error ~loc Invalid_code_point
      else 
        begin 
          for k = byte_offset to i' do 
            Buffer.add_char buf s.[k]; 
          done;   
          check_and_transform (loc + 1 ) buf s (i' + 1) s_len 
        end
(* we share the same escape sequence with js *)        
and escape_code loc buf s offset s_len = 
  if offset >= s_len then 
    error ~loc Unterminated_backslash
  else
    Buffer.add_char buf '\\'; 
  let cur_char = s.[offset] in
  match cur_char with 
  | '\\'
  | 'b' 
  | 't' 
  | 'n' 
  | 'v'
  | 'f'
  | 'r' 
  | '0' 
  | '$'
    -> 
    begin 
      Buffer.add_char buf cur_char ;
      check_and_transform (loc + 1) buf s (offset + 1) s_len 
    end 
  | 'u' -> 
    begin 
      Buffer.add_char buf cur_char;
      unicode (loc + 1) buf s (offset + 1) s_len 
    end 
  | 'x' -> begin 
      Buffer.add_char buf cur_char ; 
      two_hex (loc + 1) buf s (offset + 1) s_len 
    end 
  | _ -> error ~loc (Invalid_escape_code cur_char)
and two_hex loc buf s offset s_len = 
  if offset + 1 >= s_len then 
    error ~loc Invalid_hex_escape;
  (*Location.raise_errorf ~loc "\\x need at least two chars";*)
  let a, b = s.[offset], s.[offset + 1] in 
  if Ext_char.valid_hex a && Ext_char.valid_hex b then 
    begin 
      Buffer.add_char buf a ; 
      Buffer.add_char buf b ; 
      check_and_transform (loc + 2) buf s (offset + 2) s_len 
    end
  else
    error ~loc Invalid_hex_escape
(*Location.raise_errorf ~loc "%c%c is not a valid hex code" a b*)

and unicode loc buf s offset s_len = 
  if offset + 3 >= s_len then 
    error ~loc Invalid_unicode_escape
  (*Location.raise_errorf ~loc "\\u need at least four chars"*)
  ;
  let a0,a1,a2,a3 = s.[offset], s.[offset+1], s.[offset+2], s.[offset+3] in
  if 
    Ext_char.valid_hex a0 &&
    Ext_char.valid_hex a1 &&
    Ext_char.valid_hex a2 &&
    Ext_char.valid_hex a3 then 
    begin 
      Buffer.add_char buf a0;
      Buffer.add_char buf a1;
      Buffer.add_char buf a2;
      Buffer.add_char buf a3;  
      check_and_transform (loc + 4) buf s  (offset + 4) s_len 
    end 
  else
    error ~loc Invalid_unicode_escape 
(*Location.raise_errorf ~loc "%c%c%c%c is not a valid unicode point"
  a0 a1 a2 a3 *)
(* http://www.2ality.com/2015/01/es6-strings.html
   console.log('\uD83D\uDE80'); (* ES6*)
   console.log('\u{1F680}');
*)   









let transform_test s =
  let s_len = String.length s in 
  let buf = Buffer.create (s_len * 2) in
  check_and_transform 0 buf s 0 s_len;
  Buffer.contents buf

let transform loc s = 
  let s_len = String.length s in 
  let buf = Buffer.create (s_len * 2) in
  try
    check_and_transform 0 buf s 0 s_len;
    Buffer.contents buf 
  with
    Error (offset, error)
    ->  Location.raise_errorf ~loc "Offset: %d, %a" offset pp_error error



end
module Ast_utf8_string_interp : sig 
#1 "ast_utf8_string_interp.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



 type kind =
  | String
  | Var of int * int (* int records its border length *)

type error = private
  | Invalid_code_point
  | Unterminated_backslash
  | Invalid_escape_code of char
  | Invalid_hex_escape
  | Invalid_unicode_escape
  | Unterminated_variable
  | Unmatched_paren
  | Invalid_syntax_of_var of string 

(** Note the position is about code point *)
type pos = { lnum : int ; offset : int ; byte_bol : int }

type segment = {
  start : pos;
  finish : pos ;
  kind : kind;
  content : string ;
} 

type segments = segment list  

type cxt = {
  mutable segment_start : pos ;
  buf : Buffer.t ;
  s_len : int ;
  mutable segments : segments;
  mutable pos_bol : int; (* record the abs position of current beginning line *)
  mutable byte_bol : int ; 
  mutable pos_lnum : int ; (* record the line number *)
}

type exn += Error of pos *  pos * error 

val empty_segment : segment -> bool

val transform_test : string -> segment list
val transform_interp : Location.t -> string -> Parsetree.expression

end = struct
#1 "ast_utf8_string_interp.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type error = 
  | Invalid_code_point
  | Unterminated_backslash
  | Invalid_escape_code of char
  | Invalid_hex_escape
  | Invalid_unicode_escape
  | Unterminated_variable
  | Unmatched_paren
  | Invalid_syntax_of_var of string 

type kind =
  | String
  | Var of int * int 
(* [Var (loffset, roffset)] 
  For parens it used to be (2,-1)
  for non-parens it used to be (1,0)
*)

(** Note the position is about code point *)
type pos = { 
  lnum : int ; 
  offset : int ;
  byte_bol : int (* Note it actually needs to be in sync with OCaml's lexing semantics *)
}


type segment = {
  start : pos;
  finish : pos ;
  kind : kind;
  content : string ;
} 

type segments = segment list 


type cxt = {
  mutable segment_start : pos ;
  buf : Buffer.t ;
  s_len : int ;
  mutable segments : segments;
  mutable pos_bol : int; (* record the abs position of current beginning line *)
  mutable byte_bol : int ; 
  mutable pos_lnum : int ; (* record the line number *)
}


type exn += Error of pos *  pos * error 

let pp_error fmt err = 
  Format.pp_print_string fmt @@  match err with 
  | Invalid_code_point -> "Invalid code point"
  | Unterminated_backslash -> "\\ ended unexpectedly"
  | Invalid_escape_code c -> "Invalid escape code: " ^ String.make 1 c 
  | Invalid_hex_escape -> 
    "Invalid \\x escape"
  | Invalid_unicode_escape -> "Invalid \\u escape"
  | Unterminated_variable -> "$ unterminated"
  | Unmatched_paren -> "Unmatched paren"
  | Invalid_syntax_of_var s -> "`" ^s ^ "' is not a valid syntax of interpolated identifer"
let valid_lead_identifier_char x = 
  match x with
  | 'a'..'z' | '_' -> true
  | _ -> false

let valid_identifier_char x = 
  match x with
  | 'a'..'z' 
  | 'A'..'Z'
  | '0'..'9'
  | '_' | '\''-> true
  | _ -> false
(** Invariant: [valid_lead_identifier] has to be [valid_identifier] *)

let valid_identifier s =
  let s_len = String.length s in 
  if s_len = 0 then false 
  else
    valid_lead_identifier_char s.[0] &&
    Ext_string.for_all_from s 1  valid_identifier_char

      
let is_space x = 
  match x with
  | ' ' | '\n' | '\t' -> true
  | _ -> false



(**
   FIXME: multiple line offset 
   if there is no line offset. Note {|{j||} border will never trigger a new line
*)
let update_position border 
    ({lnum ; offset;byte_bol } : pos)
    (pos : Lexing.position)= 
  if lnum = 0 then 
    {pos with pos_cnum = pos.pos_cnum + border + offset  }
    (** When no newline, the column number is [border + offset] *)
  else 
    {
      pos with 
      pos_lnum = pos.pos_lnum + lnum ;
      pos_bol = pos.pos_cnum + border + byte_bol;
      pos_cnum = pos.pos_cnum + border + byte_bol + offset;
      (** when newline, the column number is [offset] *)
    }  
let update border
    (start : pos) 
    (finish : pos) (loc : Location.t) : Location.t = 
  let start_pos = loc.loc_start in 
  { loc  with 
    loc_start = 
      update_position  border start start_pos;
    loc_end = 
      update_position border finish start_pos
  }


(** Note [Var] kind can not be mpty  *)
let empty_segment {content } =
  Ext_string.is_empty content



let update_newline ~byte_bol loc  cxt = 
  cxt.pos_lnum <- cxt.pos_lnum + 1 ; 
  cxt.pos_bol <- loc;
  cxt.byte_bol <- byte_bol  

let pos_error cxt ~loc error = 
  raise (Error 
           (cxt.segment_start,
            { lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ; byte_bol = cxt.byte_bol}, error))

let add_var_segment cxt loc loffset roffset = 
  let content =  Buffer.contents cxt.buf in
  Buffer.clear cxt.buf ;
  let next_loc = {
    lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ; 
    byte_bol = cxt.byte_bol } in
  if valid_identifier content then 
    begin 
      cxt.segments <- 
        { start = cxt.segment_start; 
          finish =  next_loc ;
          kind = Var (loffset, roffset); 
          content} :: cxt.segments ;
      cxt.segment_start <- next_loc
    end
  else pos_error cxt ~loc (Invalid_syntax_of_var content)

let add_str_segment cxt loc   =
  let content =  Buffer.contents cxt.buf in
  Buffer.clear cxt.buf ;
  let next_loc = {
    lnum = cxt.pos_lnum ; offset = loc - cxt.pos_bol ; 
    byte_bol = cxt.byte_bol } in
  cxt.segments <- 
    { start = cxt.segment_start; 
      finish =  next_loc ;
      kind = String; 
      content} :: cxt.segments ;
  cxt.segment_start <- next_loc


  


let rec check_and_transform (loc : int )  s byte_offset ({s_len; buf} as cxt : cxt) =
  if byte_offset = s_len then
    add_str_segment cxt loc 
  else 
    let current_char = s.[byte_offset] in 
    match Ext_utf8.classify current_char with 
    | Single 92 (* '\\' *) -> 
      escape_code (loc + 1)  s (byte_offset+1) cxt
    | Single 34 ->
      Buffer.add_string buf "\\\"";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 39 -> 
      Buffer.add_string buf "\\'";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 10 ->          

      Buffer.add_string buf "\\n";
      let loc = loc + 1 in 
      let byte_offset = byte_offset + 1 in 
      update_newline ~byte_bol:byte_offset loc cxt ; (* Note variable could not have new-line *)
      check_and_transform loc  s byte_offset cxt
    | Single 13 -> 
      Buffer.add_string buf "\\r";
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt
    | Single 36 -> (* $ *)
      add_str_segment cxt loc  ; 
      let offset = byte_offset + 1 in
      if offset >= s_len then
        pos_error ~loc cxt  Unterminated_variable
      else
        let cur_char = s.[offset] in
        if cur_char = '(' then
          expect_var_paren  (loc + 2)  s (offset + 1) cxt
        else
          expect_simple_var (loc + 1)  s offset cxt
    | Single _ -> 
      Buffer.add_char buf current_char;
      check_and_transform (loc + 1)  s (byte_offset + 1) cxt

    | Invalid 
    | Cont _ -> pos_error ~loc cxt Invalid_code_point
    | Leading (n,_) -> 
      let i' = Ext_utf8.next s ~remaining:n  byte_offset in
      if i' < 0 then 
        pos_error cxt ~loc Invalid_code_point
      else 
        begin 
          for k = byte_offset to i' do 
            Buffer.add_char buf s.[k]; 
          done;   
          check_and_transform (loc + 1 )  s (i' + 1) cxt
        end
(**Lets keep identifier simple, so that we could generating a function easier in the future
   for example
   let f = [%fn{| $x + $y = $x_add_y |}]
*)
and expect_simple_var  loc  s offset ({buf; s_len} as cxt) =
  let v = ref offset in
  (* prerr_endline @@ Ext_pervasives.dump (s, has_paren, (is_space s.[!v]), !v); *)
  if not (offset < s_len  && valid_lead_identifier_char s.[offset]) then 
    pos_error cxt ~loc (Invalid_syntax_of_var Ext_string.empty)
  else 
    begin 
      while !v < s_len && valid_identifier_char s.[!v]  do (* TODO*)
        let cur_char = s.[!v] in
        Buffer.add_char buf cur_char;
        incr v ;
      done;
      let added_length = !v - offset in
      let loc = added_length + loc in 
      add_var_segment cxt loc 1 0 ; 
      check_and_transform loc  s (added_length + offset) cxt
    end
and expect_var_paren  loc  s offset ({buf; s_len} as cxt) =
  let v = ref offset in
  (* prerr_endline @@ Ext_pervasives.dump (s, has_paren, (is_space s.[!v]), !v); *)
  while !v < s_len &&  s.[!v] <> ')' do 
    let cur_char = s.[!v] in
    Buffer.add_char buf cur_char;
    incr v ;
  done;
  let added_length = !v - offset in
  let loc = added_length +  1 + loc  in
  if !v < s_len && s.[!v] = ')' then
    begin 
      add_var_segment cxt loc 2 (-1) ; 
      check_and_transform loc  s (added_length + 1 + offset) cxt 
    end
  else
    pos_error cxt ~loc Unmatched_paren





(* we share the same escape sequence with js *)        
and escape_code loc  s offset ({ buf; s_len} as cxt) = 
  if offset >= s_len then 
    pos_error cxt ~loc Unterminated_backslash
  else
    Buffer.add_char buf '\\'; 
  let cur_char = s.[offset] in
  match cur_char with 
  | '\\'
  | 'b' 
  | 't' 
  | 'n' 
  | 'v'
  | 'f'
  | 'r' 
  | '0' 
  | '$'
    -> 
    begin 
      Buffer.add_char buf cur_char ;
      check_and_transform (loc + 1)  s (offset + 1) cxt
    end 
  | 'u' -> 
    begin 
      Buffer.add_char buf cur_char;
      unicode (loc + 1) s (offset + 1) cxt
    end 
  | 'x' -> begin 
      Buffer.add_char buf cur_char ; 
      two_hex (loc + 1)  s (offset + 1) cxt
    end 
  | _ -> pos_error cxt ~loc (Invalid_escape_code cur_char)
and two_hex loc  s offset ({buf ; s_len} as cxt) = 
  if offset + 1 >= s_len then 
    pos_error cxt ~loc Invalid_hex_escape;
  let a, b = s.[offset], s.[offset + 1] in 
  if Ext_char.valid_hex a && Ext_char.valid_hex b then 
    begin 
      Buffer.add_char buf a ; 
      Buffer.add_char buf b ; 
      check_and_transform (loc + 2)  s (offset + 2) cxt
    end
  else
    pos_error cxt ~loc Invalid_hex_escape


and unicode loc  s offset ({buf ; s_len} as cxt) = 
  if offset + 3 >= s_len then 
    pos_error cxt ~loc Invalid_unicode_escape
  ;
  let a0,a1,a2,a3 = s.[offset], s.[offset+1], s.[offset+2], s.[offset+3] in
  if 
    Ext_char.valid_hex a0 &&
    Ext_char.valid_hex a1 &&
    Ext_char.valid_hex a2 &&
    Ext_char.valid_hex a3 then 
    begin 
      Buffer.add_char buf a0;
      Buffer.add_char buf a1;
      Buffer.add_char buf a2;
      Buffer.add_char buf a3;  
      check_and_transform (loc + 4) s  (offset + 4) cxt
    end 
  else
    pos_error cxt ~loc Invalid_unicode_escape 
let transform_test s =
  let s_len = String.length s in
  let buf = Buffer.create (s_len * 2) in
  let cxt = 
    { segment_start = {lnum = 0; offset = 0; byte_bol = 0}; 
      buf ;
      s_len;
      segments = [];
      pos_lnum = 0;          
      byte_bol = 0;
      pos_bol = 0;

    } in 
  check_and_transform 0 s 0 cxt;
  List.rev cxt.segments


(** TODO: test empty var $() $ failure, 
    Allow identifers x.A.y *)    

open Ast_helper     

(** Longident.parse "Pervasives.^" *)
let concat_ident  : Longident.t = 
  Ldot (Lident "Pervasives", "^") (* FIXME: remove deps on `Pervasives` *)
   (* JS string concatMany *)
    (* Ldot (Ldot (Lident "Js", "String"), "concat") *)

(* Longident.parse "Js.String.make"     *)
let to_string_ident : Longident.t = 
    Ldot (Ldot (Lident "Js", "String"), "make")



let escaped = Some Literals.escaped_j_delimiter 

let concat_exp 
  (a : Parsetree.expression)
  (b : Parsetree.expression) : Parsetree.expression = 
  let loc = Bs_loc.merge a.pexp_loc b.pexp_loc in 
  Exp.apply ~loc 
  (Exp.ident { txt =concat_ident; loc})
    ["",a ;
     "",b]

let border = String.length "{j|"

let aux loc (segment : segment) =  
  match segment with 
  | {start ; finish; kind ; content} 
    ->     
    begin match kind with 
      | String ->         
        let loc = update border start finish  loc  in 
        Exp.constant 
          ~loc
          (Const_string (content, escaped)) 
      | Var (soffset, foffset) ->
        let loc = {
          loc with 
          loc_start = update_position  (soffset + border) start loc.loc_start ;
          loc_end = update_position (foffset + border) finish loc.loc_start
        } in 
        Exp.apply ~loc 
          (Exp.ident ~loc {loc ; txt = to_string_ident })
          [
            "",
            Exp.ident ~loc {loc ; txt = Lident content}
          ]
    end 


let transform_interp loc s = 
  let s_len = String.length s in 
  let buf = Buffer.create (s_len * 2 ) in 
  try 
    let cxt : cxt = 
      { segment_start = {lnum = 0; offset = 0; byte_bol = 0}; 
        buf ;
        s_len;
        segments = [];
        pos_lnum = 0;          
        byte_bol = 0;
        pos_bol = 0;

      } in 

    check_and_transform 0 s 0 cxt; 
    let rev_segments =  cxt.segments in 
    match rev_segments with 
    | [] -> 
      Exp.constant ~loc 
        (Const_string ("", Some Literals.escaped_j_delimiter)) 
    | [ segment] -> 
      aux loc segment 
    | a::rest -> 
      List.fold_left (fun (acc : Parsetree.expression)
       (x : segment) ->
          concat_exp (aux loc x) acc )
        (aux loc a) rest
  with 
    Error (start,pos, error) 
    -> 
    Location.raise_errorf ~loc:(update border start pos loc )
      "%a"  pp_error error 

end
module Ppx_entry : sig 
#1 "ppx_entry.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)




val rewrite_signature :   
  (Parsetree.signature -> Parsetree.signature) ref

val rewrite_implementation : 
  (Parsetree.structure -> Parsetree.structure) ref





(* object 
    for setter : we can push more into [Lsend] and enclose it with a unit type

    for getter :

    (* Invariant: we expect the typechecker & lambda emitter  
       will not do agressive inlining
       Worst things could happen
    {[
      let x = y## case 3  in 
      x 2
    ]}
       in normal case, it should be compiled into Lambda
    {[
      let x = Lsend(y,case, [3]) in 
      Lapp(x,2)
    ]}

       worst:
    {[ Lsend(y, case, [3,2])
    ]}               
       for setter(include case setter), this could 
       be prevented by type system, for getter.

       solution: we can prevent this by rewrite into 
    {[
      Fn.run1  (!x# case) v 
      ]}
       *)

      *)

end = struct
#1 "ppx_entry.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)






(* When we design a ppx, we should keep it simple, and also think about
   how it would work with other tools like merlin and ocamldep  *)

(**
   1. extension point
   {[
     [%bs.raw{| blabla |}]
   ]}
   will be desugared into
   {[
     let module Js =
     struct unsafe_js : string -> 'a end
     in Js.unsafe_js {| blabla |}
   ]}
   The major benefit is to better error reporting (with locations).
   Otherwise

   {[

     let f u = Js.unsafe_js u
     let _ = f (1 + 2)
   ]}
   And if it is inlined some where
*)



open Ast_helper




let record_as_js_object = ref false (* otherwise has an attribute *)
let no_export = ref false

let () =
  Ast_derive_projector.init ();
  Ast_derive_js_mapper.init ()

let reset () =
  record_as_js_object := false ;
  no_export  :=  false

let rec unsafe_mapper : Bs_ast_mapper.mapper =
  { Bs_ast_mapper.default_mapper with
    expr = (fun self ({ pexp_loc = loc } as e) ->
        match e.pexp_desc with
        (** Its output should not be rewritten anymore *)
        | Pexp_extension extension ->
          Ast_exp_extension.handle_extension record_as_js_object e self extension
        | Pexp_constant (Const_string (s, (Some delim)))
          ->
          if Ext_string.equal delim Literals.unescaped_js_delimiter then
            let js_str = Ast_utf8_string.transform loc s in
            { e with pexp_desc =
                       Pexp_constant (Const_string (js_str, Some Literals.escaped_j_delimiter))}
          else if Ext_string.equal delim Literals.unescaped_j_delimiter then
            Ast_utf8_string_interp.transform_interp loc s
          else e
        (** End rewriting *)
        | Pexp_function cases ->
          (* {[ function [@bs.exn]
                | Not_found -> 0
                | Invalid_argument -> 1
              ]}*)
          begin match
              Ast_attributes.process_pexp_fun_attributes_rev e.pexp_attributes
            with
            | `Nothing, _ ->
              Bs_ast_mapper.default_mapper.expr self  e
            | `Exn, pexp_attributes ->
              Ast_util.convertBsErrorFunction loc self  pexp_attributes cases
          end
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_attributes.process_attributes_rev e.pexp_attributes with
            | `Nothing, _
              -> Bs_ast_mapper.default_mapper.expr self e
            |   `Uncurry, pexp_attributes
              ->
              {e with
               pexp_desc = Ast_util.to_uncurry_fn loc self pat body  ;
               pexp_attributes}
            | `Method , _
              ->  Location.raise_errorf ~loc "bs.meth is not supported in function expression"
            | `Meth_callback , pexp_attributes
              ->
              {e with pexp_desc = Ast_util.to_method_callback loc  self pat body ;
                      pexp_attributes }
          end
        | Pexp_apply (fn, args  ) ->
          Ast_exp_apply.handle_exp_apply e self fn args
        | Pexp_record (label_exprs, opt_exp)  ->
          if !record_as_js_object then
            (match opt_exp with
             | None ->
               { e with
                 pexp_desc =
                   Ast_util.record_as_js_object loc self label_exprs;
               }
             | Some e ->
               Location.raise_errorf
                 ~loc:e.pexp_loc "`with` construct is not supported in bs.obj ")
          else
            (* could be supported using `Object.assign`?
               type
               {[
                 external update : 'a Js.t -> 'b Js.t -> 'a Js.t = ""
                 constraint 'b :> 'a
               ]}
            *)
            Bs_ast_mapper.default_mapper.expr  self e
        | Pexp_object {pcstr_self;  pcstr_fields} ->
          begin match Ast_attributes.process_bs e.pexp_attributes with
            | `Has, pexp_attributes
              ->
              {e with
               pexp_desc =
                 Ast_util.ocaml_obj_as_js_object
                   loc self pcstr_self pcstr_fields;
               pexp_attributes
              }
            | `Nothing , _ ->
              Bs_ast_mapper.default_mapper.expr  self e
          end
        | _ ->  Bs_ast_mapper.default_mapper.expr self e
      );
    typ = (fun self typ ->
        Ast_core_type_class_type.handle_core_type self typ record_as_js_object);
    class_type =
      (fun self ({pcty_attributes; pcty_loc} as ctd) ->
         match Ast_attributes.process_bs pcty_attributes with
         | `Nothing,  _ ->
           Bs_ast_mapper.default_mapper.class_type self ctd
         | `Has, pcty_attributes ->
             (match ctd.pcty_desc with
             | Pcty_signature ({pcsig_self; pcsig_fields })
               ->
               let pcsig_self = self.typ self pcsig_self in
               {ctd with
                pcty_desc = Pcty_signature {
                    pcsig_self ;
                    pcsig_fields = Ast_core_type_class_type.handle_class_type_fields self pcsig_fields
                  };
                pcty_attributes
               }

             | Pcty_constr _
             | Pcty_extension _
             | Pcty_arrow _ ->
               Location.raise_errorf ~loc:pcty_loc "invalid or unused attribute `bs`")
               (* {[class x : int -> object
                    end [@bs]
                  ]}
                  Actually this is not going to happpen as below is an invalid syntax
                  {[class type x = int -> object
                      end[@bs]]}
               *)
      );
    signature_item =  begin fun
      (self : Bs_ast_mapper.mapper)
      (sigi : Parsetree.signature_item) ->
      match sigi.psig_desc with
      | Psig_type (_ :: _ as tdcls) ->
          Ast_tdcls.handleTdclsInSigi self sigi tdcls
      | Psig_value prim
        when Ast_attributes.process_external prim.pval_attributes
        ->
          Ast_primitive.handlePrimitiveInSig self prim sigi
      | _ -> Bs_ast_mapper.default_mapper.signature_item self sigi
    end;
    pat = begin fun self (pat : Parsetree.pattern) ->
      match pat with
      | { ppat_desc = Ppat_constant(Const_string (_, Some "j")); ppat_loc = loc} ->
        Location.raise_errorf ~loc  "Unicode string is not allowed in pattern match"
      | _  -> Bs_ast_mapper.default_mapper.pat self pat

    end;
    value_bindings = Ast_tuple_pattern_flatten.handle_value_bindings;
    structure_item = begin fun self (str : Parsetree.structure_item) ->
      begin match str.pstr_desc with
        | Pstr_extension ( ({txt = ("bs.raw"| "raw") ; loc}, payload), _attrs)
          ->
          Ast_util.handle_raw_structure loc payload
        | Pstr_extension (({txt = ("bs.debugger.chrome" | "debugger.chrome") ;loc}, payload),_)
          ->          
          if !Js_config.debug then 
            let open Ast_helper in 
            Str.eval ~loc (Exp.apply ~loc 
            (Exp.ident ~loc {txt = Ldot(Ldot (Lident"Belt","Debug"), "setupChromeDebugger");loc} )
            ["", Ast_literal.val_unit ~loc ()]
             )
          else Ast_structure.dummy_item loc
        | Pstr_type (_ :: _ as tdcls ) (* [ {ptype_attributes} as tdcl ] *)->
          Ast_tdcls.handleTdclsInStru self str tdcls
        | Pstr_primitive prim
          when Ast_attributes.process_external prim.pval_attributes
          ->
          Ast_primitive.handlePrimitiveInStru self prim str
        | _ -> Bs_ast_mapper.default_mapper.structure_item self str
      end
    end
  }




(** global configurations below *)
let common_actions_table :
  (string *  (Parsetree.expression option -> unit)) list =
  [
  ]


let structural_config_table  =
  String_map.of_list
    (( "no_export" ,
       (fun x ->
          no_export := (
            match x with
            |Some e -> Ast_payload.assert_bool_lit e
            | None -> true)
       ))
     :: common_actions_table)

let signature_config_table :
  (Parsetree.expression option -> unit) String_map.t=
  String_map.of_list common_actions_table

let dummy_unused_attribute : Warnings.t = (Bs_unused_attribute "")

let rewrite_signature :
  (Parsetree.signature  -> Parsetree.signature) ref =
  ref (fun  x ->
      let result =
        match (x : Parsetree.signature) with
        | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest
          ->
          begin
            Ast_payload.ident_or_record_as_config loc payload
            |> List.iter (Ast_payload.table_dispatch signature_config_table) ;
            unsafe_mapper.signature unsafe_mapper rest
          end
        | _ ->
          unsafe_mapper.signature  unsafe_mapper x in
      reset ();
      (* Keep this check, since the check is not inexpensive*)
      if Warnings.is_active dummy_unused_attribute then
        Bs_ast_invariant.emit_external_warnings.signature Bs_ast_invariant.emit_external_warnings result ;
      result
    )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref =
  ref (fun (x : Parsetree.structure) ->
      let result =
        match x with
        | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest
          ->
          begin
            Ast_payload.ident_or_record_as_config loc payload
            |> List.iter (Ast_payload.table_dispatch structural_config_table) ;
            let rest = unsafe_mapper.structure unsafe_mapper rest in
            if !no_export then
              [Str.include_ ~loc
                 (Incl.mk ~loc
                    (Mod.constraint_ ~loc
                       (Mod.structure ~loc rest  )
                       (Mty.signature ~loc [])
                    ))]
            else rest
          end
        | _ ->
          unsafe_mapper.structure  unsafe_mapper x  in
      reset ();
      (* Keep this check since it is not inexpensive*)
      (if Warnings.is_active dummy_unused_attribute then
         Bs_ast_invariant.emit_external_warnings.structure Bs_ast_invariant.emit_external_warnings result);
      result
    )


end
module Bsppx_main
= struct
#1 "bsppx_main.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



let apply_lazy ~source ~target impl iface =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Bs_ast_mapper: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (impl (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc


let  () =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1)
        !Ppx_entry.rewrite_implementation
        !Ppx_entry.rewrite_signature
    else
      begin
        Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
          Sys.executable_name;
        exit 2
      end
  with exn ->
    begin
      Location.report_exception Format.err_formatter exn;
      exit 2
    end


end
