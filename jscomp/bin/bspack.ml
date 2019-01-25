module Arg_helper : sig 
#1 "arg_helper.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 OCamlPro SAS                                    *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Decipher command line arguments of the form
        <value> | <key>=<value>[,...]
    (as used for example for the specification of inlining parameters
    varying by simplification round).
*)

module Make (S : sig
  module Key : sig
    type t

    (** The textual representation of a key must not contain '=' or ','. *)
    val of_string : string -> t

    module Map : Map.S with type key = t
  end

  module Value : sig
    type t

    (** The textual representation of a value must not contain ','. *)
    val of_string : string -> t
  end
end) : sig
  type parsed

  val default : S.Value.t -> parsed

  val set_base_default : S.Value.t -> parsed -> parsed

  val add_base_override : S.Key.t -> S.Value.t -> parsed -> parsed

  val reset_base_overrides : parsed -> parsed

  val set_user_default : S.Value.t -> parsed -> parsed

  val add_user_override : S.Key.t -> S.Value.t -> parsed -> parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn

  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:S.Key.t -> parsed -> S.Value.t
end

end = struct
#1 "arg_helper.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2016 OCamlPro SAS                                    *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let fatal err =
  prerr_endline err;
  exit 2

module Make (S : sig
  module Key : sig
    type t
    val of_string : string -> t
    module Map : Map.S with type key = t
  end

  module Value : sig
    type t
    val of_string : string -> t
  end
end) = struct
  type parsed = {
    base_default : S.Value.t;
    base_override : S.Value.t S.Key.Map.t;
    user_default : S.Value.t option;
    user_override : S.Value.t S.Key.Map.t;
  }

  let default v =
    { base_default = v;
      base_override = S.Key.Map.empty;
      user_default = None;
      user_override = S.Key.Map.empty; }

  let set_base_default value t =
    { t with base_default = value }

  let add_base_override key value t =
    { t with base_override = S.Key.Map.add key value t.base_override }

  let reset_base_overrides t =
    { t with base_override = S.Key.Map.empty }

  let set_user_default value t =
    { t with user_default = Some value }

  let add_user_override key value t =
    { t with user_override = S.Key.Map.add key value t.user_override }

  exception Parse_failure of exn

  let parse_exn str ~update =
    (* Is the removal of empty chunks really relevant here? *)
    (* (It has been added to mimic the old Misc.String.split.) *)
    let values = String.split_on_char ',' str |> List.filter ((<>) "") in
    let parsed =
      List.fold_left (fun acc value ->
          match String.index value '=' with
          | exception Not_found ->
            begin match S.Value.of_string value with
            | value -> set_user_default value acc
            | exception exn -> raise (Parse_failure exn)
            end
          | equals ->
            let key_value_pair = value in
            let length = String.length key_value_pair in
            assert (equals >= 0 && equals < length);
            if equals = 0 then begin
              raise (Parse_failure (
                Failure "Missing key in argument specification"))
            end;
            let key =
              let key = String.sub key_value_pair 0 equals in
              try S.Key.of_string key
              with exn -> raise (Parse_failure exn)
            in
            let value =
              let value =
                String.sub key_value_pair (equals + 1) (length - equals - 1)
              in
              try S.Value.of_string value
              with exn -> raise (Parse_failure exn)
            in
            add_user_override key value acc)
        !update
        values
    in
    update := parsed

  let parse str help_text update =
    match parse_exn str ~update with
    | () -> ()
    | exception (Parse_failure exn) ->
      fatal (Printf.sprintf "%s: %s" (Printexc.to_string exn) help_text)

  type parse_result =
    | Ok
    | Parse_failed of exn

  let parse_no_error str update =
    match parse_exn str ~update with
    | () -> Ok
    | exception (Parse_failure exn) -> Parse_failed exn

  let get ~key parsed =
    match S.Key.Map.find key parsed.user_override with
    | value -> value
    | exception Not_found ->
      match parsed.user_default with
      | Some value -> value
      | None ->
        match S.Key.Map.find key parsed.base_override with
        | value -> value
        | exception Not_found -> parsed.base_default

end

end
module Config_whole_compiler : sig 
#1 "config_whole_compiler.mli"

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
val c_compiler: string
        (* The compiler to use for compiling C files *)
val c_output_obj: string
        (* Name of the option of the C compiler for specifying the output file *)
val ocamlc_cflags : string
        (* The flags ocamlc should pass to the C compiler *)
val ocamlc_cppflags : string
        (* The flags ocamlc should pass to the C preprocessor *)
val ocamlopt_cflags : string
        (* The flags ocamlopt should pass to the C compiler *)
val ocamlopt_cppflags : string
        (* The flags ocamlopt should pass to the C preprocessor *)
val bytecomp_c_libraries: string
        (* The C libraries to link with custom runtimes *)
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
val stack_safety_margin: int
        (* Size in words of the safety margin between the bottom of
           the stack and the stack pointer. This margin can be used by
           intermediate computations of some instructions, or the event
           handler. *)

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

val flexdll_dirs : string list
        (* Directories needed for the FlexDLL objects *)

val host : string
        (* Whether the compiler is a cross-compiler *)

val target : string
        (* Whether the compiler is a cross-compiler *)

val print_config : out_channel -> unit;;

val profiling : bool
        (* Whether profiling with gprof is supported on this platform *)

val flambda : bool
        (* Whether the compiler was configured for flambda *)

val spacetime : bool
        (* Whether the compiler was configured for Spacetime profiling *)
val enable_call_counts : bool
        (* Whether call counts are to be available when Spacetime profiling *)
val profinfo : bool
        (* Whether the compiler was configured for profiling *)
val profinfo_width : int
        (* How many bits are to be used in values' headers for profiling
           information *)
val libunwind_available : bool
        (* Whether the libunwind library is available on the target *)
val libunwind_link_flags : string
        (* Linker flags to use libunwind *)

val safe_string: bool
        (* Whether the compiler was configured with -force-safe-string;
           in that case, the -unsafe-string compile-time option is unavailable

           @since 4.05.0 *)
val default_safe_string: bool
        (* Whether the compiler was configured to use the -safe-string
           or -unsafe-string compile-time option by default.

           @since 4.06.0 *)
val flat_float_array : bool
        (* Whether the compiler and runtime automagically flatten float
           arrays *)
val windows_unicode: bool
        (* Whether Windows Unicode runtime is enabled *)
val afl_instrument : bool
        (* Whether afl-fuzz instrumentation is generated by default *)


end = struct
#1 "config_whole_compiler.ml"

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The main OCaml version string has moved to ../VERSION *)
let version = "4.06.2+BS"
let standard_library =
  Filename.concat (Filename.dirname Sys.executable_name)  "ocaml"
let standard_library_default = standard_library

let standard_runtime = "ocamlrun" (*dont care:path to ocamlrun*)
let ccomp_type = "cc"
let c_compiler = "gcc"
let c_output_obj = "-o "
let ocamlc_cflags = "-O2 -fno-strict-aliasing -fwrapv "
let ocamlc_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let ocamlopt_cflags = "-O2 -fno-strict-aliasing -fwrapv"
let ocamlopt_cppflags = "-D_FILE_OFFSET_BITS=64 -D_REENTRANT"
let bytecomp_c_libraries = "-lcurses -lpthread                  "
(* bytecomp_c_compiler and native_c_compiler have been supported for a
   long time and are retained for backwards compatibility.
   For programs that don't need compatibility with older OCaml releases
   the recommended approach is to use the constituent variables
   c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
*)
let bytecomp_c_compiler =
  "" 
let native_c_compiler =
  ""
let native_c_libraries = ""
let native_pack_linker = "ld -r -arch x86_64 -o\ "
let ranlib = "ranlib"
let ar = "ar"
let cc_profile = "-pg"
let mkdll = ""
let mkexe = "" 
let mkmaindll = ""

let profiling = true
let flambda = false
let safe_string = false
let default_safe_string = true
let windows_unicode = 0 != 0

let flat_float_array = true

let afl_instrument = false

let exec_magic_number = "Caml1999X011"
and cmi_magic_number = "Caml1999I022"
and cmo_magic_number = "Caml1999O022"
and cma_magic_number = "Caml1999A022"
and cmx_magic_number =
  (* if flambda then
    "Caml1999y022"
  else *)
    "Caml1999Y022"
and cmxa_magic_number =
  (* if flambda then
    "Caml1999z022"
  else *)
    "Caml1999Z022"
and ast_impl_magic_number = "Caml1999M022"
and ast_intf_magic_number = "Caml1999N022"
and cmxs_magic_number = "Caml1999D022"
    (* cmxs_magic_number is duplicated in otherlibs/dynlink/natdynlink.ml *)
and cmt_magic_number = "Caml1999T022"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)
let stack_safety_margin = 60

let architecture = "amd64"
let model = "default"
let system = "macosx"

let asm = "clang -arch x86_64 -Wno-trigraphs -c"
let asm_cfi_supported = true
let with_frame_pointers = false
let spacetime = false
let enable_call_counts = true
let libunwind_available = false
let libunwind_link_flags = ""
let profinfo = false
let profinfo_width = 0

let ext_exe = ""
let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"

let host = "x86_64-apple-darwin17.7.0"
let target = "x86_64-apple-darwin17.7.0"

let default_executable_name =
  ""

let systhread_supported = false;;

let flexdll_dirs = [];;

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  let p_int name valu = Printf.fprintf oc "%s: %d\n" name valu in
  let p_bool name valu = Printf.fprintf oc "%s: %B\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "standard_runtime" standard_runtime;
  p "ccomp_type" ccomp_type;
  p "c_compiler" c_compiler;
  p "ocamlc_cflags" ocamlc_cflags;
  p "ocamlc_cppflags" ocamlc_cppflags;
  p "ocamlopt_cflags" ocamlopt_cflags;
  p "ocamlopt_cppflags" ocamlopt_cppflags;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "native_c_compiler" native_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p "ranlib" ranlib;
  p "cc_profile" cc_profile;
  p "architecture" architecture;
  p "model" model;
  p_int "int_size" Sys.int_size;
  p_int "word_size" Sys.word_size;
  p "system" system;
  p "asm" asm;
  p_bool "asm_cfi_supported" asm_cfi_supported;
  p_bool "with_frame_pointers" with_frame_pointers;
  p "ext_exe" ext_exe;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  p "host" host;
  p "target" target;
  p_bool "profiling" profiling;
  p_bool "flambda" flambda;
  p_bool "spacetime" spacetime;
  p_bool "safe_string" safe_string;
  p_bool "default_safe_string" default_safe_string;
  p_bool "flat_float_array" flat_float_array;
  p_bool "afl_instrument" afl_instrument;
  p_bool "windows_unicode" windows_unicode;

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
module Misc : sig 
#1 "misc.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
val fatal_errorf: ('a, Format.formatter, unit, 'b) format4 -> 'a
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
val may: ('a -> unit) -> 'a option -> unit
val may_map: ('a -> 'b) -> 'a option -> 'b option

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a
(** [protect_refs l f] temporarily sets [r] to [v] for each [R (r, v)] in [l]
    while executing [f]. The previous contents of the references is restored
    even if [f] raises an exception. *)

module Stdlib : sig
  module List : sig
    type 'a t = 'a list

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** The lexicographic order supported by the provided order.
        There is no constraint on the relative lengths of the lists. *)

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** Returns [true] iff the given lists have the same length and content
        with respect to the given equality function. *)

    val filter_map : ('a -> 'b option) -> 'a t -> 'b t
    (** [filter_map f l] applies [f] to every element of [l], filters
        out the [None] elements and returns the list of the arguments of
        the [Some] elements. *)

    val some_if_all_elements_are_some : 'a option t -> 'a t option
    (** If all elements of the given list are [Some _] then [Some xs]
        is returned with the [xs] being the contents of those [Some]s, with
        order preserved.  Otherwise return [None]. *)

    val map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> ('c t * 'b t)
    (** [let r1, r2 = map2_prefix f l1 l2]
        If [l1] is of length n and [l2 = h2 @ t2] with h2 of length n,
        r1 is [List.map2 f l1 h1] and r2 is t2. *)

    val split_at : int -> 'a t -> 'a t * 'a t
    (** [split_at n l] returns the pair [before, after] where [before] is
        the [n] first elements of [l] and [after] the remaining ones.
        If [l] has less than [n] elements, raises Invalid_argument. *)
  end

  module Option : sig
    type 'a t = 'a option

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val iter : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val value_default : ('a -> 'b) -> default:'b -> 'a t -> 'b
  end

  module Array : sig
    val exists2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
    (* Same as [Array.exists], but for a two-argument predicate. Raise
       Invalid_argument if the two arrays are determined to have
       different lengths. *)
  end
end

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
val output_to_file_via_temporary:
      ?mode:open_flag list -> string -> (string -> out_channel -> 'a) -> 'a
        (* Produce output in temporary file, then rename it
           (as atomically as possible) to the desired output file name.
           [output_to_file_via_temporary filename fn] opens a temporary file
           which is passed to [fn] (name + output channel).  When [fn] returns,
           the channel is closed and the temporary file is renamed to
           [filename]. *)

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
        (* [no_overflow_sub n1 n2] returns [true] if the computation of
           [n1 - n2] does not overflow. *)
val no_overflow_mul: int -> int -> bool
        (* [no_overflow_mul n1 n2] returns [true] if the computation of
           [n1 * n2] does not overflow. *)
val no_overflow_lsl: int -> int -> bool
        (* [no_overflow_lsl n k] returns [true] if the computation of
           [n lsl k] does not overflow. *)

module Int_literal_converter : sig
  val int : string -> int
  val int32 : string -> int32
  val int64 : string -> int64
  val nativeint : string -> nativeint
end

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
        (* [replace_substring ~before ~after str] replaces all
           occurrences of [before] with [after] in [str] and returns
           the resulting string. *)

val rev_split_words: string -> string list
        (* [rev_split_words s] splits [s] in blank-separated words, and returns
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

val spellcheck : string list -> string -> string list
(** [spellcheck env name] takes a list of names [env] that exist in
    the current environment and an erroneous [name], and returns a
    list of suggestions taken from [env], that are close enough to
    [name] that it may be a typo for one of them. *)

val did_you_mean : Format.formatter -> (unit -> string list) -> unit
(** [did_you_mean ppf get_choices] hints that the user may have meant
    one of the option returned by calling [get_choices]. It does nothing
    if the returned list is empty.

    The [unit -> ...] thunking is meant to delay any potentially-slow
    computation (typically computing edit-distance with many things
    from the current environment) to when the hint message is to be
    printed. You should print an understandable error message before
    calling [did_you_mean], so that users get a clear notification of
    the failure even if producing the hint is slow.
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


module StringSet: Set.S with type elt = string
module StringMap: Map.S with type key = string
(* TODO: replace all custom instantiations of StringSet/StringMap in various
   compiler modules with this one. *)

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

  type setting = Auto | Always | Never

  val setup : setting option -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end

val normalise_eol : string -> string
(** [normalise_eol s] returns a fresh copy of [s] with any '\r' characters
   removed. Intended for pre-processing text which will subsequently be printed
   on a channel which performs EOL transformations (i.e. Windows) *)

val delete_eol_spaces : string -> string
(** [delete_eol_spaces s] returns a fresh copy of [s] with any end of
   line spaces removed. Intended to normalize the output of the
   toplevel for tests. *)



(** {1 Hook machinery}

    Hooks machinery:
   [add_hook name f] will register a function that will be called on the
    argument of a later call to [apply_hooks]. Hooks are applied in the
    lexicographical order of their names.
*)

type hook_info = {
  sourcefile : string;
}

exception HookExnWrapper of
    {
      error: exn;
      hook_name: string;
      hook_info: hook_info;
    }
    (** An exception raised by a hook will be wrapped into a
        [HookExnWrapper] constructor by the hook machinery.  *)


val raise_direct_hook_exn: exn -> 'a
  (** A hook can use [raise_unwrapped_hook_exn] to raise an exception that will
      not be wrapped into a {!HookExnWrapper}. *)

module type HookSig = sig
  type t
  val add_hook : string -> (hook_info -> t -> t) -> unit
  val apply_hooks : hook_info -> t -> t
end

module MakeHooks : functor (M : sig type t end) -> HookSig with type t = M.t

end = struct
#1 "misc.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

let fatal_errorf fmt = Format.kasprintf fatal_error fmt

(* Exceptions *)

let try_finally work cleanup =
  let result = (try work () with e -> cleanup (); raise e) in
  cleanup ();
  result
;;

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    match f () with
    | x           -> set_refs backup; x
    | exception e -> set_refs backup; raise e

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

module Stdlib = struct
  module List = struct
    type 'a t = 'a list

    let rec compare cmp l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _::_ -> -1
      | _::_, [] -> 1
      | h1::t1, h2::t2 ->
        let c = cmp h1 h2 in
        if c <> 0 then c
        else compare cmp t1 t2

    let rec equal eq l1 l2 =
      match l1, l2 with
      | ([], []) -> true
      | (hd1 :: tl1, hd2 :: tl2) -> eq hd1 hd2 && equal eq tl1 tl2
      | (_, _) -> false

    let filter_map f l =
      let rec aux acc l =
        match l with
        | [] -> List.rev acc
        | h :: t ->
          match f h with
          | None -> aux acc t
          | Some v -> aux (v :: acc) t
      in
      aux [] l

    let map2_prefix f l1 l2 =
      let rec aux acc l1 l2 =
        match l1, l2 with
        | [], _ -> (List.rev acc, l2)
        | _ :: _, [] -> raise (Invalid_argument "map2_prefix")
        | h1::t1, h2::t2 ->
          let h = f h1 h2 in
          aux (h :: acc) t1 t2
      in
      aux [] l1 l2

    let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

    let split_at n l =
      let rec aux n acc l =
        if n = 0
        then List.rev acc, l
        else
          match l with
          | [] -> raise (Invalid_argument "split_at")
          | t::q -> aux (n-1) (t::acc) q
      in
      aux n [] l
  end

  module Option = struct
    type 'a t = 'a option

    let equal eq o1 o2 =
      match o1, o2 with
      | None, None -> true
      | Some e1, Some e2 -> eq e1 e2
      | _, _ -> false

    let iter f = function
      | Some x -> f x
      | None -> ()

    let map f = function
      | Some x -> Some (f x)
      | None -> None

    let fold f a b =
      match a with
      | None -> b
      | Some a -> f a b

    let value_default f ~default a =
      match a with
      | None -> default
      | Some a -> f a
  end

  module Array = struct
    let exists2 p a1 a2 =
      let n = Array.length a1 in
      if Array.length a2 <> n then invalid_arg "Misc.Stdlib.Array.exists2";
      let rec loop i =
        if i = n then false
        else if p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) then true
        else loop (succ i) in
      loop 0
  end
end

let may = Stdlib.Option.iter
let may_map = Stdlib.Option.map

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
  let uname = String.uncapitalize_ascii name in
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
    if Sys.file_exists filename
    then Sys.remove filename
  with Sys_error _msg ->
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

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let (temp_filename, oc) =
    Filename.open_temp_file
       ~mode ~perms:0o666 ~temp_dir:(Filename.dirname filename)
       (Filename.basename filename) ".tmp" in
    (* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res ->
      close_out oc;
      begin try
        Sys.rename temp_filename filename; res
      with exn ->
        remove_file temp_filename; raise exn
      end
  | exception exn ->
      close_out oc; remove_file temp_filename; raise exn

(* Integer operations *)

let rec log2 n =
  if n <= 1 then 0 else 1 + log2(n asr 1)

let align n a =
  if n >= 0 then (n + a - 1) land (-a) else n land (-a)

let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a+b))) < 0

let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a-b)) < 0

let no_overflow_mul a b = b <> 0 && (a * b) / b = a

let no_overflow_lsl a k =
  0 <= k && k < Sys.word_size && min_int asr k <= a && a <= max_int asr k

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0]= '-'
    then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s (~-) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
  let nativeint s = cvt_int_aux s Nativeint.neg Nativeint.of_string
end

(* String operations *)

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

let spellcheck env name =
  let cutoff =
    match String.length name with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
      | None -> acc
      | Some dist ->
         let (best_choice, best_dist) = acc in
         if dist < best_dist then ([head], dist)
         else if dist = best_dist then (head :: best_choice, dist)
         else acc
  in
  fst (List.fold_left (compare name) ([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
     let rest, last = split_last choices in
     Format.fprintf ppf "@\nHint: Did you mean %s%s%s?@?"
       (String.concat ", " rest)
       (if rest = [] then "" else " or ")
       last

let cut_at s c =
  let pos = String.index s c in
  String.sub s 0 pos, String.sub s (pos+1) (String.length s - pos - 1)


module StringSet = Set.Make(struct type t = string let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)

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

  (* either prints the tag of [s] or delegates to [or_else] *)
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
    pp_set_formatter_tag_functions ppf functions';
    (* also setup margins *)
    pp_set_margin ppf (pp_get_margin std_formatter());
    ()

  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb"
    && term <> ""
    && isatty stderr

  type setting = Auto | Always | Never

  let setup =
    let first = ref true in (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled := (match o with
            | Some Always -> true
            | Some Auto -> should_enable_color ()
            | Some Never -> false
            | None -> should_enable_color ())
      );
      ()
end

let normalise_eol s =
  let b = Buffer.create 80 in
    for i = 0 to String.length s - 1 do
      if s.[i] <> '\r' then Buffer.add_char b s.[i]
    done;
    Buffer.contents b

let delete_eol_spaces src =
  let len_src = String.length src in
  let dst = Bytes.create len_src in
  let rec loop i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces 1 (i_src + 1) i_dst
      | c ->
        Bytes.set dst i_dst c;
        loop (i_src + 1) (i_dst + 1)
  and loop_spaces spaces i_src i_dst =
    if i_src = len_src then
      i_dst
    else
      match src.[i_src] with
      | ' ' | '\t' ->
        loop_spaces (spaces + 1) (i_src + 1) i_dst
      | '\n' ->
        Bytes.set dst i_dst '\n';
        loop (i_src + 1) (i_dst + 1)
      | _ ->
        for n = 0 to spaces do
          Bytes.set dst (i_dst + n) src.[i_src - spaces + n]
        done;
        loop (i_src + 1) (i_dst + spaces + 1)
  in
  let stop = loop 0 0 in
  Bytes.sub_string dst 0 stop

type hook_info = {
  sourcefile : string;
}

exception HookExnWrapper of
    {
      error: exn;
      hook_name: string;
      hook_info: hook_info;
    }

exception HookExn of exn

let raise_direct_hook_exn e = raise (HookExn e)

let fold_hooks list hook_info ast =
  List.fold_left (fun ast (hook_name,f) ->
    try
      f hook_info ast
    with
    | HookExn e -> raise e
    | error -> raise (HookExnWrapper {error; hook_name; hook_info})
       (* when explicit reraise with backtrace will be available,
          it should be used here *)

  ) ast (List.sort compare list)

module type HookSig = sig
  type t

  val add_hook : string -> (hook_info -> t -> t) -> unit
  val apply_hooks : hook_info -> t -> t
end

module MakeHooks(M: sig
    type t
  end) : HookSig with type t = M.t
= struct

  type t = M.t

  let hooks = ref []
  let add_hook name f = hooks := (name, f) :: !hooks
  let apply_hooks sourcefile intf =
    fold_hooks !hooks sourcefile intf
end

end
module Identifiable : sig 
#1 "identifiable.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things. *)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type Set = sig
  module T : Set.OrderedType
  include Set.S
    with type elt = T.t
     and type t = Set.Make (T).t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type Map = sig
  module T : Map.OrderedType
  include Map.S
    with type key = T.t
     and type 'a t = 'a Map.Make (T).t

  val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val of_list : (key * 'a) list -> 'a t

  (** [disjoint_union m1 m2] contains all bindings from [m1] and
      [m2]. If some binding is present in both and the associated
      value is not equal, a Fatal_error is raised *)
  val disjoint_union : ?eq:('a -> 'a -> bool) -> ?print:(Format.formatter -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  (** [union_right m1 m2] contains all bindings from [m1] and [m2]. If
      some binding is present in both, the one from [m2] is taken *)
  val union_right : 'a t -> 'a t -> 'a t

  (** [union_left m1 m2 = union_right m2 m1] *)
  val union_left : 'a t -> 'a t -> 'a t

  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Set.Make(T).t
  val data : 'a t -> 'a list
  val of_set : (key -> 'a) -> Set.Make(T).t -> 'a t
  val transpose_keys_and_data : key t -> key t
  val transpose_keys_and_data_set : key t -> Set.Make(T).t t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Tbl = sig
  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S
    with type key = T.t
     and type 'a t = 'a Hashtbl.Make (T).t

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t

  val to_map : 'a t -> 'a Map.Make(T).t
  val of_map : 'a Map.Make(T).t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T
  module Tbl : Tbl with module T := T
end

module Make (T : Thing) : S with type t := T.t

end = struct
#1 "identifiable.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Set = sig
  module T : Set.OrderedType
  include Set.S
    with type elt = T.t
     and type t = Set.Make (T).t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type Map = sig
  module T : Map.OrderedType
  include Map.S
    with type key = T.t
     and type 'a t = 'a Map.Make (T).t

  val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val of_list : (key * 'a) list -> 'a t

  val disjoint_union : ?eq:('a -> 'a -> bool) -> ?print:(Format.formatter -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  val union_right : 'a t -> 'a t -> 'a t

  val union_left : 'a t -> 'a t -> 'a t

  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Set.Make(T).t
  val data : 'a t -> 'a list
  val of_set : (key -> 'a) -> Set.Make(T).t -> 'a t
  val transpose_keys_and_data : key t -> key t
  val transpose_keys_and_data_set : key t -> Set.Make(T).t t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type Tbl = sig
  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S
    with type key = T.t
     and type 'a t = 'a Hashtbl.Make (T).t

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t

  val to_map : 'a t -> 'a Map.Make(T).t
  val of_map : 'a Map.Make(T).t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t

  let compare (a1, b1) (a2, b2) =
    let c = A.compare a1 a2 in
    if c <> 0 then c
    else B.compare b1 b2

  let output oc (a, b) = Printf.fprintf oc " (%a, %a)" A.output a B.output b
  let hash (a, b) = Hashtbl.hash (A.hash a, B.hash b)
  let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2
  let print ppf (a, b) = Format.fprintf ppf " (%a, @ %a)" A.print a B.print b
end

module Make_map (T : Thing) = struct
  include Map.Make (T)

  let filter_map t ~f =
    fold (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map) t empty

  let of_list l =
    List.fold_left (fun map (id, v) -> add id v map) empty l

  let disjoint_union ?eq ?print m1 m2 =
    union (fun id v1 v2 ->
        let ok = match eq with
          | None -> false
          | Some eq -> eq v1 v2
        in
        if not ok then
          let err =
            match print with
            | None ->
              Format.asprintf "Map.disjoint_union %a" T.print id
            | Some print ->
              Format.asprintf "Map.disjoint_union %a => %a <> %a"
                T.print id print v1 print v2
          in
          Misc.fatal_error err
        else Some v1)
      m1 m2

  let union_right m1 m2 =
    merge (fun _id x y -> match x, y with
        | None, None -> None
        | None, Some v
        | Some v, None
        | Some _, Some v -> Some v)
      m1 m2

  let union_left m1 m2 = union_right m2 m1

  let union_merge f m1 m2 =
    let aux _ m1 m2 =
      match m1, m2 with
      | None, m | m, None -> m
      | Some m1, Some m2 -> Some (f m1 m2)
    in
    merge aux m1 m2

  let rename m v =
    try find v m
    with Not_found -> v

  let map_keys f m =
    of_list (List.map (fun (k, v) -> f k, v) (bindings m))

  let print f ppf s =
    let elts ppf s = iter (fun id v ->
        Format.fprintf ppf "@ (@[%a@ %a@])" T.print id f v) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  module T_set = Set.Make (T)

  let keys map = fold (fun k _ set -> T_set.add k set) map T_set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = T_set.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty
  let transpose_keys_and_data_set map =
    fold (fun k v m ->
        let set =
          match find v m with
          | exception Not_found ->
            T_set.singleton k
          | set ->
            T_set.add k set
        in
        add v set m)
      map empty
end

module Make_set (T : Thing) = struct
  include Set.Make (T)

  let output oc s =
    Printf.fprintf oc " ( ";
    iter (fun v -> Printf.fprintf oc "%a " T.output v) s;
    Printf.fprintf oc ")"

  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))
end

module Make_tbl (T : Thing) = struct
  include Hashtbl.Make (T)

  module T_map = Make_map (T)

  let to_list t =
    fold (fun key datum elts -> (key, datum)::elts) t []

  let of_list elts =
    let t = create 42 in
    List.iter (fun (key, datum) -> add t key datum) elts;
    t

  let to_map v = fold T_map.add v T_map.empty

  let of_map m =
    let t = create (T_map.cardinal m) in
    T_map.iter (fun k v -> add t k v) m;
    t

  let memoize t f = fun key ->
    try find t key with
    | Not_found ->
      let r = f key in
      add t key r;
      r

  let map t f =
    of_map (T_map.map f (to_map t))
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T
  module Tbl : Tbl with module T := T
end

module Make (T : Thing) = struct
  module T = T
  include T

  module Set = Make_set (T)
  module Map = Make_map (T)
  module Tbl = Make_tbl (T)
end

end
module Numbers : sig 
#1 "numbers.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Modules about numbers, some of which satisfy {!Identifiable.S}. *)

module Int : sig
  include Identifiable.S with type t = int

  (** [zero_to_n n] is the set of numbers \{0, ..., n\} (inclusive). *)
  val zero_to_n : int -> Set.t
end

module Int8 : sig
  type t

  val zero : t
  val one : t

  val of_int_exn : int -> t
  val to_int : t -> int
end

module Int16 : sig
  type t

  val of_int_exn : int -> t
  val of_int64_exn : Int64.t -> t

  val to_int : t -> int
end

module Float : Identifiable.S with type t = float

end = struct
#1 "numbers.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int_base = Identifiable.Make (struct
  type t = int

  let compare x y = x - y
  let output oc x = Printf.fprintf oc "%i" x
  let hash i = i
  let equal (i : int) j = i = j
  let print = Format.pp_print_int
end)

module Int = struct
  type t = int

  include Int_base

  let rec zero_to_n n =
    if n < 0 then Set.empty else Set.add n (zero_to_n (n-1))
end

module Int8 = struct
  type t = int

  let zero = 0
  let one = 1

  let of_int_exn i =
    if i < -(1 lsl 7) || i > ((1 lsl 7) - 1) then
      Misc.fatal_errorf "Int8.of_int_exn: %d is out of range" i
    else
      i

  let to_int i = i
end

module Int16 = struct
  type t = int

  let of_int_exn i =
    if i < -(1 lsl 15) || i > ((1 lsl 15) - 1) then
      Misc.fatal_errorf "Int16.of_int_exn: %d is out of range" i
    else
      i

  let lower_int64 = Int64.neg (Int64.shift_left Int64.one 15)
  let upper_int64 = Int64.sub (Int64.shift_left Int64.one 15) Int64.one

  let of_int64_exn i =
    if Int64.compare i lower_int64 < 0
        || Int64.compare i upper_int64 > 0
    then
      Misc.fatal_errorf "Int16.of_int64_exn: %Ld is out of range" i
    else
      Int64.to_int i

  let to_int t = t
end

module Float = struct
  type t = float

  include Identifiable.Make (struct
    type t = float

    let compare x y = Pervasives.compare x y
    let output oc x = Printf.fprintf oc "%f" x
    let hash f = Hashtbl.hash f
    let equal (i : float) j = i = j
    let print = Format.pp_print_float
  end)
end

end
module Profile : sig 
#1 "profile.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compiler performance recording *)

type file = string

val reset : unit -> unit
(** erase all recorded profile information *)

val record_call : ?accumulate:bool -> string -> (unit -> 'a) -> 'a
(** [record_call pass f] calls [f] and records its profile information. *)

val record : ?accumulate:bool -> string -> ('a -> 'b) -> 'a -> 'b
(** [record pass f arg] records the profile information of [f arg] *)

type column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap ]

val print : Format.formatter -> column list -> unit
(** Prints the selected recorded profiling information to the formatter. *)

(** Command line flags *)

val options_doc : string
val all_columns : column list

(** A few pass names that are needed in several places, and shared to
    avoid typos. *)

val generate : string
val transl : string
val typing : string

end = struct
#1 "profile.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-18-40-42-48"]

type file = string

external time_include_children: bool -> float = "caml_sys_time_include_children"
let cpu_time () = time_include_children true

module Measure = struct
  type t = {
    time : float;
    allocated_words : float;
    top_heap_words : int;
  }
  let create () =
    let stat = Gc.quick_stat () in
    {
      time = cpu_time ();
      allocated_words = stat.minor_words +. stat.major_words;
      top_heap_words = stat.top_heap_words;
    }
  let zero = { time = 0.; allocated_words = 0.; top_heap_words = 0 }
end

module Measure_diff = struct
  let timestamp = let r = ref (-1) in fun () -> incr r; !r
  type t = {
    timestamp : int;
    duration : float;
    allocated_words : float;
    top_heap_words_increase : int;
  }
  let zero () = {
    timestamp = timestamp ();
    duration = 0.;
    allocated_words = 0.;
    top_heap_words_increase = 0;
  }
  let accumulate t (m1 : Measure.t) (m2 : Measure.t) = {
    timestamp = t.timestamp;
    duration = t.duration +. (m2.time -. m1.time);
    allocated_words =
      t.allocated_words +. (m2.allocated_words -. m1.allocated_words);
    top_heap_words_increase =
      t.top_heap_words_increase + (m2.top_heap_words - m1.top_heap_words);
  }
  let of_diff m1 m2 =
    accumulate (zero ()) m1 m2
end

type hierarchy =
  | E of (string, Measure_diff.t * hierarchy) Hashtbl.t
[@@unboxed]

let create () = E (Hashtbl.create 2)
let hierarchy = ref (create ())
let initial_measure = ref None
let reset () = hierarchy := create (); initial_measure := None

let record_call ?(accumulate = false) name f =
  let E prev_hierarchy = !hierarchy in
  let start_measure = Measure.create () in
  if !initial_measure = None then initial_measure := Some start_measure;
  let this_measure_diff, this_table =
    (* We allow the recording of multiple categories by the same name, for tools
       like ocamldoc that use the compiler libs but don't care about profile
       information, and so may record, say, "parsing" multiple times. *)
    if accumulate
    then
      match Hashtbl.find prev_hierarchy name with
      | exception Not_found -> Measure_diff.zero (), Hashtbl.create 2
      | measure_diff, E table ->
        Hashtbl.remove prev_hierarchy name;
        measure_diff, table
    else Measure_diff.zero (), Hashtbl.create 2
  in
  hierarchy := E this_table;
  Misc.try_finally f
    (fun () ->
       hierarchy := E prev_hierarchy;
       let end_measure = Measure.create () in
       let measure_diff =
         Measure_diff.accumulate this_measure_diff start_measure end_measure in
       Hashtbl.add prev_hierarchy name (measure_diff, E this_table))

let record ?accumulate pass f x = record_call ?accumulate pass (fun () -> f x)

type display = {
  to_string : max:float -> width:int -> string;
  worth_displaying : max:float -> bool;
}

let time_display v : display =
  (* Because indentation is meaningful, and because the durations are
     the first element of each row, we can't pad them with spaces. *)
  let to_string_without_unit v ~width = Printf.sprintf "%0*.03f" width v in
  let to_string ~max:_ ~width =
    to_string_without_unit v ~width:(width - 1) ^ "s" in
  let worth_displaying ~max:_ =
    float_of_string (to_string_without_unit v ~width:0) <> 0. in
  { to_string; worth_displaying }

let memory_word_display =
  (* To make memory numbers easily comparable across rows, we choose a single
     scale for an entire column. To keep the display compact and not overly
     precise (no one cares about the exact number of bytes), we pick the largest
     scale we can and we only show 3 digits. Avoiding showing tiny numbers also
     allows us to avoid displaying passes that barely allocate compared to the
     rest of the compiler.  *)
  let bytes_of_words words = words *. float_of_int (Sys.word_size / 8) in
  let to_string_without_unit v ~width scale =
    let precision = 3 and precision_power = 1e3 in
    let v_rescaled = bytes_of_words v /. scale in
    let v_rounded =
      floor (v_rescaled *. precision_power +. 0.5) /. precision_power in
    let v_str = Printf.sprintf "%.*f" precision v_rounded in
    let index_of_dot = String.index v_str '.' in
    let v_str_truncated =
      String.sub v_str 0
        (if index_of_dot >= precision
         then index_of_dot
         else precision + 1)
    in
    Printf.sprintf "%*s" width v_str_truncated
  in
  let choose_memory_scale =
    let units = [|"B"; "kB"; "MB"; "GB"|] in
    fun words ->
      let bytes = bytes_of_words words in
      let scale = ref (Array.length units - 1) in
      while !scale > 0 && bytes < 1024. ** float_of_int !scale do
        decr scale
      done;
      1024. ** float_of_int !scale, units.(!scale)
  in
  fun ?previous v : display ->
    let to_string ~max ~width =
      let scale, scale_str = choose_memory_scale max in
      let width = width - String.length scale_str in
      to_string_without_unit v ~width scale ^ scale_str
    in
    let worth_displaying ~max =
      let scale, _ = choose_memory_scale max in
      float_of_string (to_string_without_unit v ~width:0 scale) <> 0.
      && match previous with
      | None -> true
      | Some p ->
         (* This branch is for numbers that represent absolute quantity, rather
            than differences. It allows us to skip displaying the same absolute
            quantity many times in a row. *)
         to_string_without_unit p ~width:0 scale
         <> to_string_without_unit v ~width:0 scale
    in
    { to_string; worth_displaying }

let profile_list (E table) =
  let l = Hashtbl.fold (fun k d l -> (k, d) :: l) table [] in
  List.sort (fun (_, (p1, _)) (_, (p2, _)) ->
    compare p1.Measure_diff.timestamp p2.Measure_diff.timestamp) l

let compute_other_category (E table : hierarchy) (total : Measure_diff.t) =
  let r = ref total in
  Hashtbl.iter (fun _pass ((p2 : Measure_diff.t), _) ->
    let p1 = !r in
    r := {
      timestamp = p1.timestamp;
      duration = p1.duration -. p2.duration;
      allocated_words = p1.allocated_words -. p2.allocated_words;
      top_heap_words_increase =
        p1.top_heap_words_increase - p2.top_heap_words_increase;
    }
  ) table;
  !r

type row = R of string * (float * display) list * row list
type column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap ]

let rec rows_of_hierarchy ~nesting make_row name measure_diff hierarchy env =
  let rows =
    rows_of_hierarchy_list
      ~nesting:(nesting + 1) make_row hierarchy measure_diff env in
  let values, env =
    make_row env measure_diff ~toplevel_other:(nesting = 0 && name = "other") in
  R (name, values, rows), env

and rows_of_hierarchy_list ~nesting make_row hierarchy total env =
  let list = profile_list hierarchy in
  let list =
    if list <> [] || nesting = 0
    then list @ [ "other", (compute_other_category hierarchy total, create ()) ]
    else []
  in
  let env = ref env in
  List.map (fun (name, (measure_diff, hierarchy)) ->
    let a, env' =
      rows_of_hierarchy ~nesting make_row name measure_diff hierarchy !env in
    env := env';
    a
  ) list

let rows_of_hierarchy hierarchy measure_diff initial_measure columns =
  (* Computing top heap size is a bit complicated: if the compiler applies a
     list of passes n times (rather than applying pass1 n times, then pass2 n
     times etc), we only show one row for that pass but what does "top heap
     size at the end of that pass" even mean?
     It seems the only sensible answer is to pretend the compiler applied pass1
     n times, pass2 n times by accumulating all the heap size increases that
     happened during each pass, and then compute what the heap size would have
     been. So that's what we do.
     There's a bit of extra complication, which is that the heap can increase in
     between measurements. So the heap sizes can be a bit off until the "other"
     rows account for what's missing. We special case the toplevel "other" row
     so that any increases that happened before the start of the compilation is
     correctly reported, as a lot of code may run before the start of the
     compilation (eg functor applications). *)
    let make_row prev_top_heap_words (p : Measure_diff.t) ~toplevel_other =
      let top_heap_words =
        prev_top_heap_words
        + p.top_heap_words_increase
        - if toplevel_other
          then initial_measure.Measure.top_heap_words
          else 0
      in
      let make value ~f = value, f value in
      List.map (function
        | `Time ->
          make p.duration ~f:time_display
        | `Alloc ->
          make p.allocated_words ~f:memory_word_display
        | `Top_heap ->
          make (float_of_int p.top_heap_words_increase) ~f:memory_word_display
        | `Abs_top_heap ->
          make (float_of_int top_heap_words)
           ~f:(memory_word_display ~previous:(float_of_int prev_top_heap_words))
      ) columns,
      top_heap_words
  in
  rows_of_hierarchy_list ~nesting:0 make_row hierarchy measure_diff
    initial_measure.top_heap_words

let max_by_column ~n_columns rows =
  let a = Array.make n_columns 0. in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i (v, _) -> a.(i) <- max a.(i) v) values;
    List.iter loop rows
  in
  List.iter loop rows;
  a

let width_by_column ~n_columns ~display_cell rows =
  let a = Array.make n_columns 1 in
  let rec loop (R (_, values, rows)) =
    List.iteri (fun i cell ->
      let _, str = display_cell i cell ~width:0 in
      a.(i) <- max a.(i) (String.length str)
    ) values;
    List.iter loop rows;
  in
  List.iter loop rows;
  a

let display_rows ppf rows =
  let n_columns =
    match rows with
    | [] -> 0
    | R (_, values, _) :: _ -> List.length values
  in
  let maxs = max_by_column ~n_columns rows in
  let display_cell i (_, c) ~width =
    let display_cell = c.worth_displaying ~max:maxs.(i) in
    display_cell, if display_cell
                  then c.to_string ~max:maxs.(i) ~width
                  else String.make width '-'
  in
  let widths = width_by_column ~n_columns ~display_cell rows in
  let rec loop (R (name, values, rows)) ~indentation =
    let worth_displaying, cell_strings =
      values
      |> List.mapi (fun i cell -> display_cell i cell ~width:widths.(i))
      |> List.split
    in
    if List.exists (fun b -> b) worth_displaying then
      Format.fprintf ppf "%s%s %s@\n"
        indentation (String.concat " " cell_strings) name;
    List.iter (loop ~indentation:("  " ^ indentation)) rows;
  in
  List.iter (loop ~indentation:"") rows

let print ppf columns =
  match columns with
  | [] -> ()
  | _ :: _ ->
     let initial_measure =
       match !initial_measure with
       | Some v -> v
       | None -> Measure.zero
     in
     let total = Measure_diff.of_diff Measure.zero (Measure.create ()) in
     display_rows ppf (rows_of_hierarchy !hierarchy total initial_measure columns)

let column_mapping = [
  "time", `Time;
  "alloc", `Alloc;
  "top-heap", `Top_heap;
  "absolute-top-heap", `Abs_top_heap;
]

let column_names = List.map fst column_mapping

let options_doc =
  Printf.sprintf
    " Print performance information for each pass\
   \n    The columns are: %s."
    (String.concat " " column_names)

let all_columns = List.map snd column_mapping

let generate = "generate"
let transl = "transl"
let typing = "typing"

end
module Clflags : sig 
#1 "clflags.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2005 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Command line flags *)

(** Optimization parameters represented as ints indexed by round number. *)
module Int_arg_helper : sig
  type parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn
  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:int -> parsed -> int
end

(** Optimization parameters represented as floats indexed by round number. *)
module Float_arg_helper : sig
  type parsed

  val parse : string -> string -> parsed ref -> unit

  type parse_result =
    | Ok
    | Parse_failed of exn
  val parse_no_error : string -> parsed ref -> parse_result

  val get : key:int -> parsed -> float
end

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  inline_branch_factor : float option;
  inline_max_depth : int option;
  inline_max_unroll : int option;
  inline_threshold : float option;
  inline_toplevel_threshold : int option;
}

val classic_arguments : inlining_arguments
val o1_arguments : inlining_arguments
val o2_arguments : inlining_arguments
val o3_arguments : inlining_arguments

(** Set all the inlining arguments for a round.
    The default is set if no round is provided. *)
val use_inlining_arguments_set : ?round:int -> inlining_arguments -> unit

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
val use_linscan : bool ref
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
val noversion : bool ref
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
val dump_rawclambda : bool ref
val dump_clambda : bool ref
val dump_rawflambda : bool ref
val dump_flambda : bool ref
val dump_flambda_let : int option ref
val dump_instr : bool ref
val keep_asm_file : bool ref
val optimize_for_speed : bool ref
val dump_cmm : bool ref
val dump_selection : bool ref
val dump_cse : bool ref
val dump_live : bool ref
val dump_avail : bool ref
val debug_runavail : bool ref
val dump_spill : bool ref
val dump_split : bool ref
val dump_interf : bool ref
val dump_prefer : bool ref
val dump_regalloc : bool ref
val dump_reload : bool ref
val dump_scheduling : bool ref
val dump_linear : bool ref
val dump_interval : bool ref
val keep_startup_file : bool ref
val dump_combine : bool ref
val native_code : bool ref
val default_inline_threshold : float
val inline_threshold : Float_arg_helper.parsed ref
val inlining_report : bool ref
val simplify_rounds : int option ref
val default_simplify_rounds : int ref
val rounds : unit -> int
val default_inline_max_unroll : int
val inline_max_unroll : Int_arg_helper.parsed ref
val default_inline_toplevel_threshold : int
val inline_toplevel_threshold : Int_arg_helper.parsed ref
val default_inline_call_cost : int
val default_inline_alloc_cost : int
val default_inline_prim_cost : int
val default_inline_branch_cost : int
val default_inline_indirect_cost : int
val default_inline_lifting_benefit : int
val inline_call_cost : Int_arg_helper.parsed ref
val inline_alloc_cost : Int_arg_helper.parsed ref
val inline_prim_cost : Int_arg_helper.parsed ref
val inline_branch_cost : Int_arg_helper.parsed ref
val inline_indirect_cost : Int_arg_helper.parsed ref
val inline_lifting_benefit : Int_arg_helper.parsed ref
val default_inline_branch_factor : float
val inline_branch_factor : Float_arg_helper.parsed ref
val dont_write_files : bool ref
val std_include_flag : string -> string
val std_include_dir : unit -> string list
val shared : bool ref
val dlcode : bool ref
val pic_code : bool ref
val runtime_variant : string ref
val force_slash : bool ref
val keep_docs : bool ref
val keep_locs : bool ref
val unsafe_string : bool ref
val opaque : bool ref
val profile_columns : Profile.column list ref
val flambda_invariant_checks : bool ref
val unbox_closures : bool ref
val unbox_closures_factor : int ref
val default_unbox_closures_factor : int
val unbox_free_vars_of_closures : bool ref
val unbox_specialised_args : bool ref
val clambda_checks : bool ref
val default_inline_max_depth : int
val inline_max_depth : Int_arg_helper.parsed ref
val remove_unused_arguments : bool ref
val dump_flambda_verbose : bool ref
val classic_inlining : bool ref
val afl_instrument : bool ref
val afl_inst_ratio : int ref

val all_passes : string list ref
val dumped_pass : string -> bool
val set_dumped_pass : string -> bool -> unit

val parse_color_setting : string -> Misc.Color.setting option
val color : Misc.Color.setting option ref

val unboxed_types : bool ref

val arg_spec : (string * Arg.spec * string) list ref

(* [add_arguments __LOC__ args] will add the arguments from [args] at
   the end of [arg_spec], checking that they have not already been
   added by [add_arguments] before. A warning is printed showing the
   locations of the function from which the argument was previously
   added. *)
val add_arguments : string -> (string * Arg.spec * string) list -> unit

(* [parse_arguments anon_arg usage] will parse the arguments, using
  the arguments provided in [Clflags.arg_spec]. It allows plugins to
  provide their own arguments.
*)
val parse_arguments : Arg.anon_fun -> string -> unit

(* [print_arguments usage] print the standard usage message *)
val print_arguments : string -> unit

(* [reset_arguments ()] clear all declared arguments *)
val reset_arguments : unit -> unit

 
type mli_status = Mli_na | Mli_exists | Mli_non_exists
val no_implicit_current_dir : bool ref
val assume_no_mli : mli_status ref 
val record_event_when_debug : bool ref 
val bs_vscode : bool
val dont_record_crc_unit : string option ref
val bs_only : bool ref (* set true on bs top*)
val no_assert_false : bool ref


end = struct
#1 "clflags.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Command-line parameters *)

module Int_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Int
    let of_string = int_of_string
  end
end)
module Float_arg_helper = Arg_helper.Make (struct
  module Key = struct
    include Numbers.Int
    let of_string = int_of_string
  end

  module Value = struct
    include Numbers.Float
    let of_string = float_of_string
  end
end)

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
and use_linscan = ref false             (* -linscan *)
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
and noversion = ref false               (* -no-version *)
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
and dump_rawclambda = ref false         (* -drawclambda *)
and dump_clambda = ref false            (* -dclambda *)
and dump_rawflambda = ref false            (* -drawflambda *)
and dump_flambda = ref false            (* -dflambda *)
and dump_flambda_let = ref (None : int option) (* -dflambda-let=... *)
and dump_flambda_verbose = ref false    (* -dflambda-verbose *)
and dump_instr = ref false              (* -dinstr *)

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
and opaque = ref false                  (* -opaque *)

and dump_cmm = ref false                (* -dcmm *)
let dump_selection = ref false          (* -dsel *)
let dump_cse = ref false                (* -dcse *)
let dump_live = ref false               (* -dlive *)
let dump_avail = ref false              (* -davail *)
let dump_spill = ref false              (* -dspill *)
let dump_split = ref false              (* -dsplit *)
let dump_interf = ref false             (* -dinterf *)
let dump_prefer = ref false             (* -dprefer *)
let dump_regalloc = ref false           (* -dalloc *)
let dump_reload = ref false             (* -dreload *)
let dump_scheduling = ref false         (* -dscheduling *)
let dump_linear = ref false             (* -dlinear *)
let dump_interval = ref false           (* -dinterval *)
let keep_startup_file = ref false       (* -dstartup *)
let dump_combine = ref false            (* -dcombine *)
let profile_columns : Profile.column list ref = ref [] (* -dprofile/-dtimings *)

let debug_runavail = ref false          (* -drunavail *)

let native_code = ref false             (* set to true under ocamlopt *)

let force_slash = ref false             (* for ocamldep *)
let clambda_checks = ref false          (* -clambda-checks *)

let flambda_invariant_checks = ref true (* -flambda-invariants *)

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

let pic_code = ref (match Config.architecture with (* -fPIC *)
                     | "amd64" -> true
                     | _       -> false)

let runtime_variant = ref "";;      (* -runtime-variant *)

let keep_docs = ref false              (* -keep-docs *)
let keep_locs = ref true               (* -keep-locs *)
let unsafe_string =
  if Config.safe_string then ref false
  else ref (not Config.default_safe_string)
                                   (* -safe-string / -unsafe-string *)

let classic_inlining = ref false       (* -Oclassic *)
let inlining_report = ref false    (* -inlining-report *)

let afl_instrument = ref Config.afl_instrument (* -afl-instrument *)
let afl_inst_ratio = ref 100           (* -afl-inst-ratio *)

let simplify_rounds = ref None        (* -rounds *)
let default_simplify_rounds = ref 1        (* -rounds *)
let rounds () =
  match !simplify_rounds with
  | None -> !default_simplify_rounds
  | Some r -> r

let default_inline_threshold = if Config.flambda then 10. else 10. /. 8.
let inline_toplevel_multiplier = 16
let default_inline_toplevel_threshold =
  int_of_float ((float inline_toplevel_multiplier) *. default_inline_threshold)
let default_inline_call_cost = 5
let default_inline_alloc_cost = 7
let default_inline_prim_cost = 3
let default_inline_branch_cost = 5
let default_inline_indirect_cost = 4
let default_inline_branch_factor = 0.1
let default_inline_lifting_benefit = 1300
let default_inline_max_unroll = 0
let default_inline_max_depth = 1

let inline_threshold = ref (Float_arg_helper.default default_inline_threshold)
let inline_toplevel_threshold =
  ref (Int_arg_helper.default default_inline_toplevel_threshold)
let inline_call_cost = ref (Int_arg_helper.default default_inline_call_cost)
let inline_alloc_cost = ref (Int_arg_helper.default default_inline_alloc_cost)
let inline_prim_cost = ref (Int_arg_helper.default default_inline_prim_cost)
let inline_branch_cost =
  ref (Int_arg_helper.default default_inline_branch_cost)
let inline_indirect_cost =
  ref (Int_arg_helper.default default_inline_indirect_cost)
let inline_branch_factor =
  ref (Float_arg_helper.default default_inline_branch_factor)
let inline_lifting_benefit =
  ref (Int_arg_helper.default default_inline_lifting_benefit)
let inline_max_unroll =
  ref (Int_arg_helper.default default_inline_max_unroll)
let inline_max_depth =
  ref (Int_arg_helper.default default_inline_max_depth)


let unbox_specialised_args = ref true   (* -no-unbox-specialised-args *)
let unbox_free_vars_of_closures = ref true
let unbox_closures = ref false          (* -unbox-closures *)
let default_unbox_closures_factor = 10
let unbox_closures_factor =
  ref default_unbox_closures_factor      (* -unbox-closures-factor *)
let remove_unused_arguments = ref false (* -remove-unused-arguments *)

type inlining_arguments = {
  inline_call_cost : int option;
  inline_alloc_cost : int option;
  inline_prim_cost : int option;
  inline_branch_cost : int option;
  inline_indirect_cost : int option;
  inline_lifting_benefit : int option;
  inline_branch_factor : float option;
  inline_max_depth : int option;
  inline_max_unroll : int option;
  inline_threshold : float option;
  inline_toplevel_threshold : int option;
}

let set_int_arg round (arg:Int_arg_helper.parsed ref) default value =
  let value : int =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Int_arg_helper.set_base_default value
             (Int_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Int_arg_helper.add_base_override round value !arg

let set_float_arg round (arg:Float_arg_helper.parsed ref) default value =
  let value =
    match value with
    | None -> default
    | Some value -> value
  in
  match round with
  | None ->
    arg := Float_arg_helper.set_base_default value
             (Float_arg_helper.reset_base_overrides !arg)
  | Some round ->
    arg := Float_arg_helper.add_base_override round value !arg

let use_inlining_arguments_set ?round (arg:inlining_arguments) =
  let set_int = set_int_arg round in
  let set_float = set_float_arg round in
  set_int inline_call_cost default_inline_call_cost arg.inline_call_cost;
  set_int inline_alloc_cost default_inline_alloc_cost arg.inline_alloc_cost;
  set_int inline_prim_cost default_inline_prim_cost arg.inline_prim_cost;
  set_int inline_branch_cost
    default_inline_branch_cost arg.inline_branch_cost;
  set_int inline_indirect_cost
    default_inline_indirect_cost arg.inline_indirect_cost;
  set_int inline_lifting_benefit
    default_inline_lifting_benefit arg.inline_lifting_benefit;
  set_float inline_branch_factor
    default_inline_branch_factor arg.inline_branch_factor;
  set_int inline_max_depth
    default_inline_max_depth arg.inline_max_depth;
  set_int inline_max_unroll
    default_inline_max_unroll arg.inline_max_unroll;
  set_float inline_threshold
    default_inline_threshold arg.inline_threshold;
  set_int inline_toplevel_threshold
    default_inline_toplevel_threshold arg.inline_toplevel_threshold

(* o1 is the default *)
let o1_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  inline_threshold = None;
  inline_toplevel_threshold = None;
}

let classic_arguments = {
  inline_call_cost = None;
  inline_alloc_cost = None;
  inline_prim_cost = None;
  inline_branch_cost = None;
  inline_indirect_cost = None;
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = None;
  inline_max_unroll = None;
  (* [inline_threshold] matches the current compiler's default.
     Note that this particular fraction can be expressed exactly in
     floating point. *)
  inline_threshold = Some (10. /. 8.);
  (* [inline_toplevel_threshold] is not used in classic mode. *)
  inline_toplevel_threshold = Some 1;
}

let o2_arguments = {
  inline_call_cost = Some (2 * default_inline_call_cost);
  inline_alloc_cost = Some (2 * default_inline_alloc_cost);
  inline_prim_cost = Some (2 * default_inline_prim_cost);
  inline_branch_cost = Some (2 * default_inline_branch_cost);
  inline_indirect_cost = Some (2 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = None;
  inline_max_depth = Some 2;
  inline_max_unroll = None;
  inline_threshold = Some 25.;
  inline_toplevel_threshold = Some (25 * inline_toplevel_multiplier);
}

let o3_arguments = {
  inline_call_cost = Some (3 * default_inline_call_cost);
  inline_alloc_cost = Some (3 * default_inline_alloc_cost);
  inline_prim_cost = Some (3 * default_inline_prim_cost);
  inline_branch_cost = Some (3 * default_inline_branch_cost);
  inline_indirect_cost = Some (3 * default_inline_indirect_cost);
  inline_lifting_benefit = None;
  inline_branch_factor = Some 0.;
  inline_max_depth = Some 3;
  inline_max_unroll = Some 1;
  inline_threshold = Some 50.;
  inline_toplevel_threshold = Some (50 * inline_toplevel_multiplier);
}

let all_passes = ref []
let dumped_passes_list = ref []
let dumped_pass s =
  assert(List.mem s !all_passes);
  List.mem s !dumped_passes_list

let set_dumped_pass s enabled =
  if (List.mem s !all_passes) then begin
    let passes_without_s = List.filter ((<>) s) !dumped_passes_list in
    let dumped_passes =
      if enabled then
        s :: passes_without_s
      else
        passes_without_s
    in
    dumped_passes_list := dumped_passes
  end

let parse_color_setting = function
  | "auto" -> Some Misc.Color.Auto
  | "always" -> Some Misc.Color.Always
  | "never" -> Some Misc.Color.Never
  | _ -> None
let color = ref None ;; (* -color *)

let unboxed_types = ref false

let arg_spec = ref []
let arg_names = ref Misc.StringMap.empty

let reset_arguments () =
  arg_spec := [];
  arg_names := Misc.StringMap.empty

let add_arguments loc args =
  List.iter (function (arg_name, _, _) as arg ->
    try
      let loc2 = Misc.StringMap.find arg_name !arg_names in
      Printf.eprintf
        "Warning: plugin argument %s is already defined:\n" arg_name;
      Printf.eprintf "   First definition: %s\n" loc2;
      Printf.eprintf "   New definition: %s\n" loc;
    with Not_found ->
      arg_spec := !arg_spec @ [ arg ];
      arg_names := Misc.StringMap.add arg_name loc !arg_names
  ) args

let print_arguments usage =
  Arg.usage !arg_spec usage

(* This function is almost the same as [Arg.parse_expand], except
   that [Arg.parse_expand] could not be used because it does not take a
   reference for [arg_spec].*)
let parse_arguments f msg =
  try
    let argv = ref Sys.argv in
    let current = ref (!Arg.current) in
    Arg.parse_and_expand_argv_dynamic current argv arg_spec f msg
  with
  | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
  | Arg.Help msg -> Printf.printf "%s" msg; exit 0

 
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


end
module Terminfo : sig 
#1 "terminfo.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string * loc * loc        (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted of string list           (*  6 *)
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
  | All_clauses_guarded                     (* 8, used to be 25 *)
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
  | Unused_extension of string * bool * bool * bool (* 38 *)
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
  | No_cmi_file of string * string option   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)
  | Expect_tailcall                         (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of string           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_pattern of string list        (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Assignment_to_non_mutable_value         (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)

  | Bs_unused_attribute of string           (* 101 *)
  | Bs_polymorphic_comparison               (* 102 *)
  | Bs_ffi_warning of string                (* 103 *)
  | Bs_derive_warning of string             (* 104 *)
      
;;

val parse_options : bool -> string -> unit;;

val without_warnings : (unit -> 'a) -> 'a

val is_active : t -> bool;;
val is_error : t -> bool;;

val defaults_w : string;;
val defaults_warn_error : string;;

type reporting_information =
  { number : int
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

val report : t -> [ `Active of reporting_information | `Inactive ]

exception Errors;;

val check_fatal : unit -> unit;;
val reset_fatal: unit -> unit

val help_warnings: unit -> unit

type state
val backup: unit -> state
val restore: state -> unit
val mk_lazy: (unit -> 'a) -> 'a Lazy.t
    (** Like [Lazy.of_fun], but the function is applied with
        the warning settings at the time [mk_lazy] is called. *)


val message : t -> string 
val number: t -> int
val super_report :
  (t -> string) ->
  t ->  [ `Active of reporting_information | `Inactive ]


end = struct
#1 "warnings.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update the documentation:
   - man/ocamlc.m
   - man/ocamlopt.m
   - manual/manual/cmds/comp.etex
   - manual/manual/cmds/native.etex
*)

type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
  | Deprecated of string * loc * loc        (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Partial_application                     (*  5 *)
  | Labels_omitted of string list           (*  6 *)
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
  | All_clauses_guarded                     (* 8, used to be 25 *)
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
  | Unused_extension of string * bool * bool * bool (* 38 *)
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
  | No_cmi_file of string * string option   (* 49 *)
  | Bad_docstring of bool                   (* 50 *)
  | Expect_tailcall                         (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of string           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_pattern of string list        (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Assignment_to_non_mutable_value         (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)
    
    
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
  | Labels_omitted _ -> 6
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
  | All_clauses_guarded -> 8 (* used to be 25 *)
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
  | Expect_tailcall -> 51
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Inlining_impossible _ -> 55
  | Unreachable_case -> 56
  | Ambiguous_pattern _ -> 57
  | No_cmx_file _ -> 58
  | Assignment_to_non_mutable_value -> 59
  | Unused_module _ -> 60
  | Unboxable_type_in_prim_decl _ -> 61
  | Constraint_on_gadt -> 62

    
  | Bs_unused_attribute _ -> 101
  | Bs_polymorphic_comparison -> 102
  | Bs_ffi_warning _ -> 103
  | Bs_derive_warning _ -> 104
    
;;

let last_warning_number = 104
;;

let letter_all = 
  let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
  loop last_warning_number

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' -> letter_all
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
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
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

let disabled = ref false

let without_warnings f =
  Misc.protect_refs [Misc.R(disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x = not !disabled && (!current).active.(number x);;
let is_error x = not !disabled && (!current).error.(number x);;

let mk_lazy f =
  let state = backup () in
  lazy
    (
      let prev = backup () in
      restore state;
      try
        let r = f () in
        restore prev;
        r
      with exn ->
        restore prev;
        raise exn
    )

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
       List.iter set (letter (Char.lowercase_ascii s.[i]));
       loop (i+1)
    | 'a' .. 'z' ->
       List.iter clear (letter s.[i]);
       loop (i+1)
    | '+' -> loop_letter_num set (i+1)
    | '-' -> loop_letter_num clear (i+1)
    | '@' -> loop_letter_num set_all (i+1)
    | _ -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 last_warning_number do myset n done;
        loop i
    | 'A' .. 'Z' ->
       List.iter myset (letter (Char.lowercase_ascii s.[i]));
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
let defaults_w = "+a-4-6-7-9-27-29-32..42-44-45-48-50-60-102";;
let defaults_warn_error = "-a+31";;

let () = parse_options false defaults_w;;
let () = parse_options true defaults_warn_error;;

let message = function
  | Comment_start -> "this is the start of a comment."
  | Comment_not_end -> "this is not the end of a comment."
  | Deprecated (s, _, _) ->
      (* Reduce \r\n to \n:
           - Prevents any \r characters being printed on Unix when processing
             Windows sources
           - Prevents \r\r\n being generated on Windows, which affects the
             testsuite
       *)
       "deprecated: " ^ Misc.normalise_eol s
  | Fragile_match "" ->
      "this pattern-matching is fragile."
  | Fragile_match s ->
      "this pattern-matching is fragile.\n\
       It will remain exhaustive when constructors are added to type " ^ s ^ "."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Labels_omitted [] -> assert false
  | Labels_omitted [l] ->
     "label " ^ l ^ " was omitted in the application of this function."
  | Labels_omitted ls ->
     "labels " ^ String.concat ", " ls ^
       " were omitted in the application of this function."
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
       Here is an example of a case that is not matched:\n" ^ s
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
      "this pattern-matching is not exhaustive.\n\
       All clauses in this pattern-matching are guarded."
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
  | Unused_extension (s, is_exception, cu_pattern, cu_privatize) ->
     let kind =
       if is_exception then "exception" else "extension constructor" in
     let name = kind ^ " " ^ s in
     begin match cu_pattern, cu_privatize with
       | false, false -> "unused " ^ name
       | true, _ ->
          name ^
          " is never used to build values.\n\
           (However, this constructor appears in patterns.)"
       | false, true ->
          name ^
          " is never used to build values.\n\
            It is exported or rebound as a private extension."
     end
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
  | Ambiguous_name (_slist, tl, true) ->
      "these field labels belong to several types: " ^
      String.concat " " tl ^
      "\nThe first one was selected. Please disambiguate if this is wrong."
  | Disambiguated_name s ->
      "this use of " ^ s ^ " relies on type-directed disambiguation,\n\
       it will not compile with OCaml 4.00 or earlier."
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
  | No_cmi_file(name, None) ->
      "no cmi file was found in path for module " ^ name
  | No_cmi_file(name, Some msg) ->
      Printf.sprintf
        "no valid cmi file was found in path for module %s. %s"
        name msg
  | Bad_docstring unattached ->
      if unattached then "unattached documentation comment (ignored)"
      else "ambiguous documentation comment"
  | Expect_tailcall ->
      Printf.sprintf "expected tailcall"
  | Fragile_literal_pattern ->
      Printf.sprintf
        "Code should not depend on the actual values of\n\
         this constructor's arguments. They are only for information\n\
         and may change in future versions. (See manual section 8.5)"
  | Unreachable_case ->
      "this match case is unreachable.\n\
       Consider replacing it with a refutation case '<pat> -> .'"
  | Misplaced_attribute attr_name ->
      Printf.sprintf "the %S attribute cannot appear in this context" attr_name
  | Duplicated_attribute attr_name ->
      Printf.sprintf "the %S attribute is used more than once on this \
          expression"
        attr_name
  | Inlining_impossible reason ->
      Printf.sprintf "Cannot inline: %s" reason
  | Ambiguous_pattern vars ->
      let msg =
        let vars = List.sort String.compare vars in
        match vars with
        | [] -> assert false
        | [x] -> "variable " ^ x
        | _::_ ->
            "variables " ^ String.concat "," vars in
      Printf.sprintf
        "Ambiguous or-pattern variables under guard;\n\
         %s may match different arguments. (See manual section 8.5)"
        msg
  | No_cmx_file name ->
      Printf.sprintf
        "no cmx file was found in path for module %s, \
         and its interface was not compiled with -opaque" name
  | Assignment_to_non_mutable_value ->
      "A potential assignment to a non-mutable value was detected \n\
        in this source file.  Such assignments may generate incorrect code \n\
        when using Flambda."
  | Unused_module s -> "unused module " ^ s ^ "."
  | Unboxable_type_in_prim_decl t ->
      Printf.sprintf
        "This primitive declaration uses type %s, which is unannotated and\n\
         unboxable. The representation of such types may change in future\n\
         versions. You should annotate the declaration of %s with [@@boxed]\n\
         or [@@unboxed]." t t
  | Constraint_on_gadt ->
      "Type constraints do not apply to GADT cases of variant types."


  | Bs_unused_attribute s ->
      "Unused BuckleScript attribute: " ^ s
  | Bs_polymorphic_comparison ->
      "polymorphic comparison introduced (maybe unsafe)"
  | Bs_ffi_warning s ->
      "BuckleScript FFI warning: " ^ s
  | Bs_derive_warning s ->
      "BuckleScript bs.deriving warning: " ^ s     

;;

let sub_locs = function
  | Deprecated (_, def, use) ->
      [
        def, "Definition";
        use, "Expected signature";
      ]
  | _ -> []

let nerrors = ref 0;;

type reporting_information =
  { number : int
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

let report w =
  match is_active w with
  | false -> `Inactive
  | true ->
     if is_error w then incr nerrors;
     `Active { number = number w; message = message w; is_error = is_error w;
               sub_locs = sub_locs w;
             }
;;


let super_report message w =
  match is_active w with
  | false -> `Inactive
  | true ->
     if is_error w then incr nerrors;
     `Active { number = number w; message = message w; is_error = is_error w;
               sub_locs = sub_locs w;
             }
;;    

exception Errors;;

let reset_fatal () =
  nerrors := 0

let check_fatal () =
  if !nerrors > 0 then begin
    nerrors := 0;
    raise Errors;
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
   10, "Expression on the left-hand side of a sequence that doesn't have \
      type\n\
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
   22, "Preprocessor warning.";
   23, "Useless record \"with\" clause.";
   24, "Bad module name: the source file name is not a valid OCaml module \
        name.";
   25, "Deprecated: now part of warning 8.";
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
   42, "Disambiguated constructor or label name (compatibility warning).";
   43, "Nonoptional label applied as optional.";
   44, "Open statement shadows an already defined identifier.";
   45, "Open statement shadows an already defined label or constructor.";
   46, "Error in environment variable.";
   47, "Illegal attribute payload.";
   48, "Implicit elimination of optional arguments.";
   49, "Absent cmi file when looking up module alias.";
   50, "Unexpected documentation comment.";
   51, "Warning on non-tail calls if @tailcall present.";
   52, "Fragile constant pattern.";
   53, "Attribute cannot appear in this context";
   54, "Attribute used more than once on an expression";
   55, "Inlining impossible";
   56, "Unreachable case in a pattern-matching (based on type information).";
   57, "Ambiguous or-pattern variables under guard";
   58, "Missing cmx file";
   59, "Assignment to non-mutable value";
   60, "Unused module declaration";
   61, "Unboxable type in primitive declaration";
   62, "Type constraint on GADT type declaration";
    
    
   101, "Unused bs attributes";
   102, "polymorphic comparison introduced (maybe unsafe)";
   103, "BuckleScript FFI warning: " ;
   104, "BuckleScript bs.deriving warning: "
   
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
        Printf.printf "  %c Alias for warning %i.\n" (Char.uppercase_ascii c) n
    | l ->
        Printf.printf "  %c warnings %s.\n"
          (Char.uppercase_ascii c)
          (String.concat ", " (List.map string_of_int l))
  done;
  exit 0
;;

end
module Location : sig 
#1 "location.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = Warnings.loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

(** Note on the use of Lexing.position in this module.
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

val default_printer : formatter -> t -> unit
val printer : (formatter -> t -> unit) ref

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
val print_compact: formatter -> t -> unit
val print_filename: formatter -> string -> unit

val absolute_path: string -> string

val show_filename: string -> string
    (** In -absname mode, return the absolute path for this filename.
        Otherwise, returns the filename unchanged. *)


val absname: bool ref

(** Support for located errors *)

type error =
  {
    loc: t;
    msg: string;
    sub: error list;
    if_highlight: string; (* alternative message if locations are highlighted *)
  }

exception Already_displayed_error
exception Error of error

val error: ?loc:t -> ?sub:error list -> ?if_highlight:string -> string -> error


val print_error_prefix : Format.formatter -> unit
val pp_ksprintf : ?before:(formatter -> unit) -> (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b


val errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, error) format4 -> 'a

val raise_errorf: ?loc:t -> ?sub:error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val error_of_printer: t -> (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error

val error_of_exn: exn -> [ `Ok of error | `Already_displayed ] option

val register_error_of_exn: (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
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
(** Reraise the exception if it is unknown. *)

val deprecated: ?def:t -> ?use:t -> t -> string -> unit

end = struct
#1 "location.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

let absname = ref false
    (* This reference should be in Clflags, but it would create an additional
       dependency and make bootstrapping Camlp4 more difficult. *)

type t = Warnings.loc = { loc_start: position; loc_end: position; loc_ghost: bool };;

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
  Format.fprintf ppf "@[<v>Characters %i-%i:@,"
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
        Format.fprintf ppf "@,  ";
        for _i = !pos_at_bol to loc.loc_start.pos_cnum - 1 do
          Format.pp_print_char ppf ' '
        done;
        for _i = loc.loc_start.pos_cnum to loc.loc_end.pos_cnum - 1 do
          Format.pp_print_char ppf '^'
        done
      end;
      if !line >= !line_start && !line <= !line_end then begin
        Format.fprintf ppf "@,";
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
  done;
  Format.fprintf ppf "@]"

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

let default_printer ppf loc =
  setup_colors ();
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else fprintf ppf "@{<loc>%a@}%s@," print_loc loc msg_colon
;;

let printer = ref default_printer
let print ppf loc = !printer ppf loc

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_error_prefix ppf =
  setup_colors ();
  fprintf ppf "@{<error>%s@}" error_prefix;
;;

let print_compact ppf loc =
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else begin
    let (file, line, startchar) = get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    fprintf ppf "%a:%i" print_filename file line;
    if startchar >= 0 then fprintf ppf ",%i--%i" startchar endchar
  end
;;

let print_error ppf loc =
  fprintf ppf "%a%t:" print loc print_error_prefix;
;;

let print_error_cur_file ppf () = print_error ppf (in_file !input_name);;

let default_warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs } ->
    setup_colors ();
    fprintf ppf "@[<v>";
    print ppf loc;
    if is_error
    then
      fprintf ppf "%t (%s %d): %s@," print_error_prefix
           (String.uncapitalize_ascii warning_prefix) number message
    else fprintf ppf "@{<warning>%s@} %d: %s@," warning_prefix number message;
    List.iter
      (fun (loc, msg) ->
         if loc <> none then fprintf ppf "  %a  %s@," print loc msg
      )
      sub_locs;
    fprintf ppf "@]"
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

exception Already_displayed_error = Warnings.Errors

let error_of_exn exn =
  match exn with
  | Already_displayed_error -> Some `Already_displayed
  | _ ->
     let rec loop = function
       | [] -> None
       | f :: rest ->
          match f exn with
          | Some error -> Some (`Ok error)
          | None -> loop rest
     in
     loop !error_of_exn

let rec default_error_reporter ppf ({loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" && loc.loc_start.pos_fname = "//toplevel//" then
      let rec collect_locs locs {loc; sub; _} =
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
    fprintf ppf "@[<v>%a %s" print_error loc msg;
    List.iter (Format.fprintf ppf "@,@[<2>%a@]" default_error_reporter) sub;
    fprintf ppf "@]"
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

      | Misc.HookExnWrapper {error = e; hook_name;
                             hook_info={Misc.sourcefile}} ->
          let sub = match error_of_exn e with
            | None | Some `Already_displayed -> error (Printexc.to_string e)
            | Some (`Ok err) -> err
          in
          Some
            (errorf ~loc:(in_file sourcefile)
               "In hook %S:" hook_name
               ~sub:[sub])
      | _ -> None
    )

external reraise : exn -> 'a = "%reraise"

let rec report_exception_rec n ppf exn =
  try
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) -> fprintf ppf "@[%a@]@." report_error err
  with exn when n > 0 -> report_exception_rec (n-1) ppf exn

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

let deprecated ?(def = none) ?(use = none) loc msg =
  prerr_warning loc (Warnings.Deprecated (msg, def, use))

end
module Bs_exception : sig 
#1 "bs_exception.mli"
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
  | Cmj_not_found of string
  | Js_not_found of string
  | Bs_cyclic_depends of string  list
  | Bs_duplicated_module of string * string
  | Bs_duplicate_exports of string (* gpr_974 *)
  | Bs_package_not_found of string                                                        
  | Bs_main_not_exist of string 
  | Bs_invalid_path of string
  | Missing_ml_dependency of string 
  | Dependency_script_module_dependent_not  of string
(*
TODO: In the futrue, we should refine dependency [bsb] 
should not rely on such exception, it should have its own exception handling
*)

(* exception Error of error *)

(* val report_error : Format.formatter -> error -> unit *)

val error : error -> 'a 

end = struct
#1 "bs_exception.ml"
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
  | Cmj_not_found of string
  | Js_not_found of string 
  | Bs_cyclic_depends of string  list
  | Bs_duplicated_module of string * string
  | Bs_duplicate_exports of string (* gpr_974 *)
  | Bs_package_not_found of string                            
  | Bs_main_not_exist of string 
  | Bs_invalid_path of string
  | Missing_ml_dependency of string 
  | Dependency_script_module_dependent_not  of string 
  (** TODO: we need add location handling *)    
exception Error of error

let error err = raise (Error err)

let report_error ppf = function
  | Dependency_script_module_dependent_not s
    -> 
    Format.fprintf ppf 
      "%s is compiled in script mode while its dependent is not"
      s
  | Missing_ml_dependency s -> 
    Format.fprintf ppf "Missing dependency %s in search path" s 
  | Cmj_not_found s ->
    Format.fprintf ppf "%s not found, cmj format is generated by BuckleScript" s
  | Js_not_found s -> 
    Format.fprintf ppf "%s not found, needed in script mode " s
  | Bs_cyclic_depends  str
    ->
    Format.fprintf ppf "Cyclic depends : @[%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      str
  | Bs_duplicate_exports str -> 
    Format.fprintf ppf "%s are exported as twice" str 
  | Bs_duplicated_module (a,b)
    ->
    Format.fprintf ppf "The build system does not support two files with same names yet %s, %s" a b
  | Bs_main_not_exist main
    ->
    Format.fprintf ppf "File %s not found " main

  | Bs_package_not_found package
    ->
    Format.fprintf ppf "Package %s not found or %s/lib/ocaml does not exist or please set npm_config_prefix correctly"
      package package
  | Bs_invalid_path path
    ->  Format.pp_print_string ppf ("Invalid path: " ^ path )


let () =
  Location.register_error_of_exn
    (function
      | Error err
        -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

end
(** Interface as module  *)
module Asttypes
= struct
#1 "asttypes.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary AST types used by parsetree and typedtree. *)

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

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type arg_label =
    Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)

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
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t

val flatten: t -> string list
val unflatten: string list -> t option
val last: t -> string
val parse: string -> t

end = struct
#1 "longident.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl -> Some (List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl)

let parse s =
  match unflatten (split_at_dots s 0) with
  | None -> Lident ""  (* should not happen, but don't put assert false
                          so as not to crash the toplevel (see Genprintval) *)
  | Some v -> v

end
(** Interface as module  *)
module Parsetree
= struct
#1 "parsetree.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstract syntax tree produced by parsing *)

open Asttypes

type constant =
    Pconst_integer of string * char option
  (* 3 3l 3L 3n

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes except 'l', 'L' and 'n' are rejected by the typechecker
  *)
  | Pconst_char of char
  (* 'c' *)
  | Pconst_string of string * string option
  (* "constant"
     {delim|other constant|delim}
  *)
  | Pconst_float of string * char option
  (* 3.4 2e5 1.4e-4

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
  *)

(** {1 Extension points} *)

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
  | PSig of signature (* : SIG *)
  | PTyp of core_type  (* : T *)
  | PPat of pattern * expression option  (* ? P  or  ? P when E *)

(** {1 Core language} *)

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
  | Ptyp_arrow of arg_label * core_type * core_type
        (* T1 -> T2       Simple
           ~l:T1 -> T2    Labelled
           ?l:T1 -> T2    Optional
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
  | Ptyp_object of object_field list * closed_flag
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
  | Ptyp_poly of string loc list * core_type
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
  | Rtag of label loc * attributes * bool * core_type list
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

and object_field =
  | Otag of label loc * attributes * core_type
  | Oinherit of core_type

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
  | Ppat_open of Longident.t loc * pattern
        (* M.(P) *)

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
  | Pexp_fun of arg_label * expression option * pattern * expression
        (* fun P -> E1                          (Simple, None)
           fun ~l:P -> E1                       (Labelled l, None)
           fun ?l:P -> E1                       (Optional l, None)
           fun ?l:(P = E0) -> E1                (Optional l, Some E0)

           Notes:
           - If E0 is provided, only Optional is allowed.
           - "fun P1 P2 .. Pn -> E1" is represented as nested Pexp_fun.
           - "let f P = E" is represented using Pexp_fun.
         *)
  | Pexp_apply of expression * (arg_label * expression) list
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
  | Pexp_send of expression * label loc
        (*  E # m *)
  | Pexp_new of Longident.t loc
        (* new M.c *)
  | Pexp_setinstvar of label loc * expression
        (* x <- 2 *)
  | Pexp_override of (label loc * expression) list
        (* {< x1 = E1; ...; Xn = En >} *)
  | Pexp_letmodule of string loc * module_expr * expression
        (* let module M = ME in E *)
  | Pexp_letexception of extension_constructor * expression
        (* let exception C in E *)
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
  | Pexp_newtype of string loc * expression
        (* fun (type t) -> E *)
  | Pexp_pack of module_expr
        (* (module ME)

           (module ME : S) is represented as
           Pexp_constraint(Pexp_pack, Ptyp_package S) *)
  | Pexp_open of override_flag * Longident.t loc * expression
        (* M.(E)
           let open M in E
           let! open M in E *)
  | Pexp_extension of extension
        (* [%id] *)
  | Pexp_unreachable
        (* . *)

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
     pld_attributes: attributes; (* l : T [@id1] [@id2] *)
    }

(*  { ...; l: T; ... }            (mutable=Immutable)
    { ...; mutable l: T; ... }    (mutable=Mutable)

    Note: T can be a Ptyp_poly.
*)

and constructor_declaration =
    {
     pcd_name: string loc;
     pcd_args: constructor_arguments;
     pcd_res: core_type option;
     pcd_loc: Location.t;
     pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
    }

and constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

(*
  | C of T1 * ... * Tn     (res = None,    args = Pcstr_tuple [])
  | C: T0                  (res = Some T0, args = [])
  | C: T1 * ... * Tn -> T0 (res = Some T0, args = Pcstr_tuple)
  | C of {...}             (res = None,    args = Pcstr_record)
  | C: {...} -> T0         (res = Some T0, args = Pcstr_record)
  | C of {...} as t        (res = None,    args = Pcstr_record)
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
     pext_attributes: attributes; (* C of ... [@id1] [@id2] *)
    }

and extension_constructor_kind =
    Pext_decl of constructor_arguments * core_type option
      (*
         | C of T1 * ... * Tn     ([T1; ...; Tn], None)
         | C: T0                  ([], Some T0)
         | C: T1 * ... * Tn -> T0 ([T1; ...; Tn], Some T0)
       *)
  | Pext_rebind of Longident.t loc
      (*
         | C = D
       *)

(** {1 Class language} *)

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
  | Pcty_arrow of arg_label * core_type * class_type
        (* T -> CT       Simple
           ~l:T -> CT    Labelled l
           ?l:T -> CT    Optional l
         *)
  | Pcty_extension of extension
        (* [%id] *)
  | Pcty_open of override_flag * Longident.t loc * class_type
        (* let open M in CT *)

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
  | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
        (* val x: T *)
  | Pctf_method  of (label loc * private_flag * virtual_flag * core_type)
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
  | Pcl_fun of arg_label * expression option * pattern * class_expr
        (* fun P -> CE                          (Simple, None)
           fun ~l:P -> CE                       (Labelled l, None)
           fun ?l:P -> CE                       (Optional l, None)
           fun ?l:(P = E0) -> CE                (Optional l, Some E0)
         *)
  | Pcl_apply of class_expr * (arg_label * expression) list
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
  | Pcl_open of override_flag * Longident.t loc * class_expr
  (* let open M in CE *)


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
  | Pcf_inherit of override_flag * class_expr * string loc option
        (* inherit CE
           inherit CE as x
           inherit! CE
           inherit! CE as x
         *)
  | Pcf_val of (label loc * mutable_flag * class_field_kind)
        (* val x = E
           val virtual x: T
         *)
  | Pcf_method of (label loc * private_flag * class_field_kind)
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

(** {1 Module language} *)

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
  | Psig_type of rec_flag * type_declaration list
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
  | Pwith_typesubst of Longident.t loc * type_declaration
        (* with type X.t := ..., same format as [Pwith_type] *)
  | Pwith_modsubst of Longident.t loc * Longident.t loc
        (* with module X.Y := Z *)

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
        (*  val x: T
            external x: T = "s1" ... "sn" *)
  | Pstr_type of rec_flag * type_declaration list
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

(** {1 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of string * directive_argument
     (* #use, #load ... *)

and directive_argument =
  | Pdir_none
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool

end
module Builtin_attributes : sig 
#1 "builtin_attributes.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Support for some of the builtin attributes:

   ocaml.deprecated
   ocaml.error
   ocaml.ppwarning
   ocaml.warning
   ocaml.warnerror
   ocaml.explicit_arity (for camlp4/camlp5)
   ocaml.warn_on_literal_pattern
   ocaml.deprecated_mutable
   ocaml.immediate
   ocaml.boxed / ocaml.unboxed
*)


val check_deprecated: Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit
val deprecated_of_attrs: Parsetree.attributes -> string option
val deprecated_of_sig: Parsetree.signature -> string option
val deprecated_of_str: Parsetree.structure -> string option

val check_deprecated_mutable:
    Location.t -> Parsetree.attributes -> string -> unit
val check_deprecated_mutable_inclusion:
  def:Location.t -> use:Location.t -> Location.t -> Parsetree.attributes ->
  Parsetree.attributes -> string -> unit

val error_of_extension: Parsetree.extension -> Location.error

val warning_attribute: ?ppwarning:bool -> Parsetree.attribute -> unit
  (** Apply warning settings from the specified attribute.
      "ocaml.warning"/"ocaml.warnerror" (and variants without the prefix)
      are processed and other attributes are ignored.

      Also implement ocaml.ppwarning (unless ~ppwarning:false is
      passed).
  *)

val warning_scope:
  ?ppwarning:bool ->
  Parsetree.attributes -> (unit -> 'a) -> 'a
  (** Execute a function in a new scope for warning settings.  This
      means that the effect of any call to [warning_attribute] during
      the execution of this function will be discarded after
      execution.

      The function also takes a list of attributes which are processed
      with [warning_attribute] in the fresh scope before the function
      is executed.
  *)

val warn_on_literal_pattern: Parsetree.attributes -> bool
val explicit_arity: Parsetree.attributes -> bool


val immediate: Parsetree.attributes -> bool

val has_unboxed: Parsetree.attributes -> bool
val has_boxed: Parsetree.attributes -> bool

end = struct
#1 "builtin_attributes.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree

let string_of_cst = function
  | Pconst_string(s, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let string_of_opt_payload p =
  match string_of_payload p with
  | Some s -> s
  | None -> ""

let rec error_of_extension ext =
  match ext with
  | ({txt = ("ocaml.error"|"error") as txt; loc}, p) ->
    let rec sub_from inner =
      match inner with
      | {pstr_desc=Pstr_extension (ext, _)} :: rest ->
          error_of_extension ext :: sub_from rest
      | _ :: rest ->
          (Location.errorf ~loc
             "Invalid syntax for sub-error of extension '%s'." txt) ::
            sub_from rest
      | [] -> []
    in
    begin match p with
    | PStr [] -> raise Location.Already_displayed_error
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::
           {pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(if_highlight,_))}, _)}::
           inner) ->
        Location.error ~loc ~if_highlight ~sub:(sub_from inner) msg
    | PStr({pstr_desc=Pstr_eval
              ({pexp_desc=Pexp_constant(Pconst_string(msg,_))}, _)}::inner) ->
        Location.error ~loc ~sub:(sub_from inner) msg
    | _ -> Location.errorf ~loc "Invalid syntax for extension '%s'." txt
    end
  | ({txt; loc}, _) ->
      Location.errorf ~loc "Uninterpreted extension '%s'." txt

let cat s1 s2 =
  if s2 = "" then s1 else
 
    if Clflags.bs_vscode then s1 ^ " " ^ s2
    else s1 ^ "\n" ^ s2 
    

let rec deprecated_of_attrs = function
  | [] -> None
  | ({txt = "ocaml.deprecated"|"deprecated"; _}, p) :: _ ->
      Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_of_attrs tl

let check_deprecated loc attrs s =
  match deprecated_of_attrs attrs with
  | None -> ()
  | Some txt -> Location.deprecated loc (cat s txt)

let check_deprecated_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_of_attrs attrs1, deprecated_of_attrs attrs2 with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None -> Location.deprecated ~def ~use loc (cat s txt)

let rec deprecated_mutable_of_attrs = function
  | [] -> None
  | ({txt = "ocaml.deprecated_mutable"|"deprecated_mutable"; _}, p) :: _ ->
      Some (string_of_opt_payload p)
  | _ :: tl -> deprecated_mutable_of_attrs tl

let check_deprecated_mutable loc attrs s =
  match deprecated_mutable_of_attrs attrs with
  | None -> ()
  | Some txt ->
      Location.deprecated loc (Printf.sprintf "mutating field %s" (cat s txt))

let check_deprecated_mutable_inclusion ~def ~use loc attrs1 attrs2 s =
  match deprecated_mutable_of_attrs attrs1,
        deprecated_mutable_of_attrs attrs2
  with
  | None, _ | Some _, Some _ -> ()
  | Some txt, None ->
      Location.deprecated ~def ~use loc
        (Printf.sprintf "mutating field %s" (cat s txt))

let rec deprecated_of_sig = function
  | {psig_desc = Psig_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_sig tl
      | Some _ as r -> r
      end
  | _ -> None


let rec deprecated_of_str = function
  | {pstr_desc = Pstr_attribute a} :: tl ->
      begin match deprecated_of_attrs [a] with
      | None -> deprecated_of_str tl
      | Some _ as r -> r
      end
  | _ -> None


let warning_attribute ?(ppwarning = true) =
  let process loc txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag s
        with Arg.Bad _ ->
          Location.prerr_warning loc
            (Warnings.Attribute_payload
               (txt, "Ill-formed list of warnings"))
        end
    | None ->
        Location.prerr_warning loc
          (Warnings.Attribute_payload
             (txt, "A single string literal is expected"))
  in
  function
  | ({txt = ("ocaml.warning"|"warning") as txt; loc}, payload) ->
      process loc txt false payload
  | ({txt = ("ocaml.warnerror"|"warnerror") as txt; loc}, payload) ->
      process loc txt true payload
  | {txt="ocaml.ppwarning"|"ppwarning"},
    PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant
                                   (Pconst_string (s, _))},_);
          pstr_loc}] when ppwarning ->
      Location.prerr_warning pstr_loc (Warnings.Preprocessor s)
  | _ ->
      ()

let warning_scope ?ppwarning attrs f =
  let prev = Warnings.backup () in
  try
    List.iter (warning_attribute ?ppwarning) (List.rev attrs);
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn


let warn_on_literal_pattern =
  List.exists
    (function
      | ({txt="ocaml.warn_on_literal_pattern"|"warn_on_literal_pattern"; _}, _)
        -> true
      | _ -> false
    )

let explicit_arity =
  List.exists
    (function
      | ({txt="ocaml.explicit_arity"|"explicit_arity"; _}, _) -> true
      | _ -> false
    )

let immediate =
  List.exists
    (function
      | ({txt="ocaml.immediate"|"immediate"; _}, _) -> true
      | _ -> false
    )

(* The "ocaml.boxed (default)" and "ocaml.unboxed (default)"
   attributes cannot be input by the user, they are added by the
   compiler when applying the default setting. This is done to record
   in the .cmi the default used by the compiler when compiling the
   source file because the default can change between compiler
   invocations. *)

let check l (x, _) = List.mem x.txt l

let has_unboxed attr =
  List.exists (check ["ocaml.unboxed"; "unboxed"])
    attr

let has_boxed attr =
  List.exists (check ["ocaml.boxed"; "boxed"]) attr

end
module Depend : sig 
#1 "depend.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Module dependencies. *)

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

type map_tree = Node of StringSet.t * bound_map
and  bound_map = map_tree StringMap.t
val make_leaf : string -> map_tree
val make_node : bound_map -> map_tree
val weaken_map : StringSet.t -> map_tree -> map_tree

val free_structure_names : StringSet.t ref

(* dependencies found by preprocessing tools (plugins) *)
val pp_deps : string list ref

val open_module : bound_map -> Longident.t -> bound_map

val add_use_file : bound_map -> Parsetree.toplevel_phrase list -> unit

val add_signature : bound_map -> Parsetree.signature -> unit

val add_implementation : bound_map -> Parsetree.structure -> unit

val add_implementation_binding : bound_map -> Parsetree.structure -> bound_map
val add_signature_binding : bound_map -> Parsetree.signature -> bound_map

end = struct
#1 "depend.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Location
open Longident
open Parsetree

let pp_deps = ref []

module StringSet = Set.Make(struct type t = string let compare = compare end)
module StringMap = Map.Make(String)

(* Module resolution map *)
(* Node (set of imports for this path, map for submodules) *)
type map_tree = Node of StringSet.t * bound_map
and  bound_map = map_tree StringMap.t
let bound = Node (StringSet.empty, StringMap.empty)

(*let get_free (Node (s, _m)) = s*)
let get_map (Node (_s, m)) = m
let make_leaf s = Node (StringSet.singleton s, StringMap.empty)
let make_node m =  Node (StringSet.empty, m)
let rec weaken_map s (Node(s0,m0)) =
  Node (StringSet.union s s0, StringMap.map (weaken_map s) m0)
let rec collect_free (Node (s, m)) =
  StringMap.fold (fun _ n -> StringSet.union (collect_free n)) m s

(* Returns the imports required to access the structure at path p *)
(* Only raises Not_found if the head of p is not in the toplevel map *)
let rec lookup_free p m =
  match p with
    [] -> raise Not_found
  | s::p ->
      let Node (f, m') = StringMap.find s m  in
      try lookup_free p m' with Not_found -> f

(* Returns the node corresponding to the structure at path p *)
let rec lookup_map lid m =
  match lid with
    Lident s    -> StringMap.find s m
  | Ldot (l, s) -> StringMap.find s (get_map (lookup_map l m))
  | Lapply _    -> raise Not_found

(* Collect free module identifiers in the a.s.t. *)

let free_structure_names = ref StringSet.empty

let add_names s =
  free_structure_names := StringSet.union s !free_structure_names

let rec add_path bv ?(p=[]) = function
  | Lident s ->
      let free =
        try lookup_free (s::p) bv with Not_found -> StringSet.singleton s
      in
      (*StringSet.iter (fun s -> Printf.eprintf "%s " s) free;
        prerr_endline "";*)
      add_names free
  | Ldot(l, s) -> add_path bv ~p:(s::p) l
  | Lapply(l1, l2) -> add_path bv l1; add_path bv l2

let open_module bv lid =
  match lookup_map lid bv with
  | Node (s, m) ->
      add_names s;
      StringMap.fold StringMap.add m bv
  | exception Not_found ->
      add_path bv lid; bv

let add_parent bv lid =
  match lid.txt with
    Ldot(l, _s) -> add_path bv l
  | _ -> ()

let add = add_parent

let addmodule bv lid = add_path bv lid.txt

let handle_extension ext =
  match (fst ext).txt with
  | "error" | "ocaml.error" ->
    raise (Location.Error
             (Builtin_attributes.error_of_extension ext))
  | _ ->
    ()

let rec add_type bv ty =
  match ty.ptyp_desc with
    Ptyp_any -> ()
  | Ptyp_var _ -> ()
  | Ptyp_arrow(_, t1, t2) -> add_type bv t1; add_type bv t2
  | Ptyp_tuple tl -> List.iter (add_type bv) tl
  | Ptyp_constr(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_object (fl, _) ->
      List.iter
       (function Otag (_, _, t) -> add_type bv t
         | Oinherit t -> add_type bv t) fl
  | Ptyp_class(c, tl) -> add bv c; List.iter (add_type bv) tl
  | Ptyp_alias(t, _) -> add_type bv t
  | Ptyp_variant(fl, _, _) ->
      List.iter
        (function Rtag(_,_,_,stl) -> List.iter (add_type bv) stl
          | Rinherit sty -> add_type bv sty)
        fl
  | Ptyp_poly(_, t) -> add_type bv t
  | Ptyp_package pt -> add_package_type bv pt
  | Ptyp_extension e -> handle_extension e

and add_package_type bv (lid, l) =
  add bv lid;
  List.iter (add_type bv) (List.map (fun (_, e) -> e) l)

let add_opt add_fn bv = function
    None -> ()
  | Some x -> add_fn bv x

let add_constructor_arguments bv = function
  | Pcstr_tuple l -> List.iter (add_type bv) l
  | Pcstr_record l -> List.iter (fun l -> add_type bv l.pld_type) l

let add_constructor_decl bv pcd =
  add_constructor_arguments bv pcd.pcd_args;
  Misc.may (add_type bv) pcd.pcd_res

let add_type_declaration bv td =
  List.iter
    (fun (ty1, ty2, _) -> add_type bv ty1; add_type bv ty2)
    td.ptype_cstrs;
  add_opt add_type bv td.ptype_manifest;
  let add_tkind = function
    Ptype_abstract -> ()
  | Ptype_variant cstrs ->
      List.iter (add_constructor_decl bv) cstrs
  | Ptype_record lbls ->
      List.iter (fun pld -> add_type bv pld.pld_type) lbls
  | Ptype_open -> () in
  add_tkind td.ptype_kind

let add_extension_constructor bv ext =
  match ext.pext_kind with
    Pext_decl(args, rty) ->
      add_constructor_arguments bv args;
      Misc.may (add_type bv) rty
  | Pext_rebind lid -> add bv lid

let add_type_extension bv te =
  add bv te.ptyext_path;
  List.iter (add_extension_constructor bv) te.ptyext_constructors

let rec add_class_type bv cty =
  match cty.pcty_desc with
    Pcty_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcty_signature { pcsig_self = ty; pcsig_fields = fieldl } ->
      add_type bv ty;
      List.iter (add_class_type_field bv) fieldl
  | Pcty_arrow(_, ty1, cty2) ->
      add_type bv ty1; add_class_type bv cty2
  | Pcty_extension e -> handle_extension e
  | Pcty_open (_ovf, m, e) ->
      let bv = open_module bv m.txt in add_class_type bv e

and add_class_type_field bv pctf =
  match pctf.pctf_desc with
    Pctf_inherit cty -> add_class_type bv cty
  | Pctf_val(_, _, _, ty) -> add_type bv ty
  | Pctf_method(_, _, _, ty) -> add_type bv ty
  | Pctf_constraint(ty1, ty2) -> add_type bv ty1; add_type bv ty2
  | Pctf_attribute _ -> ()
  | Pctf_extension e -> handle_extension e

let add_class_description bv infos =
  add_class_type bv infos.pci_expr

let add_class_type_declaration = add_class_description

let pattern_bv = ref StringMap.empty

let rec add_pattern bv pat =
  match pat.ppat_desc with
    Ppat_any -> ()
  | Ppat_var _ -> ()
  | Ppat_alias(p, _) -> add_pattern bv p
  | Ppat_interval _
  | Ppat_constant _ -> ()
  | Ppat_tuple pl -> List.iter (add_pattern bv) pl
  | Ppat_construct(c, op) -> add bv c; add_opt add_pattern bv op
  | Ppat_record(pl, _) ->
      List.iter (fun (lbl, p) -> add bv lbl; add_pattern bv p) pl
  | Ppat_array pl -> List.iter (add_pattern bv) pl
  | Ppat_or(p1, p2) -> add_pattern bv p1; add_pattern bv p2
  | Ppat_constraint(p, ty) -> add_pattern bv p; add_type bv ty
  | Ppat_variant(_, op) -> add_opt add_pattern bv op
  | Ppat_type li -> add bv li
  | Ppat_lazy p -> add_pattern bv p
  | Ppat_unpack id -> pattern_bv := StringMap.add id.txt bound !pattern_bv
  | Ppat_open ( m, p) -> let bv = open_module bv m.txt in add_pattern bv p
  | Ppat_exception p -> add_pattern bv p
  | Ppat_extension e -> handle_extension e

let add_pattern bv pat =
  pattern_bv := bv;
  add_pattern bv pat;
  !pattern_bv

let rec add_expr bv exp =
  match exp.pexp_desc with
    Pexp_ident l -> add bv l
  | Pexp_constant _ -> ()
  | Pexp_let(rf, pel, e) ->
      let bv = add_bindings rf bv pel in add_expr bv e
  | Pexp_fun (_, opte, p, e) ->
      add_opt add_expr bv opte; add_expr (add_pattern bv p) e
  | Pexp_function pel ->
      add_cases bv pel
  | Pexp_apply(e, el) ->
      add_expr bv e; List.iter (fun (_,e) -> add_expr bv e) el
  | Pexp_match(e, pel) -> add_expr bv e; add_cases bv pel
  | Pexp_try(e, pel) -> add_expr bv e; add_cases bv pel
  | Pexp_tuple el -> List.iter (add_expr bv) el
  | Pexp_construct(c, opte) -> add bv c; add_opt add_expr bv opte
  | Pexp_variant(_, opte) -> add_opt add_expr bv opte
  | Pexp_record(lblel, opte) ->
      List.iter (fun (lbl, e) -> add bv lbl; add_expr bv e) lblel;
      add_opt add_expr bv opte
  | Pexp_field(e, fld) -> add_expr bv e; add bv fld
  | Pexp_setfield(e1, fld, e2) -> add_expr bv e1; add bv fld; add_expr bv e2
  | Pexp_array el -> List.iter (add_expr bv) el
  | Pexp_ifthenelse(e1, e2, opte3) ->
      add_expr bv e1; add_expr bv e2; add_opt add_expr bv opte3
  | Pexp_sequence(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_while(e1, e2) -> add_expr bv e1; add_expr bv e2
  | Pexp_for( _, e1, e2, _, e3) ->
      add_expr bv e1; add_expr bv e2; add_expr bv e3
  | Pexp_coerce(e1, oty2, ty3) ->
      add_expr bv e1;
      add_opt add_type bv oty2;
      add_type bv ty3
  | Pexp_constraint(e1, ty2) ->
      add_expr bv e1;
      add_type bv ty2
  | Pexp_send(e, _m) -> add_expr bv e
  | Pexp_new li -> add bv li
  | Pexp_setinstvar(_v, e) -> add_expr bv e
  | Pexp_override sel -> List.iter (fun (_s, e) -> add_expr bv e) sel
  | Pexp_letmodule(id, m, e) ->
      let b = add_module_binding bv m in
      add_expr (StringMap.add id.txt b bv) e
  | Pexp_letexception(_, e) -> add_expr bv e
  | Pexp_assert (e) -> add_expr bv e
  | Pexp_lazy (e) -> add_expr bv e
  | Pexp_poly (e, t) -> add_expr bv e; add_opt add_type bv t
  | Pexp_object { pcstr_self = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
  | Pexp_newtype (_, e) -> add_expr bv e
  | Pexp_pack m -> add_module bv m
  | Pexp_open (_ovf, m, e) ->
      let bv = open_module bv m.txt in add_expr bv e
  | Pexp_extension (({ txt = ("ocaml.extension_constructor"|
                              "extension_constructor"); _ },
                     PStr [item]) as e) ->
      begin match item.pstr_desc with
      | Pstr_eval ({ pexp_desc = Pexp_construct (c, None) }, _) -> add bv c
      | _ -> handle_extension e
      end
  | Pexp_extension e -> handle_extension e
  | Pexp_unreachable -> ()

and add_cases bv cases =
  List.iter (add_case bv) cases

and add_case bv {pc_lhs; pc_guard; pc_rhs} =
  let bv = add_pattern bv pc_lhs in
  add_opt add_expr bv pc_guard;
  add_expr bv pc_rhs

and add_bindings recf bv pel =
  let bv' = List.fold_left (fun bv x -> add_pattern bv x.pvb_pat) bv pel in
  let bv = if recf = Recursive then bv' else bv in
  List.iter (fun x -> add_expr bv x.pvb_expr) pel;
  bv'

and add_modtype bv mty =
  match mty.pmty_desc with
    Pmty_ident l -> add bv l
  | Pmty_alias l -> addmodule bv l
  | Pmty_signature s -> add_signature bv s
  | Pmty_functor(id, mty1, mty2) ->
      Misc.may (add_modtype bv) mty1;
      add_modtype (StringMap.add id.txt bound bv) mty2
  | Pmty_with(mty, cstrl) ->
      add_modtype bv mty;
      List.iter
        (function
          | Pwith_type (_, td) -> add_type_declaration bv td
          | Pwith_module (_, lid) -> addmodule bv lid
          | Pwith_typesubst (_, td) -> add_type_declaration bv td
          | Pwith_modsubst (_, lid) -> addmodule bv lid
        )
        cstrl
  | Pmty_typeof m -> add_module bv m
  | Pmty_extension e -> handle_extension e

and add_module_alias bv l =
  try
    add_parent bv l;
    lookup_map l.txt bv
  with Not_found ->
    match l.txt with
      Lident s -> make_leaf s
    | _ -> addmodule bv l; bound (* cannot delay *)

and add_modtype_binding bv mty =
  if not !Clflags.transparent_modules then add_modtype bv mty;
  match mty.pmty_desc with
    Pmty_alias l ->
      add_module_alias bv l
  | Pmty_signature s ->
      make_node (add_signature_binding bv s)
  | Pmty_typeof modl ->
      add_module_binding bv modl
  | _ ->
      if !Clflags.transparent_modules then add_modtype bv mty; bound

and add_signature bv sg =
  ignore (add_signature_binding bv sg)

and add_signature_binding bv sg =
  snd (List.fold_left add_sig_item (bv, StringMap.empty) sg)

and add_sig_item (bv, m) item =
  match item.psig_desc with
    Psig_value vd ->
      add_type bv vd.pval_type; (bv, m)
  | Psig_type (_, dcls) ->
      List.iter (add_type_declaration bv) dcls; (bv, m)
  | Psig_typext te ->
      add_type_extension bv te; (bv, m)
  | Psig_exception pext ->
      add_extension_constructor bv pext; (bv, m)
  | Psig_module pmd ->
      let m' = add_modtype_binding bv pmd.pmd_type in
      let add = StringMap.add pmd.pmd_name.txt m' in
      (add bv, add m)
  | Psig_recmodule decls ->
      let add =
        List.fold_right (fun pmd -> StringMap.add pmd.pmd_name.txt bound)
                        decls
      in
      let bv' = add bv and m' = add m in
      List.iter (fun pmd -> add_modtype bv' pmd.pmd_type) decls;
      (bv', m')
  | Psig_modtype x ->
      begin match x.pmtd_type with
        None -> ()
      | Some mty -> add_modtype bv mty
      end;
      (bv, m)
  | Psig_open od ->
      (open_module bv od.popen_lid.txt, m)
  | Psig_include incl ->
      let Node (s, m') = add_modtype_binding bv incl.pincl_mod in
      add_names s;
      let add = StringMap.fold StringMap.add m' in
      (add bv, add m)
  | Psig_class cdl ->
      List.iter (add_class_description bv) cdl; (bv, m)
  | Psig_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; (bv, m)
  | Psig_attribute _ -> (bv, m)
  | Psig_extension (e, _) ->
      handle_extension e;
      (bv, m)

and add_module_binding bv modl =
  if not !Clflags.transparent_modules then add_module bv modl;
  match modl.pmod_desc with
    Pmod_ident l ->
      begin try
        add_parent bv l;
        lookup_map l.txt bv
      with Not_found ->
        match l.txt with
          Lident s -> make_leaf s
        | _ ->  addmodule bv l; bound
      end
  | Pmod_structure s ->
      make_node (snd (add_structure_binding bv s))
  | _ ->
      if !Clflags.transparent_modules then add_module bv modl; bound

and add_module bv modl =
  match modl.pmod_desc with
    Pmod_ident l -> addmodule bv l
  | Pmod_structure s -> ignore (add_structure bv s)
  | Pmod_functor(id, mty, modl) ->
      Misc.may (add_modtype bv) mty;
      add_module (StringMap.add id.txt bound bv) modl
  | Pmod_apply(mod1, mod2) ->
      add_module bv mod1; add_module bv mod2
  | Pmod_constraint(modl, mty) ->
      add_module bv modl; add_modtype bv mty
  | Pmod_unpack(e) ->
      add_expr bv e
  | Pmod_extension e ->
      handle_extension e

and add_structure bv item_list =
  let (bv, m) = add_structure_binding bv item_list in
  add_names (collect_free (make_node m));
  bv

and add_structure_binding bv item_list =
  List.fold_left add_struct_item (bv, StringMap.empty) item_list

and add_struct_item (bv, m) item : _ StringMap.t * _ StringMap.t =
  match item.pstr_desc with
    Pstr_eval (e, _attrs) ->
      add_expr bv e; (bv, m)
  | Pstr_value(rf, pel) ->
      let bv = add_bindings rf bv pel in (bv, m)
  | Pstr_primitive vd ->
      add_type bv vd.pval_type; (bv, m)
  | Pstr_type (_, dcls) ->
      List.iter (add_type_declaration bv) dcls; (bv, m)
  | Pstr_typext te ->
      add_type_extension bv te;
      (bv, m)
  | Pstr_exception pext ->
      add_extension_constructor bv pext; (bv, m)
  | Pstr_module x ->
      let b = add_module_binding bv x.pmb_expr in
      let add = StringMap.add x.pmb_name.txt b in
      (add bv, add m)
  | Pstr_recmodule bindings ->
      let add =
        List.fold_right (fun x -> StringMap.add x.pmb_name.txt bound) bindings
      in
      let bv' = add bv and m = add m in
      List.iter
        (fun x -> add_module bv' x.pmb_expr)
        bindings;
      (bv', m)
  | Pstr_modtype x ->
      begin match x.pmtd_type with
        None -> ()
      | Some mty -> add_modtype bv mty
      end;
      (bv, m)
  | Pstr_open od ->
      (open_module bv od.popen_lid.txt, m)
  | Pstr_class cdl ->
      List.iter (add_class_declaration bv) cdl; (bv, m)
  | Pstr_class_type cdtl ->
      List.iter (add_class_type_declaration bv) cdtl; (bv, m)
  | Pstr_include incl ->
      let Node (s, m') = add_module_binding bv incl.pincl_mod in
      add_names s;
      let add = StringMap.fold StringMap.add m' in
      (add bv, add m)
  | Pstr_attribute _ -> (bv, m)
  | Pstr_extension (e, _) ->
      handle_extension e;
      (bv, m)

and add_use_file bv top_phrs =
  ignore (List.fold_left add_top_phrase bv top_phrs)

and add_implementation bv l =
  if !Clflags.transparent_modules then
    ignore (add_structure_binding bv l)
  else ignore (add_structure bv l)

and add_implementation_binding bv l =
  snd (add_structure_binding bv l)

and add_top_phrase bv = function
  | Ptop_def str -> add_structure bv str
  | Ptop_dir (_, _) -> bv

and add_class_expr bv ce =
  match ce.pcl_desc with
    Pcl_constr(l, tyl) ->
      add bv l; List.iter (add_type bv) tyl
  | Pcl_structure { pcstr_self = pat; pcstr_fields = fieldl } ->
      let bv = add_pattern bv pat in List.iter (add_class_field bv) fieldl
  | Pcl_fun(_, opte, pat, ce) ->
      add_opt add_expr bv opte;
      let bv = add_pattern bv pat in add_class_expr bv ce
  | Pcl_apply(ce, exprl) ->
      add_class_expr bv ce; List.iter (fun (_,e) -> add_expr bv e) exprl
  | Pcl_let(rf, pel, ce) ->
      let bv = add_bindings rf bv pel in add_class_expr bv ce
  | Pcl_constraint(ce, ct) ->
      add_class_expr bv ce; add_class_type bv ct
  | Pcl_extension e -> handle_extension e
  | Pcl_open (_ovf, m, e) ->
      let bv = open_module bv m.txt in add_class_expr bv e

and add_class_field bv pcf =
  match pcf.pcf_desc with
    Pcf_inherit(_, ce, _) -> add_class_expr bv ce
  | Pcf_val(_, _, Cfk_concrete (_, e))
  | Pcf_method(_, _, Cfk_concrete (_, e)) -> add_expr bv e
  | Pcf_val(_, _, Cfk_virtual ty)
  | Pcf_method(_, _, Cfk_virtual ty) -> add_type bv ty
  | Pcf_constraint(ty1, ty2) -> add_type bv ty1; add_type bv ty2
  | Pcf_initializer e -> add_expr bv e
  | Pcf_attribute _ -> ()
  | Pcf_extension e -> handle_extension e

and add_class_declaration bv decl =
  add_class_expr bv decl.pci_expr

end
module Ext_format : sig 
#1 "ext_format.mli"
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








(** Simplified wrapper module for the standard library [Format] module. 
  *) 

type t = private Format.formatter

val string : t -> string -> unit

val break : t -> unit

val break1 : t -> unit

val space :  t -> unit

val group : t -> int -> (unit -> 'a) -> 'a
(** [group] will record current indentation 
    and indent futher
 *)

val vgroup : t -> int -> (unit -> 'a) -> 'a

val paren : t -> (unit -> 'a) -> 'a

val paren_group : t -> int -> (unit -> 'a) -> 'a

val brace_group : t -> int -> (unit -> 'a) -> 'a

val brace_vgroup : t -> int -> (unit -> 'a) -> 'a

val bracket_group : t -> int -> (unit -> 'a) -> 'a

val newline : t -> unit

val to_out_channel : out_channel -> t

val flush : t -> unit -> unit

val pp_print_queue :
  ?pp_sep:(Format.formatter -> unit -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Queue.t -> unit

end = struct
#1 "ext_format.ml"
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








open Format

type t = formatter

let string = pp_print_string

let break = fun fmt -> pp_print_break fmt 0 0

let break1 =
  fun fmt -> pp_print_break fmt 0 1 

let space  fmt  = 
  pp_print_break fmt 1 0

let vgroup fmt indent u = 
  pp_open_vbox fmt indent; 
  let v = u () in
  pp_close_box fmt ();
  v

let group fmt indent u = 
  pp_open_hovbox fmt indent; 
  let v = u () in
  pp_close_box fmt ();
  v
  
let paren fmt u = 
  string fmt "(";
  let v = u () in
  string fmt ")";
  v

let brace fmt u = 
  string fmt "{";
  (* break1 fmt ; *)
  let v = u () in
  string fmt "}";
  v 

let bracket fmt u = 
  string fmt "[";
  let v = u () in
  string fmt "]";
  v 

let paren_group st n action = 
  group st n (fun _ -> paren st action)

let brace_group st n action = 
  group st n (fun _ -> brace st action )

let brace_vgroup st n action = 
  vgroup st n (fun _ -> 
    string st "{";
    pp_print_break st 0 2;
    let v = vgroup st 0 action in
    pp_print_break st 0 0;
    string st "}";
    v
              )
let bracket_group st n action = 
  group st n (fun _ -> bracket st action)

let newline fmt = pp_print_newline fmt ()

let to_out_channel = formatter_of_out_channel

(* let non_breaking_space  fmt = string fmt " " *)
(* let set_needed_space_function _ _ = () *)
let flush = pp_print_flush

let list = pp_print_list

let rec pp_print_queue ?(pp_sep = pp_print_cut) pp_v ppf q =
  Queue.iter (fun q -> pp_v ppf q ;  pp_sep ppf ()) q 

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

val try_it : (unit -> 'a) ->  unit 

val with_file_as_chan : string -> (out_channel -> 'a) -> 'a

val with_file_as_pp : string -> (Format.formatter -> 'a) -> 'a

val is_pos_pow : Int32.t -> int

val failwithf : loc:string -> ('a, unit, string, 'b) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val bad_argf : ('a, unit, string, 'b) format4 -> 'a




external id : 'a -> 'a = "%identity"

(** Copied from {!Btype.hash_variant}:
    need sync up and add test case
 *)
val hash_variant : string -> int

val todo : string -> 'a
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

let try_it f  =   
  try ignore (f ()) with _ -> ()

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

let todo loc = 
  failwith (loc ^ " Not supported yet")
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

val for_all : 
  string -> 
  (char -> bool) -> 
  bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

(**
  [extract_until s cursor sep]
   When [sep] not found, the cursor is updated to -1,
   otherwise cursor is increased to 1 + [sep_position]
   User can not determine whether it is found or not by
   telling the return string is empty since 
   "\n\n" would result in an empty string too.
*)
val extract_until:
  string -> 
  int ref -> (* cursor to be updated *)
  char -> 
  string

val index_count:  
  string -> 
  int ->
  char -> 
  int -> 
  int 

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

val no_slash_idx_from : string -> int -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;  
  
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

val uncapitalize_ascii : string -> string

val lowercase_ascii : string -> string 
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


let for_all s (p : char -> bool)  =   
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

let rec index_rec s lim i c =
  if i >= lim then -1 else
  if String.unsafe_get s i = c then i 
  else index_rec s lim (i + 1) c

let rec index_rec_count s lim i c count =
  if i >= lim then -1 else
  if String.unsafe_get s i = c then 
    if count = 1 then i 
    else index_rec_count s lim (i + 1) c (count - 1)
  else index_rec_count s lim (i + 1) c count

let index_count s i c count =     
  let lim = String.length s in 
  if i < 0 || i >= lim || count < 1 then 
    Ext_pervasives.invalid_argf "index_count: (%d,%d)"  i count;

  index_rec_count s lim i c count 
let extract_until s cursor c =       
  let len = String.length s in   
  let start = !cursor in 
  if start < 0 || start >= len then (
    cursor := -1;
    ""
    )
  else 
    let i = index_rec s len start c in   
    let finish = 
      if i < 0 then (      
        cursor := -1 ;
        len 
      )
      else (
        cursor := i + 1;
        i 
      ) in 
    String.sub s start (finish - start)
  
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

let no_slash_idx_from x from = 
  let last_idx = String.length x - 1  in 
  assert (from >= 0); 
  unsafe_no_char_idx x '/' from last_idx

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


external compare : string -> string -> int = "caml_string_length_based_compare" [@@noalloc];;    

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

let uncapitalize_ascii =

    String.uncapitalize_ascii
      


 
let lowercase_ascii = String.lowercase_ascii





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


val map : 'a list -> ('a -> 'b) ->  'b list 

val mapi :
  'a list -> 
  (int -> 'a -> 'b) -> 
  'b list 
  
val map_snd : ('a * 'b) list -> ('b -> 'c) -> ('a * 'c) list 

(** [map_last f xs ]
    will pass [true] to [f] for the last element, 
    [false] otherwise. 
    For empty list, it returns empty
*)
val map_last : 
    'a list -> 
    (bool -> 'a -> 'b) -> 'b list

(** [last l]
    return the last element
    raise if the list is empty
*)
val last : 'a list -> 'a

val append : 
  'a list -> 
  'a list -> 
  'a list 

val append_one :  
  'a list -> 
  'a -> 
  'a list

val map_append :  
  'b list -> 
  'a list -> 
  ('b -> 'a) -> 
  'a list

val fold_right : 
  'a list -> 'b -> 
  ('a -> 'b -> 'b) -> 'b

val fold_right2 : 
  'a list -> 
  'b list -> 
  'c -> 
  ('a -> 'b -> 'c -> 'c) ->  'c

val map2 : 
  'a list ->
  'b list ->
  ('a -> 'b -> 'c) ->
  'c list

val fold_left_with_offset : 
  'a list -> 
  'acc -> 
  int -> 
  ('a -> 'acc ->  int ->  'acc) ->   
  'acc 


(** @unused *)
val filter_map : 
  'a list -> 
  ('a -> 'b option) -> 
  'b list  

(** [exclude p l] is the opposite of [filter p l] *)
val exclude : 
  'a list -> 
  ('a -> bool) -> 
  'a list 

(** [excludes p l]
    return a tuple [excluded,newl]
    where [exluded] is true indicates that at least one  
    element is removed,[newl] is the new list where all [p x] for [x] is false

*)
val exclude_with_val : 
  'a list -> 
  ('a -> bool) -> 
  bool * 'a list 


val same_length : 'a list -> 'b list -> bool

val init : int -> (int -> 'a) -> 'a list

(** [split_at n l]
    will split [l] into two lists [a,b], [a] will be of length [n], 
    otherwise, it will raise
*)
val split_at : 
  'a list -> 
  int -> 
  'a list * 'a list


(** [split_at_last l]
    It is equivalent to [split_at (List.length l - 1) l ]
*)
val split_at_last : 'a list -> 'a list * 'a

val filter_mapi : 
  'a list -> 
  ('a -> int ->  'b option) -> 
  'b list

val filter_map2 : 
  'a list -> 
  'b list -> 
  ('a -> 'b -> 'c option) -> 
  'c list


val length_compare : 'a list -> int -> [`Gt | `Eq | `Lt ]

val length_ge : 'a list -> int -> bool

(**

   {[length xs = length ys + n ]}
   input n should be positive 
   TODO: input checking
*)

val length_larger_than_n : 
  'a list -> 
  'a list -> 
   int -> 
   bool


(**
   [rev_map_append f l1 l2]
   [map f l1] and reverse it to append [l2]
   This weird semantics is due to it is the most efficient operation
   we can do
*)
val rev_map_append : 
  'a list -> 
  'b list -> 
  ('a -> 'b) -> 
  'b list


val flat_map : 
  'a list -> 
  ('a -> 'b list) -> 
  'b list

val flat_map_append : 
  'a list -> 
  'b list  ->
  ('a -> 'b list) -> 
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
val stable_group : 
  'a list -> 
  ('a -> 'a -> bool) -> 
  'a list list 

(** [drop n list]
    raise when [n] is negative
    raise when list's length is less than [n]
*)
val drop : 
  'a list -> 
  int -> 
  'a list 

(** [find_first_not p lst ]
    if all elements in [lst] pass, return [None] 
    otherwise return the first element [e] as [Some e] which
    fails the predicate
*)
val find_first_not : 
  'a list -> 
  ('a -> bool) -> 
  'a option 

(** [find_opt f l] returns [None] if all return [None],  
    otherwise returns the first one. 
*)

val find_opt : 
  'a list -> 
  ('a -> 'b option) -> 
  'b option 


val rev_iter : 
  'a list -> 
  ('a -> unit) -> 
  unit 

val iter:   
   'a list ->  
   ('a -> unit) -> 
   unit
   
val for_all:  
    'a list -> 
    ('a -> bool) -> 
    bool
val for_all_snd:    
    ('a * 'b) list -> 
    ('b -> bool) -> 
    bool

(** [for_all2_no_exn p xs ys]
    return [true] if all satisfied,
    [false] otherwise or length not equal
*)
val for_all2_no_exn : 
  'a list -> 
  'b list -> 
  ('a -> 'b -> bool) -> 
  bool



(** [f] is applied follow the list order *)
val split_map : 
  'a list -> 
  ('a -> 'b * 'c) -> 
  'b list * 'c list       

(** [fn] is applied from left to right *)
val reduce_from_left : 
  'a list -> 
  ('a -> 'a -> 'a) ->
  'a

val sort_via_array :
  'a list -> 
  ('a -> 'a -> int) -> 
  'a list  




(** [assoc_by_string default key lst]
    if  [key] is found in the list  return that val,
    other unbox the [default], 
    otherwise [assert false ]
*)
val assoc_by_string : 
  (string * 'a) list -> 
  string -> 
  'a  option ->   
  'a  

val assoc_by_int : 
  (int * 'a) list -> 
  int -> 
  'a  option ->   
  'a   


val nth_opt : 'a list -> int -> 'a option  

val iter_snd : ('a * 'b) list -> ('b -> unit) -> unit 

val iter_fst : ('a * 'b) list -> ('a -> unit) -> unit 

val exists : 'a list -> ('a -> bool) -> bool 
val exists_snd : ('a * 'b) list -> ('b -> bool) -> bool

val concat_append:
    'a list list -> 
    'a list -> 
    'a list

val fold_left2:
    'a list -> 
    'b list -> 
    'c -> 
    ('a -> 'b -> 'c -> 'c)
    -> 'c 

val fold_left:    
    'a list -> 
    'b -> 
    ('a -> 'b -> 'b) -> 
    'b

val singleton_exn:     
    'a list -> 'a
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




let rec map l f =
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
    y1::y2::y3::y4::y5::(map tail f)


let rec map_snd l f =
  match l with
  | [] ->
    []
  | [ v1,x1 ] ->
    let y1 = f x1 in
    [v1,y1]
  | [v1, x1; v2, x2] ->
    let y1 = f x1 in
    let y2 = f x2 in
    [v1, y1; v2, y2]
  | [ v1, x1; v2, x2; v3, x3] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    [v1, y1; v2, y2; v3, y3]
  | [ v1, x1; v2, x2; v3, x3; v4, x4] ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    [v1, y1; v2, y2; v3, y3; v4, y4]
  | (v1, x1) ::(v2, x2) :: (v3, x3)::(v4, x4) :: (v5, x5) ::tail ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    (v1, y1)::(v2, y2) :: (v3, y3) :: (v4, y4) :: (v5, y5) :: (map_snd tail f)


let rec map_last l f=
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
    y1::y2::y3::y4::(map_last tail f)

let rec mapi_aux lst i f = 
  match lst with
    [] -> []
  | a::l -> 
    let r = f i a in r :: mapi_aux l (i + 1) f 

let mapi lst f = mapi_aux lst 0 f

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

let append_one l1 x = append_aux l1 [x]  

let rec map_append l1 l2 f =   
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
    b0::b1::b2::b3::b4::map_append rest l2 f



let rec fold_right l acc f  = 
  match l with  
  | [] -> acc 
  | [a0] -> f a0 acc 
  | [a0;a1] -> f a0 (f a1 acc)
  | [a0;a1;a2] -> f a0 (f a1 (f a2 acc))
  | [a0;a1;a2;a3] -> f a0 (f a1 (f a2 (f a3 acc))) 
  | [a0;a1;a2;a3;a4] -> 
    f a0 (f a1 (f a2 (f a3 (f a4 acc))))
  | a0::a1::a2::a3::a4::rest -> 
    f a0 (f a1 (f a2 (f a3 (f a4 (fold_right rest acc f )))))  

let rec fold_right2 l r acc f = 
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
    f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 (fold_right2 arest brest acc f )))))  
  | _, _ -> invalid_arg "Ext_list.fold_right2"

let rec map2  l r f = 
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
    c0::c1::c2::c3::c4::map2 arest brest f
  | _, _ -> invalid_arg "Ext_list.map2"

let rec fold_left_with_offset l accu i f =
  match l with
  | [] -> accu
  | a::l -> 
    fold_left_with_offset 
    l     
    (f  a accu  i)  
    (i + 1)
    f  


let rec filter_map xs (f: 'a -> 'b option)= 
  match xs with 
  | [] -> []
  | y :: ys -> 
    begin match f y with 
      | None -> filter_map ys f 
      | Some z -> z :: filter_map ys f 
    end

let rec exclude xs p =   
  match xs with 
  | [] ->  []
  | x::xs -> 
    if p x then exclude xs p
    else x:: exclude xs p

let rec exclude_with_val l p =
  match l with 
  | [] ->  false, l
  | a0::xs -> 
    if p a0 then true, exclude xs p
    else 
      match xs with 
      | [] -> false, l 
      | a1::rest -> 
        if p a1 then 
          true, a0:: exclude rest p
        else 
          let st,rest = exclude_with_val rest p in 
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

let split_at l n = 
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
let  filter_mapi xs f  = 
  let rec aux i xs = 
    match xs with 
    | [] -> []
    | y :: ys -> 
      begin match f y i with 
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys
      end in
  aux 0 xs 

let rec filter_map2  xs ys (f: 'a -> 'b -> 'c option) = 
  match xs,ys with 
  | [],[] -> []
  | u::us, v :: vs -> 
    begin match f u v with 
      | None -> filter_map2 us vs f (* idea: rec f us vs instead? *)
      | Some z -> z :: filter_map2  us vs f
    end
  | _ -> invalid_arg "Ext_list.filter_map2"


let rec rev_map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | a :: l -> rev_map_append l (f a :: l2) f


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

let flat_map lx f  =
  flat_map_aux f [] [] lx

let flat_map_append lx append f =
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
let rec length_larger_than_n xs ys n =
  match xs, ys with 
  | _, [] -> length_compare xs n = `Eq   
  | _::xs, _::ys -> 
    length_larger_than_n xs ys n
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

let stable_group lst eq =  group eq lst |> List.rev  

let rec drop h n = 
  if n < 0 then invalid_arg "Ext_list.drop"
  else
  if n = 0 then h 
  else 
    match h with 
    | [] ->
      invalid_arg "Ext_list.drop"
    | _ :: tl ->   
      drop tl (n - 1)

let rec find_first_not  xs p = 
  match xs with 
  | [] -> None
  | a::l -> 
    if p a 
    then find_first_not l p 
    else Some a 


let rec rev_iter l f = 
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
    rev_iter tail f;
    f x5; f x4 ; f x3; f x2 ; f x1

let rec iter l f = 
  match l with
  | [] -> ()    
  | [x1] ->
    f x1 
  | [x1; x2] ->
    f x1 ; f x2
  | [x1; x2; x3] ->
    f x1 ; f x2 ; f x3
  | [x1; x2; x3; x4] ->
    f x1; f x2; f x3; f x4
  | x1::x2::x3::x4::x5::tail ->
    f x1; f x2 ; f x3; f x4 ; f x5;
    iter tail f 


let rec for_all lst p = 
  match lst with 
    [] -> true
  | a::l -> p a && for_all l p

let rec for_all_snd lst p = 
  match lst with 
    [] -> true
  | (_,a)::l -> p a && for_all_snd l p


let rec for_all2_no_exn  l1 l2 p = 
  match (l1, l2) with
  | ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2_no_exn l1 l2 p
  | (_, _) -> false


let rec find_opt xs p = 
  match xs with 
  | [] -> None
  | x :: l -> 
    match  p x with 
    | Some _ as v  ->  v
    | None -> find_opt l p



let rec split_map l f = 
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
    let ass,bss = split_map tail f in 
    a1::a2::a3::a4::a5::ass,
    b1::b2::b3::b4::b5::bss


let reduce_from_left lst fn = 
  match lst with 
  | first :: rest ->  List.fold_left fn first rest 
  | _ -> invalid_arg "Ext_list.reduce_from_left"


let sort_via_array lst cmp =
  let arr = Array.of_list lst  in
  Array.sort cmp arr;
  Array.to_list arr




let rec assoc_by_string lst (k : string) def  = 
  match lst with 
  | [] -> 
    begin match def with 
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if Ext_string.equal k1 k then v1 else 
      assoc_by_string  rest k def 

let rec assoc_by_int lst (k : int) def = 
  match lst with 
  | [] -> 
    begin match def with
      | None -> assert false 
      | Some x -> x end
  | (k1,v1)::rest -> 
    if k1 = k then v1 else 
      assoc_by_int rest k def 


let rec nth_aux l n =
  match l with
  | [] -> None
  | a::l -> if n = 0 then Some a else nth_aux l (n-1)

let nth_opt l n =
  if n < 0 then None 
  else
    nth_aux l n

let rec iter_snd lst f =     
  match lst with
  | [] -> ()
  | (_,x)::xs -> 
    f x ; 
    iter_snd xs f 
    
let rec iter_fst lst f =     
  match lst with
  | [] -> ()
  | (x,_)::xs -> 
    f x ; 
    iter_fst xs f 

let rec exists l p =     
  match l with 
    [] -> false  
  | x :: xs -> p x || exists xs p

let rec exists_snd l p = 
  match l with 
    [] -> false
  | (_, a)::l -> p a || exists_snd l p 

let rec concat_append 
  (xss : 'a list list)  
  (xs : 'a list) : 'a list = 
  match xss with 
  | [] -> xs 
  | l::r -> append l (concat_append r xs)

let rec fold_left l accu f =
  match l with
    [] -> accu
  | a::l -> fold_left l (f a accu) f 

let rec fold_left2 l1 l2 accu f =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2  l1 l2 (f a1 a2 accu) f 
  | (_, _) -> invalid_arg "List.fold_left2"

let singleton_exn xs = match xs with [x] -> x | _ -> assert false
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

val uppercase_ascii : char -> char

val lowercase_ascii : char -> char
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





(** {!Char.escaped} is locale sensitive in 4.02.3, fixed in the trunk,
    backport it here
 *)
 
let escaped = Char.escaped


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
let uppercase_ascii =

    Char.uppercase_ascii
      

let lowercase_ascii = 

    Char.lowercase_ascii
      

end
module Ext_sys : sig 
#1 "ext_sys.mli"
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


(* Not used yet *)
(* val is_directory_no_exn : string -> bool *)


val is_windows_or_cygwin : bool 

val getenv_opt : 
  string -> 
  string option 
end = struct
#1 "ext_sys.ml"
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

(** TODO: not exported yet, wait for Windows Fix*)
let is_directory_no_exn f = 
  try Sys.is_directory f with _ -> false 


let is_windows_or_cygwin = Sys.win32 || Sys.cygwin


let getenv_opt = Sys.getenv_opt

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
val suffix_js : string
val suffix_bs_js : string 
(* val suffix_re_js : string *)
val suffix_gen_js : string 
val suffix_gen_tsx: string

val suffix_tsx : string
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
let suffix_js = ".js"
let suffix_bs_js = ".bs.js"
(* let suffix_re_js = ".re.js" *)
let suffix_gen_js = ".gen.js"
let suffix_gen_tsx = ".gen.tsx"
let suffix_tsx = ".tsx"

let commonjs = "commonjs" 
let amdjs = "amdjs"
let es6 = "es6"
let es6_global = "es6-global"
let amdjs_global = "amdjs-global"
let unused_attribute = "Unused attribute " 
let dash_nostdlib = "-nostdlib"

let reactjs_jsx_ppx_2_exe = "reactjs_jsx_ppx_2.exe"
let reactjs_jsx_ppx_3_exe  = "reactjs_jsx_ppx_3.exe"

let native = "native"
let bytecode = "bytecode"
let js = "js"



(** Used when produce node compatible paths *)
let node_sep = "/"
let node_parent = ".."
let node_current = "."


end
module Ext_path : sig 
#1 "ext_path.mli"
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

type t 





(**
   [combine path1 path2]
   1. add some simplifications when concatenating
   2. when [path2] is absolute, return [path2]
*)  
val combine : 
  string -> 
  string -> 
  string    



val chop_extension : ?loc:string -> string -> string 


val chop_extension_if_any : string -> string

val chop_all_extensions_if_any : 
  string -> string 

(**
   {[
     get_extension "a.txt" = ".txt"
       get_extension "a" = ""
   ]}
*)
val get_extension : string -> string




val node_rebase_file :
  from:string -> 
  to_:string ->
  string -> 
  string 

(** 
   TODO: could be highly optimized
   if [from] and [to] resolve to the same path, a zero-length string is returned 
   Given that two paths are directory

   A typical use case is 
   {[
     Filename.concat 
       (rel_normalized_absolute_path cwd (Filename.dirname a))
       (Filename.basename a)
   ]}
*)
val rel_normalized_absolute_path : from:string -> string -> string 


val normalize_absolute_path : string -> string 

val absolute_path : string Lazy.t -> string -> string

(** [concat dirname filename]
    The same as {!Filename.concat} except a tiny optimization 
    for current directory simplification
*)
val concat : string -> string -> string 

val check_suffix_case : 
  string -> string -> bool
end = struct
#1 "ext_path.ml"
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

type t = 
  | File of string 
  | Dir of string 






let split_by_sep_per_os : string -> string list = 
  if Ext_sys.is_windows_or_cygwin then 
  fun x -> 
    (* on Windows, we can still accept -bs-package-output lib/js *)
    Ext_string.split_by 
      (fun x -> match x with |'/' |'\\' -> true | _ -> false) x
  else 
  fun x -> Ext_string.split x '/'

(** example
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
    ]}

    The other way
    {[

      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/ocaml_array.ml"
        "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib/external/pervasives.cmj"
    ]}
    {[
      "/bb/mbigc/mbig2899/bgit/bucklescript/jscomp/stdlib//ocaml_array.ml"
    ]}
    {[
      /a/b
      /c/d
    ]}
*)
let node_relative_path 
    ~from:(file_or_dir_2 : t )
    (file_or_dir_1 : t) 
  = 
  let relevant_dir1 = 
    match file_or_dir_1 with 
    | Dir x -> x 
    | File file1 ->  Filename.dirname file1 in
  let relevant_dir2 = 
    match file_or_dir_2 with 
    | Dir x -> x 
    | File file2 -> Filename.dirname file2  in
  let dir1 = split_by_sep_per_os relevant_dir1 in
  let dir2 = split_by_sep_per_os relevant_dir2 in
  let rec go (dir1 : string list) (dir2 : string list) = 
    match dir1, dir2 with 
    | "." :: xs, ys -> go xs ys 
    | xs , "." :: ys -> go xs ys 
    | x::xs , y :: ys when x = y
      -> go xs ys 
    | _, _ -> 
      Ext_list.map_append  dir2  dir1  (fun _ ->  Literals.node_parent)
  in
  match go dir1 dir2 with
  | (x :: _ ) as ys when x = Literals.node_parent -> 
    String.concat Literals.node_sep ys
  | ys -> 
    String.concat Literals.node_sep  
    @@ Literals.node_current :: ys


let node_concat ~dir base =
  dir ^ Literals.node_sep ^ base 

let node_rebase_file ~from ~to_ file = 
  
  node_concat
    ~dir:(
      if from = to_ then Literals.node_current
      else node_relative_path ~from:(Dir from) (Dir to_)) 
    file
    
    
(***
   {[
     Filename.concat "." "";;
     "./"
   ]}
*)
let combine path1 path2 =  
  if Filename.is_relative path2 then
    if Ext_string.is_empty path2 then 
      path1
    else 
    if path1 = Filename.current_dir_name then 
      path2
    else
    if path2 = Filename.current_dir_name 
    then path1
    else
      Filename.concat path1 path2 
  else
    path2


let chop_extension ?(loc="") name =
  try Filename.chop_extension name 
  with Invalid_argument _ -> 
    Ext_pervasives.invalid_argf 
      "Filename.chop_extension ( %s : %s )"  loc name

let chop_extension_if_any fname =
  try Filename.chop_extension fname with Invalid_argument _ -> fname

let rec chop_all_extensions_if_any fname =
  match Filename.chop_extension fname with 
  | x -> chop_all_extensions_if_any x 
  | exception _ -> fname

let get_extension x =
  let pos = Ext_string.rindex_neg x '.' in 
  if pos < 0 then ""
  else Ext_string.tail_from x pos 


let (//) x y =
  if x = Filename.current_dir_name then y
  else if y = Filename.current_dir_name then x 
  else Filename.concat x y 

(**
   {[
     split_aux "//ghosg//ghsogh/";;
     - : string * string list = ("/", ["ghosg"; "ghsogh"])
   ]}
   Note that 
   {[
     Filename.dirname "/a/" = "/"
       Filename.dirname "/a/b/" = Filename.dirname "/a/b" = "/a"
   ]}
   Special case:
   {[
     basename "//" = "/"
       basename "///"  = "/"
   ]}
   {[
     basename "" =  "."
       basename "" = "."
       dirname "" = "."
       dirname "" =  "."
   ]}  
*)
let split_aux p =
  let rec go p acc =
    let dir = Filename.dirname p in
    if dir = p then dir, acc
    else
      let new_path = Filename.basename p in 
      if Ext_string.equal new_path Filename.dir_sep then 
        go dir acc 
        (* We could do more path simplification here
           leave to [rel_normalized_absolute_path]
        *)
      else 
        go dir (new_path :: acc)

  in go p []





(** 
   TODO: optimization
   if [from] and [to] resolve to the same path, a zero-length string is returned 

   This function is useed in [es6-global] and 
   [amdjs-global] format and tailored for `rollup`
*)
let rel_normalized_absolute_path ~from to_ =
  let root1, paths1 = split_aux from in 
  let root2, paths2 = split_aux to_ in 
  if root1 <> root2 then root2
  else
    let rec go xss yss =
      match xss, yss with 
      | x::xs, y::ys -> 
        if Ext_string.equal x  y then go xs ys 
        else if x = Filename.current_dir_name then go xs yss 
        else if y = Filename.current_dir_name then go xss ys
        else 
          let start = 
            List.fold_left (fun acc _ -> acc // Ext_string.parent_dir_lit )
              Ext_string.parent_dir_lit  xs in 
          List.fold_left (fun acc v -> acc // v) start yss
      | [], [] -> Ext_string.empty
      | [], y::ys -> List.fold_left (fun acc x -> acc // x) y ys
      | x::xs, [] ->
        List.fold_left (fun acc _ -> acc // Ext_string.parent_dir_lit )
          Ext_string.parent_dir_lit xs in
    let v =  go paths1 paths2  in 

    if Ext_string.is_empty v then  Literals.node_current
    else 
    if
      v = "."
      || v = ".."
      || Ext_string.starts_with v "./"  
      || Ext_string.starts_with v "../" 
    then v 
    else "./" ^ v 

(*TODO: could be hgighly optimized later 
  {[
    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/a/b/../c../d/e/f";;

    normalize_absolute_path "/gsho/./..";;

    normalize_absolute_path "/gsho/./../..";;

    normalize_absolute_path "/a/b/c/d";;

    normalize_absolute_path "/a/b/c/d/";;

    normalize_absolute_path "/a/";;

    normalize_absolute_path "/a";;
  ]}
*)
(** See tests in {!Ounit_path_tests} *)
let normalize_absolute_path x =
  let drop_if_exist xs =
    match xs with 
    | [] -> []
    | _ :: xs -> xs in 
  let rec normalize_list acc paths =
    match paths with 
    | [] -> acc 
    | x :: xs -> 
      if Ext_string.equal x Ext_string.current_dir_lit then 
        normalize_list acc xs 
      else if Ext_string.equal x Ext_string.parent_dir_lit then 
        normalize_list (drop_if_exist acc ) xs 
      else   
        normalize_list (x::acc) xs 
  in
  let root, paths = split_aux x in
  let rev_paths =  normalize_list [] paths in 
  let rec go acc rev_paths =
    match rev_paths with 
    | [] -> Filename.concat root acc 
    | last::rest ->  go (Filename.concat last acc ) rest  in 
  match rev_paths with 
  | [] -> root 
  | last :: rest -> go last rest 




let absolute_path cwd s = 
  let process s = 
    let s = 
      if Filename.is_relative s then
        Lazy.force cwd // s 
      else s in
    (* Now simplify . and .. components *)
    let rec aux s =
      let base,dir  = Filename.basename s, Filename.dirname s  in
      if dir = s then dir
      else if base = Filename.current_dir_name then aux dir
      else if base = Filename.parent_dir_name then Filename.dirname (aux dir)
      else aux dir // base
    in aux s  in 
  process s 


let absolute cwd s =   
  match s with 
  | File x -> File (absolute_path cwd x )
  | Dir x -> Dir (absolute_path cwd x)

let concat dirname filename =
  if filename = Filename.current_dir_name then dirname
  else if dirname = Filename.current_dir_name then filename
  else Filename.concat dirname filename
  

let check_suffix_case =
  Ext_string.ends_with
end
module Ext_modulename : sig 
#1 "ext_modulename.mli"
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




val module_name_of_file : string -> string


val module_name_of_file_if_any : string -> string

(** [modulename, upper]
  if [upper = true] then it means it is indeed uppercase
*)
val module_name_of_file_if_any_with_upper : string -> string * bool


(** Given an JS bundle name, generate a meaningful
  bounded module name
*)
val js_id_name_of_hint_name : string -> string 
end = struct
#1 "ext_modulename.ml"
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


let module_name_of_file file =
  Ext_string.capitalize_ascii 
    (Filename.chop_extension @@ Filename.basename file)  

let module_name_of_file_if_any file = 
  let v = Ext_path.chop_extension_if_any @@ Filename.basename file in
  Ext_string.capitalize_ascii v 

let module_name_of_file_if_any_with_upper file = 
  let v = Ext_path.chop_extension_if_any @@ Filename.basename file in
  let res = Ext_string.capitalize_ascii v in 
  res, res == v 




let good_hint_name module_name offset =
  let len = String.length module_name in 
  len > offset && 
  (function | 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) 
    (String.unsafe_get module_name offset) &&
  Ext_string.for_all_from module_name (offset + 1) 
    (function 
      | 'a' .. 'z' 
      | 'A' .. 'Z' 
      | '0' .. '9' 
      | '_' 
         -> true
      | _ -> false)

let rec collect_start buf s off len = 
  if off >= len then ()
  else 
    let next = succ off in 
    match String.unsafe_get  s off with     
    | 'a' .. 'z' as c ->
    Buffer.add_char buf (Ext_char.uppercase_ascii c)
    ;
      collect_next buf s next len
    | 'A' .. 'Z' as c -> 
      Buffer.add_char buf c ;
      collect_next buf s next len
    | _ -> collect_start buf s next len
and collect_next buf s off len = 
  if off >= len then ()  
  else 
    let next = off + 1 in 
    match String.unsafe_get s off with 
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    as c ->
      Buffer.add_char buf c ;
      collect_next buf s next len 
    | '.'
    | '-' -> 
      collect_start buf s next len      
    | _ -> 
      collect_next buf s next len 

(** This is for a js exeternal module, we can change it when printing
   for example
   {[
     var React$1 = require('react');
     React$1.render(..)
   ]}
   Given a name, if duplicated, they should  have the same id
*)
let js_id_name_of_hint_name module_name =       
  let i = Ext_string.rindex_neg module_name '/' in 
  if i >= 0 then
    let offset = succ i in 
    if good_hint_name module_name offset then 
      Ext_string.capitalize_ascii
        (Ext_string.tail_from module_name offset)
    else 
      let str_len = String.length module_name in 
      let buf = Buffer.create str_len in 
      collect_start buf module_name offset str_len ;
      let res = Buffer.contents buf in 
      if Ext_string.is_empty res then 
        Ext_string.capitalize_ascii module_name
      else res 
  else 
  if good_hint_name module_name 0 then
    Ext_string.capitalize_ascii module_name
  else 
    let str_len = (String.length module_name) in 
    let buf = Buffer.create str_len in 
    collect_start buf module_name 0 str_len ;
    let res = Buffer.contents buf in 
    if Ext_string.is_empty res then module_name
    else res   

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

val force_cmi : bool ref 
val force_cmj : bool ref
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
    (Filename.basename (Ext_string.uncapitalize_ascii !current_file))

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

let force_cmi = ref false
let force_cmj = ref false
end
module Ml_binary : sig 
#1 "ml_binary.mli"
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



type _ kind = 
  | Ml : Parsetree.structure kind 
  | Mli : Parsetree.signature kind


val read_ast : 'a kind -> in_channel -> 'a 

val write_ast :
   'a kind -> string -> 'a -> out_channel -> unit
end = struct
#1 "ml_binary.ml"
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


type _ kind = 
  | Ml : Parsetree.structure kind 
  | Mli : Parsetree.signature kind

(** [read_ast kind ic] assume [ic] channel is 
    in the right position *)
let read_ast (type t ) (kind : t  kind) ic : t  =
  let magic =
    match kind with 
    | Ml -> Config.ast_impl_magic_number
    | Mli -> Config.ast_intf_magic_number in 
  let buffer = really_input_string ic (String.length magic) in
  assert(buffer = magic); (* already checked by apply_rewriter *)
  Location.input_name := input_value ic;
  input_value ic 

let write_ast (type t) (kind : t kind) 
    (fname : string)
    (pt : t) oc = 
  let magic = 
    match kind with 
    | Ml -> Config.ast_impl_magic_number
    | Mli -> Config.ast_intf_magic_number in
  output_string oc magic ;
  output_value oc fname;
  output_value oc pt
end
module Bs_hash_stubs
= struct
#1 "bs_hash_stubs.ml"

 (* not suporting nested if here..*)
external hash_string :  string -> int = "caml_bs_hash_string" [@@noalloc];;

external hash_string_int :  string -> int  -> int = "caml_bs_hash_string_and_int" [@@noalloc];;

external hash_string_small_int :  string -> int  -> int = "caml_bs_hash_string_and_small_int" [@@noalloc];;

external hash_stamp_and_name : int -> string -> int = "caml_bs_hash_stamp_and_name" [@@noalloc];;

external hash_small_int : int -> int = "caml_bs_hash_small_int" [@@noalloc];;

external hash_int :  int  -> int = "caml_bs_hash_int" [@@noalloc];;

external string_length_based_compare : string -> string -> int  = "caml_string_length_based_compare" [@@noalloc];;


external    
    int_unsafe_blit : 
    int array -> int -> int array -> int -> int -> unit = "caml_int_array_blit" [@@noalloc];;
  
    

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

val of_list_map : 
  'a list -> 
  ('a -> 'b) -> 
  'b array 

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
  'a array ->
  'b array -> 
  ('a -> 'b -> bool) -> 
  bool

val map :   
  'a array -> 
  ('a -> 'b) -> 
  'b array

val iter :
  'a array -> 
  ('a -> unit) -> 
  unit
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


let of_list_map a f = 
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

let for_all2_no_exn xs ys p = 
  let len_xs = Array.length xs in 
  let len_ys = Array.length ys in 
  len_xs = len_ys &&    
  unsafe_loop 0 len_xs p xs ys


let map a f =
  let open Array in 
  let l = length a in
  if l = 0 then [||] else begin
    let r = make l (f(unsafe_get a 0)) in
    for i = 1 to l - 1 do
      unsafe_set r i (f(unsafe_get a i))
    done;
    r
  end

let iter a f =
  let open Array in 
  for i = 0 to length a - 1 do f(unsafe_get a i) done

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
module Hashtbl_gen
= struct
#1 "hashtbl_gen.ml"
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

(* Hash tables *)



module type S = sig 
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val reset: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val modify_or_init: 'a t -> key -> ('a -> unit) -> (unit -> 'a) -> unit 
  val remove: 'a t -> key -> unit
  val find_exn: 'a t -> key -> 'a
  val find_all: 'a t -> key -> 'a list
  val find_opt: 'a t -> key  -> 'a option
  
  (** return the key found in the hashtbl.
    Use case: when you find the key existed in hashtbl, 
    you want to use the one stored in the hashtbl. 
    (they are semantically equivlanent, but may have other information different) 
   *)
  val find_key_opt: 'a t -> key -> key option 

  val find_default: 'a t -> key -> 'a -> 'a 

  val replace: 'a t -> key -> 'a -> unit
  val mem: 'a t -> key -> bool
  val iter: 'a t -> (key -> 'a -> unit) -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
  val stats: 'a t -> Hashtbl.statistics
  val of_list2: key list -> 'a list -> 'a t
end

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
  | Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist


let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; seed = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty


let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
        insert_bucket rest; (* preserve original order of elements *)
        let nidx = indexfun h key in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done
  end



let iter h f =
  let rec do_bucket = function
    | Empty ->
      ()
    | Cons(k, d, rest) ->
      f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
      accu
    | Cons(k, d, rest) ->
      do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

let rec bucket_length accu = function
  | Empty -> accu
  | Cons(_, _, rest) -> bucket_length (accu + 1) rest

let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Ext_array.iter h.data
    (fun b ->
       let l = bucket_length 0 b in
       histo.(l) <- histo.(l) + 1)
    ;
  {Hashtbl.
    num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }



let rec small_bucket_mem eq key (lst : _ bucketlist) =
  match lst with 
  | Empty -> false 
  | Cons(k1,_,rest1) -> 
    eq  key k1 ||
    match rest1 with
    | Empty -> false 
    | Cons(k2,_,rest2) -> 
      eq key k2  || 
      match rest2 with 
      | Empty -> false 
      | Cons(k3,_,rest3) -> 
        eq key k3  ||
        small_bucket_mem eq key rest3 


let rec small_bucket_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some d1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some d2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some d3 else 
              small_bucket_opt eq key rest3 


let rec small_bucket_key_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some k1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some k2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some k3 else 
              small_bucket_key_opt eq key rest3


let rec small_bucket_default eq key default (lst : _ bucketlist) =
  match lst with 
  | Empty -> default 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then  d1 else 
      match rest1 with
      | Empty -> default 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then  d2 else 
          match rest2 with 
          | Empty -> default 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then  d3 else 
              small_bucket_default eq key default rest3 

end
module String_hashtbl : sig 
#1 "string_hashtbl.mli"
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


include Hashtbl_gen.S with type key = string




end = struct
#1 "string_hashtbl.ml"
# 9 "ext/hashtbl.cppo.ml"
type key = string
type 'a t = (key, 'a)  Hashtbl_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_string  key ) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 

# 33 "ext/hashtbl.cppo.ml"
type ('a, 'b) bucketlist = ('a,'b) Hashtbl_gen.bucketlist
let create = Hashtbl_gen.create
let clear = Hashtbl_gen.clear
let reset = Hashtbl_gen.reset
let copy = Hashtbl_gen.copy
let iter = Hashtbl_gen.iter
let fold = Hashtbl_gen.fold
let length = Hashtbl_gen.length
let stats = Hashtbl_gen.stats



let add (h : _ t) key info =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons(key, info, (Array.unsafe_get h_data i)));
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let modify_or_init (h : _ t) key modf default =
  let rec find_bucket (bucketlist : _ bucketlist)  =
    match bucketlist with
    | Cons(k,i,next) ->
      if eq_key k key then begin modf i; false end
      else find_bucket next 
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons(key,default (), Array.unsafe_get h_data i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h 
    end


let rec remove_bucket key (h : _ t) (bucketlist : _ bucketlist) : _ bucketlist = 
  match bucketlist with  
  | Empty ->
    Empty
  | Cons(k, i, next) ->
    if eq_key k key 
    then begin h.size <- h.size - 1; next end
    else Cons(k, i, remove_bucket key h next) 

let remove (h : _ t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  let old_h_szie = h.size in 
  let new_bucket = remove_bucket key h (Array.unsafe_get h_data i) in  
  if old_h_szie <> h.size then 
    Array.unsafe_set h_data i  new_bucket

let rec find_rec key (bucketlist : _ bucketlist) = match bucketlist with  
  | Empty ->
    raise Not_found
  | Cons(k, d, rest) ->
    if eq_key key k then d else find_rec key rest

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
    if eq_key key k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
        if eq_key key k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
            if eq_key key k3  then d3 else find_rec key rest3

let find_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hashtbl_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))
let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      []
    | Cons(k, d, rest) ->
      if eq_key k key 
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))

let replace h key info =
  let rec replace_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with 
    | Empty ->
      raise_notrace Not_found
    | Cons(k, i, next) ->
      if eq_key k key
      then Cons(key, info, next)
      else Cons(k, i, replace_bucket next) in
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  try
    Array.unsafe_set h_data i  (replace_bucket l)
  with Not_found ->
    begin 
      Array.unsafe_set h_data i (Cons(key, info, l));
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h;
    end 

let mem (h : _ t) key =
  let rec mem_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      false
    | Cons(k, d, rest) ->
      eq_key k key  || mem_in_bucket rest in
  mem_in_bucket (Array.unsafe_get h.data (key_index h key))


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map


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

  
let rec fill_array_aux (s : _ t) i arr : int =    
  match s with 
  | Empty -> i 
  | Node (l,k,v,r,_) -> 
    let inext = fill_array_aux l i arr in 
    Array.unsafe_set arr inext (k,v);
    fill_array_aux r (inext + 1) arr 

let to_sorted_array (s : ('key,'a) t)  : ('key * 'a ) array =    
  match s with 
  | Empty -> [||]
  | Node(l,k,v,r,_) -> 
    let len = 
      cardinal_aux (cardinal_aux 1 r) l in 
    let arr =
      Array.make len (k,v) in  
    ignore (fill_array_aux s 0 arr : int);
    arr 
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


let rec iter x f = match x with 
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter l f; f v d; iter r f

let rec map f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map f l in
    let d' = f d in
    let r' = map f r in
    Node(l', v, d', r', h)

let rec mapi x f = match x with
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi l f in
    let d' = f v d in
    let r' = mapi r f in
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
    val compare_key: key -> key -> int 
    val is_empty: 'a t -> bool
    val mem: 'a t -> key -> bool
    val to_sorted_array : 
      'a t -> (key * 'a ) array
    val add: 'a t -> key -> 'a -> 'a t
    (** [add x y m] 
        If [x] was already bound in [m], its previous binding disappears. *)
    val adjust: 'a t -> key -> ('a option->  'a) ->  'a t 
    (** [adjust acc k replace ] if not exist [add (replace None ], otherwise 
        [add k v (replace (Some old))]
    *)
    val singleton: key -> 'a -> 'a t

    val remove: 'a t -> key -> 'a t
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

    val iter: 'a t -> (key -> 'a -> unit) ->  unit
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

    val find_exn: 'a t -> key ->  'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find_opt:  'a t ->  key ->'a option
    val find_default: 'a t -> key  ->  'a  -> 'a 
    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: 'a t ->  (key -> 'a -> 'b) -> 'b t
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


  
# 10 "ext/map.cppo.ml"
  type key = string 
  let compare_key = Ext_string.compare

# 22 "ext/map.cppo.ml"
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
let to_sorted_array = Map_gen.to_sorted_array
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


let rec add (tree : _ Map_gen.t as 'a) x data  : 'a = match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add l x data ) v d r
    else
      bal l v d (add r x data )


let rec adjust (tree : _ Map_gen.t as 'a) x replace  : 'a = 
  match tree with 
  | Empty ->
    Node(Empty, x, replace None, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, replace  (Some d) , r, h)
    else if c < 0 then
      bal (adjust l x  replace ) v d r
    else
      bal l v d (adjust r x  replace )


let rec find_exn (tree : _ Map_gen.t ) x = match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find_exn (if c < 0 then l else r) x

let rec find_opt (tree : _ Map_gen.t ) x = match tree with 
  | Empty -> None 
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then Some d
    else find_opt (if c < 0 then l else r) x

let rec find_default (tree : _ Map_gen.t ) x  default     = match tree with 
  | Empty -> default  
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then  d
    else find_default (if c < 0 then l else r) x default

let rec mem (tree : _ Map_gen.t )  x= match tree with 
  | Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    c = 0 || mem (if c < 0 then l else r) x 

let rec remove (tree : _ Map_gen.t as 'a) x : 'a = match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove l x) v d r
    else
      bal l v d (remove r x )


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
  List.fold_left (fun acc (k,v) -> add acc k v ) init xs 

let of_list xs = add_list xs empty

let of_array xs = 
  Array.fold_left (fun acc (k,v) -> add acc k v ) empty xs

end
module Ast_extract : sig 
#1 "ast_extract.mli"
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









module String_set = Depend.StringSet

val read_parse_and_extract : 'a Ml_binary.kind -> 'a -> String_set.t

type ('a,'b) t 

val sort_files_by_dependencies :
  domain:String_set.t -> String_set.t String_map.t -> string Queue.t


val sort :
  ('a -> Parsetree.structure) ->
  ('b -> Parsetree.signature) ->
  ('a, 'b) t String_map.t -> string Queue.t  



(**
   [build fmt files parse_implementation parse_interface]
   Given a list of files return an ast table 
*)
val collect_ast_map :
  Format.formatter ->
  string list ->
  (Format.formatter -> string -> 'a) ->
  (Format.formatter -> string -> 'b) ->
  ('a, 'b) t String_map.t

type dir_spec = 
  { dir : string ;
    mutable  excludes : string list 
  }

(** If the genereated queue is empty, it means 
    1. The main module  does not exist (does not exist due to typo)
    2. It does exist but not in search path
    The order matters from head to tail 
*)
val collect_from_main :
  ?extra_dirs:dir_spec list -> 
  ?excludes : string list -> 
  ?alias_map: string String_hashtbl.t ->
  Format.formatter ->
  (Format.formatter -> string -> 'a) ->
  (Format.formatter -> string -> 'b) ->
  ('a -> Parsetree.structure) ->
  ('b -> Parsetree.signature) ->
  string -> ('a, 'b) t String_map.t * string Queue.t

val build_queue :
  Format.formatter ->
  string Queue.t ->
  ('b, 'c) t String_map.t ->
  (Format.formatter -> string -> string -> 'b -> unit) ->
  (Format.formatter -> string -> string -> 'c -> unit) -> unit
  
val handle_queue :
  Format.formatter ->
  string Queue.t ->
  ('a, 'b) t String_map.t ->
  (string -> string -> 'a -> unit) ->
  (string -> string -> 'b  -> unit) ->
  (string -> string -> string -> 'b -> 'a -> unit) -> unit


val build_lazy_queue :
  Format.formatter ->
  string Queue.t ->
  (Parsetree.structure lazy_t, Parsetree.signature lazy_t) t String_map.t ->
  (Format.formatter -> string -> string -> Parsetree.structure -> unit) ->
  (Format.formatter -> string -> string -> Parsetree.signature -> unit) -> unit  



end = struct
#1 "ast_extract.ml"
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

type module_name = private string

module String_set = Depend.StringSet

(* FIXME: [Clflags.open_modules] seems not to be properly used *)
 
module SMap = Depend.StringMap
let bound_vars = SMap.empty 


type 'a kind = 'a Ml_binary.kind 


let read_parse_and_extract (type t) (k : t kind) (ast : t) : String_set.t =
  Depend.free_structure_names := String_set.empty;
  List.iter
    (fun modname  ->

       ignore @@ 
       
       Depend.open_module bound_vars (Longident.Lident modname))
    (!Clflags.open_modules);
  (match k with
   | Ml_binary.Ml  -> Depend.add_implementation bound_vars ast
   | Ml_binary.Mli  -> Depend.add_signature bound_vars ast  ); 
  !Depend.free_structure_names


type ('a,'b) ast_info =
  | Ml of
      string * (* sourcefile *)
      'a *
      string (* opref *)      
  | Mli of string * (* sourcefile *)
           'b *
           string (* opref *)
  | Ml_mli of
      string * (* sourcefile *)
      'a *
      string  * (* opref1 *)
      string * (* sourcefile *)      
      'b *
      string (* opref2*)

type ('a,'b) t =
  { module_name : string ; ast_info : ('a,'b) ast_info }


(* only visit nodes that are currently in the domain *)
(* https://en.wikipedia.org/wiki/Topological_sorting *)
(* dfs   *)
let sort_files_by_dependencies ~(domain : String_set.t) (dependency_graph : String_set.t String_map.t) : 
  string Queue.t =
  let next current =
    String_map.find_exn  dependency_graph current in    
  let worklist = ref domain in
  let result = Queue.create () in
  let rec visit (visiting : String_set.t) path (current : string) =
    let next_path = current :: path in 
    if String_set.mem current visiting then
      Bs_exception.error (Bs_cyclic_depends next_path)
    else if String_set.mem current !worklist then
      begin
        let next_set = String_set.add current visiting in         
        next current |>        
        String_set.iter
          (fun node ->
             if  String_map.mem dependency_graph node then
               visit next_set next_path node)
        ;
        worklist := String_set.remove  current !worklist;
        Queue.push current result ;
      end in        
  while not (String_set.is_empty !worklist) do 
    visit String_set.empty []  (String_set.choose !worklist)
  done;
  if Js_config.get_diagnose () then
    Format.fprintf Format.err_formatter
      "Order: @[%a@]@."    
      (Ext_format.pp_print_queue
         ~pp_sep:Format.pp_print_space
         Format.pp_print_string)
      result ;       
  result
;;



let sort  project_ml project_mli (ast_table : _ t String_map.t) = 
  let domain =
    String_map.fold
      (fun k _ acc -> String_set.add k acc)
      ast_table String_set.empty in
  let h =
    String_map.map
      (fun
        ({ast_info})
        ->
          match ast_info with
          | Ml (_, ast,  _)
            ->
            read_parse_and_extract Ml (project_ml ast)            
          | Mli (_, ast, _)
            ->
            read_parse_and_extract Mli (project_mli ast)
          | Ml_mli (_, impl, _, _, intf, _)
            ->
            String_set.union
              (read_parse_and_extract Ml (project_ml impl))
              (read_parse_and_extract Mli (project_mli intf))              
      ) ast_table in    
  sort_files_by_dependencies  ~domain h

(** same as {!Ocaml_parse.check_suffix} but does not care with [-c -o] option*)
let check_suffix  name  = 
  if Ext_path.check_suffix_case name ".ml"
  || Ext_path.check_suffix_case name ".mlt" then 
    `Ml,
    Ext_path.chop_extension_if_any  name 
  else if Ext_path.check_suffix_case name !Config.interface_suffix then 
    `Mli,   Ext_path.chop_extension_if_any  name 
  else 
    raise(Arg.Bad("don't know what to do with " ^ name))


let collect_ast_map ppf files parse_implementation parse_interface  =
  List.fold_left
    (fun (acc : _ t String_map.t)
      source_file ->
      match check_suffix source_file with
      | `Ml, opref ->
        let module_name = Ext_modulename.module_name_of_file source_file in
        begin match String_map.find_exn acc module_name with
          | exception Not_found ->
            String_map.add acc module_name
              {ast_info =
                 (Ml (source_file, parse_implementation
                        ppf source_file, opref));
               module_name ;
              } 
          | {ast_info = (Ml (source_file2, _, _)
                        | Ml_mli(source_file2, _, _,_,_,_))} ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info =  Mli (source_file2, intf, opref2)}
            ->
            String_map.add acc module_name
              {ast_info =
                 Ml_mli (source_file,
                         parse_implementation ppf source_file,
                         opref,
                         source_file2,
                         intf,
                         opref2
                        );
               module_name} 
        end
      | `Mli, opref ->
        let module_name = Ext_modulename.module_name_of_file source_file in
        begin match String_map.find_exn acc module_name with
          | exception Not_found ->
            String_map.add acc module_name
              {ast_info = (Mli (source_file, parse_interface
                                  ppf source_file, opref));
               module_name } 
          | {ast_info =
               (Mli (source_file2, _, _) |
                Ml_mli(_,_,_,source_file2,_,_)) } ->
            Bs_exception.error
              (Bs_duplicated_module (source_file, source_file2))
          | {ast_info = Ml (source_file2, impl, opref2)}
            ->
            String_map.add acc module_name
              {ast_info =
                 Ml_mli
                   (source_file2,
                    impl,
                    opref2,
                    source_file,
                    parse_interface ppf source_file,
                    opref
                   );
               module_name} 
        end
    ) String_map.empty files
;;
type dir_spec = 
  { dir : string ;
    mutable  excludes : string list 
  }

let collect_from_main 
    ?(extra_dirs=[])
    ?(excludes=[])
    ?alias_map
    (ppf : Format.formatter)
    parse_implementation
    parse_interface
    project_impl 
    project_intf 
    main_module =
  let files = 
    List.fold_left (fun acc dir_spec -> 
        let  dirname, excludes = 
          match dir_spec with 
          | { dir =  dirname; excludes = dir_excludes} ->
            (*   dirname, excludes *)
            (* | `Dir_with_excludes (dirname, dir_excludes) -> *)
            dirname,
             (Ext_list.flat_map_append 
              dir_excludes  excludes
              (fun x -> [x ^ ".ml" ; x ^ ".mli" ])
              ) 
        in 
        Array.fold_left (fun acc source_file -> 
            if (Ext_string.ends_with source_file ".ml" ||
                Ext_string.ends_with source_file ".mli" )
            && (* not_excluded source_file *) (not (List.mem source_file excludes))
            then 
              (Filename.concat dirname source_file) :: acc else acc
          ) acc (Sys.readdir dirname))
      [] extra_dirs in
  let ast_table = collect_ast_map ppf files parse_implementation parse_interface in 
  let visited = String_hashtbl.create 31 in
  let result = Queue.create () in  
  let next module_name : String_set.t =
    let module_set = 
      match String_map.find_exn ast_table module_name with
      | exception _ -> String_set.empty
      | {ast_info = Ml (_,  impl, _)} ->
        read_parse_and_extract Ml (project_impl impl)
      | {ast_info = Mli (_,  intf,_)} ->
        read_parse_and_extract Mli (project_intf intf)
      | {ast_info = Ml_mli(_, impl, _, _,  intf, _)}
        -> 
        String_set.union
          (read_parse_and_extract Ml (project_impl impl))
          (read_parse_and_extract Mli (project_intf intf))
    in 
    match alias_map with 
    | None -> module_set 
    | Some map -> 
      String_set.fold (fun x acc -> String_set.add (String_hashtbl.find_default map x x) acc  ) module_set String_set.empty
  in
  let rec visit visiting path current =
    if String_set.mem current visiting  then
      Bs_exception.error (Bs_cyclic_depends (current::path))
    else
    if not (String_hashtbl.mem visited current)
    && String_map.mem ast_table current then
      begin
        String_set.iter
          (visit
             (String_set.add current visiting)
             (current::path))
          (next current) ;
        Queue.push current result;
        String_hashtbl.add visited current ();
      end in
  visit (String_set.empty) [] main_module ;
  ast_table, result   


let build_queue ppf queue
    (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue
  |> Queue.iter
    (fun modname -> 
       match String_map.find_exn ast_table modname  with
       | {ast_info = Ml(source_file,ast, opref)}
         -> 
         after_parsing_impl ppf source_file 
           opref ast 
       | {ast_info = Mli (source_file,ast,opref) ; }  
         ->
         after_parsing_sig ppf source_file 
           opref ast 
       | {ast_info = Ml_mli(source_file1,impl,opref1,source_file2,intf,opref2)}
         -> 
         after_parsing_sig ppf source_file1 opref1 intf ;
         after_parsing_impl ppf source_file2 opref2 impl
       | exception Not_found -> assert false 
    )


let handle_queue 
  ppf 
  queue ast_table 
  decorate_module_only 
  decorate_interface_only 
  decorate_module = 
  queue 
  |> Queue.iter
    (fun base ->
       match (String_map.find_exn ast_table base ).ast_info with
       | exception Not_found -> assert false
       | Ml (ml_name,  ml_content, _)
         ->
         decorate_module_only  base ml_name ml_content
       | Mli (mli_name , mli_content, _) ->
         decorate_interface_only base  mli_name mli_content
       | Ml_mli (ml_name, ml_content, _, mli_name,   mli_content, _)
         ->
         decorate_module  base mli_name ml_name mli_content ml_content

    )



let build_lazy_queue ppf queue (ast_table : _ t String_map.t)
    after_parsing_impl
    after_parsing_sig    
  =
  queue |> Queue.iter (fun modname -> 
      match String_map.find_exn ast_table modname  with
      | {ast_info = Ml(source_file,lazy ast, opref)}
        -> 
        after_parsing_impl ppf source_file opref ast 
      | {ast_info = Mli (source_file,lazy ast,opref) ; }  
        ->
        after_parsing_sig ppf source_file opref ast 
      | {ast_info = Ml_mli(source_file1,lazy impl,opref1,source_file2,lazy intf,opref2)}
        -> 
        after_parsing_sig ppf source_file1 opref1 intf ;
        after_parsing_impl ppf source_file2 opref2 impl
      | exception Not_found -> assert false 
    )


end
module Ext_io : sig 
#1 "ext_io.mli"
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

val load_file : string -> string

val rev_lines_of_file : string -> string list

val write_file : string -> string -> unit

end = struct
#1 "ext_io.ml"
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


(** on 32 bit , there are 16M limitation *)
let load_file f =
  Ext_pervasives.finally (open_in_bin f) close_in begin fun ic ->   
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    Bytes.unsafe_to_string s
  end


let rev_lines_of_file file = 
  Ext_pervasives.finally (open_in_bin file) close_in begin fun chan -> 
    let rec loop acc = 
      match input_line chan with
      | line -> loop (line :: acc)
      | exception End_of_file -> close_in chan ; acc in
    loop []
  end

let write_file f content = 
  Ext_pervasives.finally (open_out_bin f) close_out begin fun oc ->   
    output_string oc content
  end

end
(** Interface as module  *)
module Lexer
= struct
#1 "lexer.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexical analyzer *)

val init : unit -> unit
val token: Lexing.lexbuf -> Parser.token
val skip_hash_bang: Lexing.lexbuf -> unit

type directive_type 

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else 
  | Unexpected_token_in_conditional 
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive
  | Conditional_expr_expected_type of directive_type * directive_type                           
;;

exception Error of error * Location.t

open Format

val report_error: formatter -> error -> unit
 (* Deprecated.  Use Location.{error_of_exn, report_error}. *)

val in_comment : unit -> bool;;
val in_string : unit -> bool;;


val print_warnings : bool ref
val handle_docstrings: bool ref
val comments : unit -> (string * Location.t) list
val token_with_comments : Lexing.lexbuf -> Parser.token

(*
  [set_preprocessor init preprocessor] registers [init] as the function
to call to initialize the preprocessor when the lexer is initialized,
and [preprocessor] a function that is called when a new token is needed
by the parser, as [preprocessor lexer lexbuf] where [lexer] is the
lexing function.

When a preprocessor is configured by calling [set_preprocessor], the lexer
changes its behavior to accept backslash-newline as a token-separating blank.
*)

val set_preprocessor :
  (unit -> unit) ->
  ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token) ->
  unit

(** semantic version predicate *)
val semver : Location.t ->   string -> string -> bool

val filter_directive_from_lexbuf : Lexing.lexbuf -> (int * int) list

val replace_directive_int : string -> int -> unit
val replace_directive_string : string -> string -> unit
val replace_directive_bool : string -> bool -> unit 
val remove_directive_built_in_value : string -> unit

(** @return false means failed to define *)
val define_key_value : string -> string -> bool
val list_variables : Format.formatter -> unit

end
module Docstrings : sig 
#1 "docstrings.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Documentation comments *)

(** (Re)Initialise all docstring state *)
val init : unit -> unit

(** Emit warnings for unattached and ambiguous docstrings *)
val warn_bad_docstrings : unit -> unit

(** {2 Docstrings} *)

(** Documentation comments *)
type docstring

(** Create a docstring *)
val docstring : string -> Location.t -> docstring

(** Register a docstring *)
val register : docstring -> unit

(** Get the text of a docstring *)
val docstring_body : docstring -> string

(** Get the location of a docstring *)
val docstring_loc : docstring -> Location.t

(** {2 Set functions}

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

(** {2 Items}

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

(** {2 Fields and constructors}

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

(** {2 Unattached comments}

    The {!text} type represents documentation which is not attached to
    anything. *)

type text = docstring list

val empty_text : text
val empty_text_lazy : text Lazy.t

val text_attr : docstring -> Parsetree.attribute

(** Convert text to attributes and add them to an attribute list *)
val add_text_attrs : text -> Parsetree.attributes -> Parsetree.attributes

(** Fetch the text preceding the current symbol. *)
val symbol_text : unit -> text
val symbol_text_lazy : unit -> text Lazy.t

(** Fetch the text preceding the symbol at the given position. *)
val rhs_text : int -> text
val rhs_text_lazy : int -> text Lazy.t

(** {2 Extra text}

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
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                               Leo White                                *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

(* Docstring constructors and destructors *)

let docstring body loc =
  let ds =
    { ds_body = body;
      ds_loc = loc;
      ds_attached = Unattached;
      ds_associated = Zero; }
  in
  ds

let register ds =
  docstrings := ds :: !docstrings

let docstring_body ds = ds.ds_body

let docstring_loc ds = ds.ds_loc

(* Docstrings attached to items *)

type docs =
  { docs_pre: docstring option;
    docs_post: docstring option; }

let empty_docs = { docs_pre = None; docs_post = None }

let doc_loc = {txt = "ocaml.doc"; loc = Location.none}

let docs_attr ds =
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
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
    | None | Some { ds_body=""; _ } -> attrs
    | Some ds -> docs_attr ds :: attrs
  in
  let attrs =
    match docs.docs_post with
    | None | Some { ds_body=""; _ } -> attrs
    | Some ds -> attrs @ [docs_attr ds]
  in
  attrs

(* Docstrings attached to constructors or fields *)

type info = docstring option

let empty_info = None

let info_attr = docs_attr

let add_info_attrs info attrs =
  match info with
  | None | Some {ds_body=""; _} -> attrs
  | Some ds -> attrs @ [info_attr ds]

(* Docstrings not attached to a specific item *)

type text = docstring list

let empty_text = []
let empty_text_lazy = lazy []

let text_loc = {txt = "ocaml.text"; loc = Location.none}

let text_attr ds =
  let open Parsetree in
  let exp =
    { pexp_desc = Pexp_constant (Pconst_string(ds.ds_body, None));
      pexp_loc = ds.ds_loc;
      pexp_attributes = []; }
  in
  let item =
    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }
  in
    (text_loc, PStr [item])

let add_text_attrs dsl attrs =
  let fdsl = List.filter (function {ds_body=""} -> false| _ ->true) dsl in
  (List.map text_attr fdsl) @ attrs

(* Find the first non-info docstring in a list, attach it and return it *)
let get_docstring ~info dsl =
  let rec loop = function
    | [] -> None
    | {ds_attached = Info; _} :: rest -> loop rest
    | ds :: _ ->
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
module Syntaxerr : sig 
#1 "syntaxerr.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary type for reporting syntax errors *)

open Format

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * string

exception Error of error
exception Escape_error

val report_error: formatter -> error -> unit
 (** @deprecated Use {!Location.error_of_exn}, {!Location.report_error}. *)

val location_of_error: error -> Location.t
val ill_formed_ast: Location.t -> string -> 'a

end = struct
#1 "syntaxerr.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliary type for reporting syntax errors *)

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * string

exception Error of error
exception Escape_error

let prepare_error = function
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      Location.errorf ~loc:closing_loc
        ~sub:[
          Location.errorf ~loc:opening_loc
            "This '%s' might be unmatched" opening
        ]
        ~if_highlight:
          (Printf.sprintf "Syntax error: '%s' expected, \
                           the highlighted '%s' might be unmatched"
             closing opening)
        "Syntax error: '%s' expected" closing

  | Expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s expected." nonterm
  | Not_expecting (loc, nonterm) ->
      Location.errorf ~loc "Syntax error: %s not expected." nonterm
  | Applicative_path loc ->
      Location.errorf ~loc
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      Location.errorf ~loc
        "In this scoped type, variable '%s \
         is reserved for the local type %s."
         var var
  | Other loc ->
      Location.errorf ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      Location.errorf ~loc "broken invariant in parsetree: %s" s
  | Invalid_package_type (loc, s) ->
      Location.errorf ~loc "invalid package type: %s" s

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )


let report_error ppf err =
  Location.report_error ppf (prepare_error err)

let location_of_error = function
  | Unclosed(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Not_expecting (l, _)
  | Ill_formed_ast (l, _)
  | Invalid_package_type (l, _)
  | Expecting (l, _) -> l


let ill_formed_ast loc s =
  raise (Error (Ill_formed_ast (loc, s)))

end
module Parse : sig 
#1 "parse.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Entry points in the parser *)

val implementation : Lexing.lexbuf -> Parsetree.structure
val interface : Lexing.lexbuf -> Parsetree.signature
val toplevel_phrase : Lexing.lexbuf -> Parsetree.toplevel_phrase
val use_file : Lexing.lexbuf -> Parsetree.toplevel_phrase list
val core_type : Lexing.lexbuf -> Parsetree.core_type
val expression : Lexing.lexbuf -> Parsetree.expression
val pattern : Lexing.lexbuf -> Parsetree.pattern

end = struct
#1 "parse.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Entry points in the parser *)

(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment _, _)
    | Lexer.Error (Lexer.Unterminated_string, _)
    | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
    | Lexer.Error (Lexer.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    Docstrings.init ();
    Lexer.init ();
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    Docstrings.warn_bad_docstrings ();
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
and core_type = wrap Parser.parse_core_type
and expression = wrap Parser.parse_expression
and pattern = wrap Parser.parse_pattern

end
module Bspack_main : sig 
#1 "bspack_main.mli"
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


val read_lines : string -> string  -> string list 
(* example 
   {[ 
     Line_process.read_lines "." "./tools/tools.mllib" 
   ]} 

   FIXME: we can only concat (dir/file) not (dir/dir)
   {[
     Filename.concat "/bb/x/" "/bb/x/";;
   ]}
*)
  

end = struct
#1 "bspack_main.ml"
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


module L_string_set = Set.Make(String)
(* lexical order *)

let (@>) (b, v) acc =
  if b then
    v :: acc
  else
    acc

let preprocess_to_buffer fn (str : string) (oc : Buffer.t) : unit =     
  let lexbuf = Lexing.from_string  str in
  Lexer.init () ;
  Location.init lexbuf fn;
  let segments =    
    Lexer.filter_directive_from_lexbuf  lexbuf in
  Ext_list.iter segments
    (fun (start, pos) ->
       Buffer.add_substring  oc str start (pos - start)
    )

let verify_valid_ml (str : string) = 
  try 
    ignore @@ Parse.implementation (Lexing.from_string str);
    true
  with _ -> false

(* same as {!preprocess_to_buffer} except writing to channel directly *)
let preprocess_string fn (str : string) oc =
  let lexbuf = Lexing.from_string  str in
  Lexer.init () ;
  Location.init lexbuf fn;
  let segments =    
    Lexer.filter_directive_from_lexbuf  lexbuf in
  Ext_list.iter segments
    (fun (start, pos) ->
       output_substring  oc str start (pos - start)
    )

let (//) = Filename.concat

let rec process_line cwd filedir  line =
  let line = Ext_string.trim line in
  let len = String.length line in
  if len = 0 then []
  else
    match line.[0] with
    | '#' -> []
    | _ ->
      let segments =
        Ext_string.split_by ~keep_empty:false (fun x -> x = ' ' || x = '\t' ) line
      in
      begin
        match segments with
        | ["include" ;  path ]
          ->
          (* prerr_endline path;  *)
          read_lines cwd  (filedir// path)
        | [ x ]  ->
          let ml = filedir // x ^ ".ml" in
          let mli = filedir // x ^ ".mli" in
          let ml_exists, mli_exists = Sys.file_exists ml , Sys.file_exists mli in
          if not ml_exists && not mli_exists then
            begin
              prerr_endline (filedir //x ^ " not exists");
              []
            end
          else
            (ml_exists, ml) @> (mli_exists , mli) @> []
        | _
          ->  Ext_pervasives.failwithf ~loc:__LOC__ "invalid line %s" line
      end
and read_lines (cwd : string) (file : string) : string list =
  file
  |> Ext_io.rev_lines_of_file
  |> List.fold_left (fun acc f ->
      let filedir  =   Filename.dirname file in
      let extras = process_line  cwd filedir f in
      Ext_list.append extras   acc
    ) []

let implementation sourcefile =
  let content = Ext_io.load_file sourcefile in
  let ast =
    let oldname = !Location.input_name in
    Location.input_name := sourcefile ;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf sourcefile ;
    match Parse.implementation  lexbuf
    with
    | exception e ->
      Location.input_name := oldname;
      raise e
    | ast ->
      Location.input_name := oldname ;
      ast
  in
  ast, content

let interface sourcefile =
  let content = Ext_io.load_file sourcefile in
  let ast =
    let oldname = !Location.input_name in
    Location.input_name := sourcefile ;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf sourcefile;
    match Parse.interface  lexbuf with
    | exception e ->
      Location.input_name := oldname ;
      raise e
    | ast ->
      Location.input_name := oldname ;
      ast in
  ast, content


let emit out_chan name =
  output_string out_chan "#1 \"";
  (*Note here we do this is mostly to avoid leaking user's
    information, like private path, in the future, we can have
    a flag
  *)
  output_string out_chan (Filename.basename name) ;
  output_string out_chan "\"\n"

let decorate_module 
    ?(module_bound=true)
    out_chan base mli_name ml_name mli_content ml_content =
  if module_bound then begin 
    let base = Ext_string.capitalize_ascii base in
    output_string out_chan "module ";
    output_string out_chan base ;
    output_string out_chan " : sig \n";
    emit out_chan mli_name ;
    preprocess_string mli_name mli_content out_chan;
    output_string out_chan "\nend = struct\n";
    emit out_chan  ml_name ;
    preprocess_string ml_name ml_content out_chan;
    output_string out_chan "\nend\n"
  end
  else
    begin 
      output_string out_chan "include (struct\n";
      emit out_chan  ml_name ;
      preprocess_string ml_name ml_content out_chan;
      output_string out_chan "\nend : sig \n";
      emit out_chan mli_name ;
      preprocess_string mli_name mli_content out_chan;
      output_string out_chan "\nend)";
    end

let decorate_module_only 
    ?(check : unit option)
    ?(module_bound=true) 
    out_chan base ml_name ml_content =
  if module_bound then begin 
    let base = Ext_string.capitalize_ascii base in
    output_string out_chan "module ";
    output_string out_chan base ;
    output_string out_chan "\n= struct\n"
  end;
  emit out_chan  ml_name;
  if check <> None then 
    let buf = Buffer.create 2000 in 
    preprocess_to_buffer ml_name ml_content buf; 
    let str = Buffer.contents buf in 
    if not @@ verify_valid_ml str then 
      failwith (ml_name ^ " can not be a valid ml module")
    else 
      output_string out_chan str 
  else 
    preprocess_string ml_name ml_content out_chan ; 
  if module_bound then 
    output_string out_chan "\nend\n"

(** recursive module is not good for performance, here module type only 
    has to be pure types otherwise it would not compile any way
*)
let decorate_interface_only out_chan  base  mli_name mli_content =
  output_string out_chan "(** Interface as module  *)\n";
  decorate_module_only out_chan base mli_name mli_content ~check:()

(** set mllib *)
let mllib = ref None
let set_string s = mllib := Some s

let batch_files = ref []
let collect_file name =
  batch_files := name :: !batch_files

let output_file = ref None
let set_output file = output_file := Some file
let header_option = ref false

type main_module = { modulename : string ; export : bool }

(** set bs-main*)
let main_module : main_module option ref = ref None

let set_main_module modulename = 
  main_module := Some {modulename; export = false }

let set_main_export modulename = 
  main_module := Some {modulename; export = true }

let set_mllib_file = ref false     

let prelude = ref None 
let set_prelude f = 
  if Sys.file_exists f then 
    prelude := Some f 
  else raise (Arg.Bad ("file " ^ f ^ " don't exist "))
let prelude_str = ref None
let set_prelude_str f = prelude_str := Some f 

(**
   {[
     # process_include "ghsogh?a,b,c";;
     - : [> `Dir of string | `Dir_with_excludes of string * string list ] =
     `Dir_with_excludes ("ghsogh", ["a"; "b"; "c"])
                        # process_include "ghsogh?a";;
     - : [> `Dir of string | `Dir_with_excludes of string * string list ] =
     `Dir_with_excludes ("ghsogh", ["a"])
   ]}
*)
(* type dir_spec =  *)
(*   [ `Dir of string | `Dir_with_excludes of string * string list ] *)

let cwd = Sys.getcwd ()

let normalize s = 
  Ext_path.normalize_absolute_path (Ext_path.combine cwd s )

let process_include s : Ast_extract.dir_spec = 
  let i = Ext_string.rindex_neg s '?'   in 
  if i < 0 then   
    { dir = normalize s; excludes = []}
  else 
    let dir = String.sub s 0 i in
    { dir = normalize dir;
      excludes = Ext_string.split 
          (String.sub s (i + 1) (String.length s - i - 1)    )
          ','}

let deduplicate_dirs (xs : Ast_extract.dir_spec list) =
  let set :  Ast_extract.dir_spec String_hashtbl.t = String_hashtbl.create 64 in 
  List.filter (fun ({Ast_extract.dir ; excludes = new_excludes } as y) -> 
      match String_hashtbl.find_opt set dir with
      | None ->  
        String_hashtbl.add set dir y;
        true 
      | Some x ->  x.excludes <- new_excludes @ x.excludes ; false
    ) xs 

let includes :  _ list ref = ref []

let add_include dir = 
  includes := process_include    dir :: !includes

let exclude_modules = ref []                                     
let add_exclude module_ = 
  exclude_modules := module_ :: !exclude_modules
let no_implicit_include = ref false 

let alias_map = String_hashtbl.create 0
let alias_map_rev = String_hashtbl.create 0

(**
   {[
     A -> B 
       A1 -> B
   ]}
   print 
   {[

     module A = B
     module A1 = B  
   ]}   
   We don't suppport 
   {[
     A -> B 
       A -> C
   ]}
*)    
let alias_module s = 
  match Ext_string.split s '=' with 
  | [a;b] -> 
    (* Error checking later*)
    if String_hashtbl.mem alias_map a then 
      raise (Arg.Bad ("duplicated module alias " ^ a))
    else
      begin 
        String_hashtbl.add alias_map_rev b a;
        String_hashtbl.add alias_map a b 
      end
  | _ -> raise (Arg.Bad "invalid module alias format like A=B")

let undefine_symbol (s : string) = 
  Lexer.remove_directive_built_in_value s 
let define_symbol (s : string) = 
  match Ext_string.split ~keep_empty:true s '=' with
  | [key; v] -> 
    if not @@ Lexer.define_key_value key v  then 
      raise (Arg.Bad ("illegal definition: " ^ s))
  | _ -> raise (Arg.Bad ("illegal definition: " ^ s))

let specs : (string * Arg.spec * string) list =
  [ 
    "-bs-no-implicit-include", (Arg.Set no_implicit_include),
    " Not including cwd as search path";
    "-prelude-str", (Arg.String set_prelude_str),
    " Set a prelude string, (before -prelude) option" ;
    "-module-alias", (Arg.String alias_module ),
    " -module-alis A=B, whenever need A,replace it with B" ;
    "-prelude", (Arg.String set_prelude),
    " Set a prelude file, literally copy into the beginning";    
    "-bs-mllib", (Arg.String set_string),
    " Files collected from mllib";
    "-bs-MD", (Arg.Set set_mllib_file),
    " Log files into mllib(only effective under -bs-main mode)";
    "-o", (Arg.String set_output),
    " Set output file (default to stdout)" ;
    "-with-header", (Arg.Set header_option),
    " with header of time stamp" ; 
    "-bs-exclude-I", (Arg.String add_exclude),
    " don't read and pack such modules from -I (in the future, we should detect conflicts in mllib or commandline) "
    ;
    "-bs-main", (Arg.String set_main_module),
    " set the main entry module";
    "-main-export", (Arg.String set_main_export),
    " Set the main module and respect its exports";
    "-I",  (Arg.String add_include),
    " add dir to search path";
    "-U", Arg.String undefine_symbol,
    " Undefine a symbol when bspacking";
    "-D", Arg.String define_symbol, 
    " Define a symbol when bspacking"
  ]


let anonymous filename =
  collect_file filename

let usage = "Usage: bspack <options> <files>\nOptions are:"
let () =
  try
    (Arg.parse specs anonymous usage;
     let command_files =  !batch_files in
     let mllib = !mllib in 
     (* emit code now *)
     let out_chan =
       lazy (match !output_file with
           | None -> stdout
           | Some file -> open_out_bin file)  in
     let emit_header out_chan = 
       let local_time = Unix.(localtime (gettimeofday ())) in
       (if  !header_option 
        then
          output_string out_chan
            (Printf.sprintf "(** Generated by bspack %02d/%02d-%02d:%02d *)\n"
               (local_time.tm_mon + 1) local_time.tm_mday
               local_time.tm_hour local_time.tm_min));
       (match !prelude_str with 
        | None -> ()
        | Some s -> output_string out_chan s; output_string out_chan "\n" );
       match !prelude with
       | None -> ()
       | Some f -> 
         begin 
           output_string out_chan (Ext_io.load_file f);
           output_string out_chan "\n"         
         end
     in
     let close_out_chan out_chan = 
       (if  out_chan != stdout then close_out out_chan) in
     let files =
       Ext_list.append (match mllib with
        | Some s
          -> read_lines (Sys.getcwd ()) s
        | None -> []) command_files in

     match !main_module, files with
     | Some _ , _ :: _
       -> 
       Ext_pervasives.failwithf ~loc:__LOC__ 
         "-bs-main conflicts with other flags [ %s ]"
         (String.concat ", " files)
     | Some {modulename  = main_module ; export },  []
       ->
       let excludes =
         match !exclude_modules with
         | [] -> []
         | xs -> 
           Ext_list.flat_map xs (fun x -> [x ^ ".ml" ; x ^ ".mli"] ) in 
       let extra_dirs = 
         deduplicate_dirs @@
         if not !no_implicit_include then {Ast_extract.dir =  cwd; excludes = []} :: !includes 
         else !includes 
       in  
       let ast_table, tasks =
         Ast_extract.collect_from_main ~excludes ~extra_dirs ~alias_map
           Format.err_formatter
           (fun _ppf sourcefile -> lazy (implementation sourcefile))
           (fun _ppf sourcefile -> lazy (interface sourcefile))
           (fun (lazy (stru, _)) -> stru)
           (fun (lazy (sigi, _)) -> sigi)
           main_module
       in 
       if Queue.is_empty tasks then 
         raise (Arg.Bad (main_module ^ " does not pull in any libs, maybe wrong input"))
       ;
       let out_chan = Lazy.force out_chan in
       let collect_module_by_filenames  = !set_mllib_file in 
       let collection_modules = Queue.create () in
       let count = ref 0 in 
       let task_length = Queue.length tasks in 
       emit_header out_chan ;
       begin 
         Ast_extract.handle_queue Format.err_formatter tasks ast_table
           (fun base ml_name (lazy(_, ml_content)) -> 
              incr count ;  
              if collect_module_by_filenames then 
                Queue.add ml_name collection_modules; 
              let module_bound = not  export || task_length > !count  in 
              decorate_module_only ~module_bound out_chan base ml_name ml_content;
              let aliased = Ext_string.capitalize_ascii base in 
              String_hashtbl.find_all alias_map_rev aliased
              |> List.iter 
                (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
           (fun base mli_name (lazy (_, mli_content))  -> 
              incr count ;                  
              if collect_module_by_filenames then 
                Queue.add mli_name collection_modules;                 

              decorate_interface_only out_chan base mli_name mli_content;
              let aliased = Ext_string.capitalize_ascii base in 
              String_hashtbl.find_all alias_map_rev aliased
              |> List.iter 
                (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
           (fun base mli_name ml_name (lazy (_, mli_content)) (lazy (_, ml_content))
             -> 
               incr count;  
               (*TODO: assume mli_name, ml_name are in the same dir,
                 Needs to be addressed 
               *)
               if collect_module_by_filenames then 
                 begin 
                   Queue.add ml_name collection_modules;
                   Queue.add mli_name collection_modules
                 end; 
               (** if export 
                   print it as 
                   {[inclue (struct end : sig end)]}
               *)   
               let module_bound = not export || task_length > !count in 
               decorate_module ~module_bound out_chan base mli_name ml_name mli_content ml_content;
               let aliased = (Ext_string.capitalize_ascii base) in 
               String_hashtbl.find_all alias_map_rev aliased
               |> List.iter 
                 (fun s -> output_string out_chan (Printf.sprintf "module %s = %s \n"  s aliased))

           )
       end;
       close_out_chan out_chan;
       begin 
         if !set_mllib_file then
           match !output_file with
           | None -> ()
           | Some file ->
             let output = file ^ ".d" in
             let sorted_queue = 
               Queue.fold 
               (fun acc collection_module -> 
                  L_string_set.add 
                  (
                        (*FIXME: now we normalized path,
                        we need a beautiful output too for relative path
                        The relative path should be also be normalized..
                      *)
                      Filename.concat 
                        (Ext_path.rel_normalized_absolute_path
                           ~from:cwd 
                           (Filename.dirname collection_module)
                        ) (Filename.basename collection_module)

                  )
                  (* collection_module  *)
                  acc
                ) L_string_set.empty  collection_modules in 
             Ext_io.write_file 
               output
               (                 
                 L_string_set.fold
                   (fun collection_module acc  -> 
                      acc ^ 
                      collection_module
                      ^ "\n"
                      (* ^ a ^ " : ; touch " ^ output ^ "\n" *)
                   ) sorted_queue
                   (file ^ ":\n" )
                   (* collection_modules *)
               )
       end
     | None, _ -> 
       let ast_table =
         Ast_extract.collect_ast_map
           Format.err_formatter files
           (fun _ppf sourcefile -> implementation sourcefile
           )
           (fun _ppf sourcefile -> interface sourcefile) in
       let tasks = Ast_extract.sort fst  fst ast_table in
       let out_chan = (Lazy.force out_chan) in
       emit_header out_chan ;
       Ast_extract.handle_queue Format.err_formatter tasks ast_table 
         (fun base ml_name (_, ml_content) -> decorate_module_only  out_chan base ml_name ml_content)
         (fun base mli_name (_, mli_content)  -> decorate_interface_only out_chan base mli_name mli_content )
         (fun base mli_name ml_name (_, mli_content) (_, ml_content)
           -> decorate_module out_chan base mli_name ml_name mli_content ml_content);
       close_out_chan out_chan
    )
  with x ->
    begin
      Location.report_exception Format.err_formatter x ;
      exit 2
    end

end
