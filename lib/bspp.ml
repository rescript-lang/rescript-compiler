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
module Parser
= struct
#1 "parser.ml"
type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int)
  | INT32 of (int32)
  | INT64 of (int64)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LPAREN
  | LBRACKETAT
  | LBRACKETATAT
  | LBRACKETATATAT
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint)
  | NEW
  | NONREC
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of (string)
  | OR
  | PERCENT
  | PLUS
  | PLUSDOT
  | PLUSEQ
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SHARPOP of (string)
  | SIG
  | STAR
  | STRING of (string * string option)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
  | COMMENT of (string * Location.t)

  | EOL


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
module Bspp_main : sig 
#1 "bspp_main.mli"
(** *)

end = struct
#1 "bspp_main.ml"


(*let buffer_intervals (intervals : (int * int) list) buf ic oc =
  intervals
  |> List.iter
    (fun (start, stop) -> 
       let len = stop - start in 
       if len <> 0 then 
         begin
           seek_in ic start ; 
           Buffer.add_channel buf ic len ; 
           Buffer.output_buffer oc buf ; 
           Buffer.clear buf;
         end
    )*)
  

let preprocess fn oc = 
  let ic = open_in_bin fn in 
  let lexbuf = Lexing.from_channel ic in 
  let buf = Buffer.create 4096 in 
  Location.init lexbuf fn;
  Lexer.init ();
  lexbuf
  |> Lexer.filter_directive_from_lexbuf  
  (* Get a list of segments
    TODO: output line directive
   *)
  |> List.iter
    (fun (start, stop) ->       
       let len = stop - start in 
       if len <> 0 then 
         begin
           seek_in ic start ; 
           Buffer.add_channel buf ic len ; 
           Buffer.output_buffer oc buf ; 
           Buffer.clear buf;
         end
    );
  close_in ic 


let () = 
  preprocess Sys.argv.(1) stdout

end
