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

