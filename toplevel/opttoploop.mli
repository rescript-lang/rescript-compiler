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

open Format

(* Set the load paths, before running anything *)

val set_paths : unit -> unit

(* The interactive toplevel loop *)

val loop : formatter -> unit

(* Read and execute a script from the given file *)

val run_script : formatter -> string -> string array -> bool
        (* true if successful, false if error *)

(* Interface with toplevel directives *)

type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

val directive_table : (string, directive_fun) Hashtbl.t
        (* Table of known directives, with their execution function *)
val toplevel_env : Env.t ref
        (* Typing environment for the toplevel *)
val initialize_toplevel_env : unit -> unit
        (* Initialize the typing environment for the toplevel *)
val print_exception_outcome : formatter -> exn -> unit
        (* Print an exception resulting from the evaluation of user code. *)
val execute_phrase : bool -> formatter -> Parsetree.toplevel_phrase -> bool
        (* Execute the given toplevel phrase. Return [true] if the
           phrase executed with no errors and [false] otherwise.
           First bool says whether the values and types of the results
           should be printed. Uncaught exceptions are always printed. *)
val use_file : formatter -> string -> bool
val use_silently : formatter -> string -> bool
        (* Read and execute commands from a file.
           [use_file] prints the types and values of the results.
           [use_silently] does not print them. *)
val eval_path: Path.t -> Obj.t
        (* Return the toplevel object referred to by the given path *)

(* Printing of values *)

val print_value: Env.t -> Obj.t -> formatter -> Types.type_expr -> unit
val print_untyped_exception: formatter -> Obj.t -> unit

val install_printer :
  Path.t -> Types.type_expr -> (formatter -> Obj.t -> unit) -> unit
val remove_printer : Path.t -> unit

val max_printer_depth: int ref
val max_printer_steps: int ref

(* Hooks for external parsers and printers *)

val parse_toplevel_phrase : (Lexing.lexbuf -> Parsetree.toplevel_phrase) ref
val parse_use_file : (Lexing.lexbuf -> Parsetree.toplevel_phrase list) ref
val print_location : formatter -> Location.t -> unit
val print_error : formatter -> Location.t -> unit
val print_warning : Location.t -> formatter -> Warnings.t -> unit
val input_name : string ref

val print_out_value :
  (formatter -> Outcometree.out_value -> unit) ref
val print_out_type :
  (formatter -> Outcometree.out_type -> unit) ref
val print_out_class_type :
  (formatter -> Outcometree.out_class_type -> unit) ref
val print_out_module_type :
  (formatter -> Outcometree.out_module_type -> unit) ref
val print_out_type_extension :
  (formatter -> Outcometree.out_type_extension -> unit) ref
val print_out_sig_item :
  (formatter -> Outcometree.out_sig_item -> unit) ref
val print_out_signature :
  (formatter -> Outcometree.out_sig_item list -> unit) ref
val print_out_phrase :
  (formatter -> Outcometree.out_phrase -> unit) ref

(* Hooks for external line editor *)

val read_interactive_input : (string -> string -> int -> int * bool) ref

(* Hooks for initialization *)

val toplevel_startup_hook : (unit -> unit) ref
