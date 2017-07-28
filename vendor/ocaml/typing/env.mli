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

(* Environment handling *)

open Types

type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_extension of summary * Ident.t * extension_constructor
  | Env_module of summary * Ident.t * module_declaration
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
  | Env_functor_arg of summary * Ident.t

type t

val empty: t
val initial_safe_string: t
val initial_unsafe_string: t
val diff: t -> t -> Ident.t list

type type_descriptions =
    constructor_description list * label_description list

(* For short-paths *)
type iter_cont
val iter_types:
    (Path.t -> Path.t * (type_declaration * type_descriptions) -> unit) ->
    t -> iter_cont
val run_iter_cont: iter_cont list -> (Path.t * iter_cont) list
val same_types: t -> t -> bool
val used_persistent: unit -> Concr.t
val find_shadowed_types: Path.t -> t -> Path.t list

(* Lookup by paths *)

val find_value: Path.t -> t -> value_description
val find_type: Path.t -> t -> type_declaration
val find_type_descrs: Path.t -> t -> type_descriptions
val find_module: Path.t -> t -> module_declaration
val find_modtype: Path.t -> t -> modtype_declaration
val find_class: Path.t -> t -> class_declaration
val find_cltype: Path.t -> t -> class_type_declaration

val find_type_expansion:
    Path.t -> t -> type_expr list * type_expr * int option
val find_type_expansion_opt:
    Path.t -> t -> type_expr list * type_expr * int option
(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion: Path.t -> t -> module_type
val is_functor_arg: Path.t -> t -> bool
val normalize_path: Location.t option -> t -> Path.t -> Path.t
(* Normalize the path to a concrete value or module.
   If the option is None, allow returning dangling paths.
   Otherwise raise a Missing_module error, and may add forgotten
   head as required global. *)
val reset_required_globals: unit -> unit
val get_required_globals: unit -> Ident.t list
val add_required_global: Ident.t -> unit

val has_local_constraints: t -> bool
val add_gadt_instance_level: int -> t -> t
val gadt_instance_level: t -> type_expr -> int option
val add_gadt_instances: t -> int -> type_expr list -> unit
val add_gadt_instance_chain: t -> int -> type_expr -> unit

(* Lookup by long identifiers *)

val lookup_value: Longident.t -> t -> Path.t * value_description
val lookup_constructor: Longident.t -> t -> constructor_description
val lookup_all_constructors:
  Longident.t -> t -> (constructor_description * (unit -> unit)) list
val lookup_label: Longident.t -> t -> label_description
val lookup_all_labels:
  Longident.t -> t -> (label_description * (unit -> unit)) list
val lookup_type: Longident.t -> t -> Path.t * type_declaration
val lookup_module: load:bool -> Longident.t -> t -> Path.t
val lookup_modtype: Longident.t -> t -> Path.t * modtype_declaration
val lookup_class: Longident.t -> t -> Path.t * class_declaration
val lookup_cltype: Longident.t -> t -> Path.t * class_type_declaration

exception Recmodule
  (* Raise by lookup_module when the identifier refers
     to one of the modules of a recursive definition
     during the computation of its approximation (see #5965). *)

(* Insertion by identifier *)

val add_value:
    ?check:(string -> Warnings.t) -> Ident.t -> value_description -> t -> t
val add_type: check:bool -> Ident.t -> type_declaration -> t -> t
val add_extension: check:bool -> Ident.t -> extension_constructor -> t -> t
val add_module: ?arg:bool -> Ident.t -> module_type -> t -> t
val add_module_declaration: ?arg:bool -> Ident.t -> module_declaration -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_class: Ident.t -> class_declaration -> t -> t
val add_cltype: Ident.t -> class_type_declaration -> t -> t
val add_local_constraint: Ident.t -> type_declaration -> int -> t -> t

(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
val add_signature: signature -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. *)

val open_signature:
    ?loc:Location.t -> ?toplevel:bool -> Asttypes.override_flag -> Path.t ->
      signature -> t -> t
val open_pers_signature: string -> t -> t

(* Insertion by name *)

val enter_value:
    ?check:(string -> Warnings.t) ->
    string -> value_description -> t -> Ident.t * t
val enter_type: string -> type_declaration -> t -> Ident.t * t
val enter_extension: string -> extension_constructor -> t -> Ident.t * t
val enter_module: ?arg:bool -> string -> module_type -> t -> Ident.t * t
val enter_module_declaration:
    ?arg:bool -> string -> module_declaration -> t -> Ident.t * t
val enter_modtype: string -> modtype_declaration -> t -> Ident.t * t
val enter_class: string -> class_declaration -> t -> Ident.t * t
val enter_cltype: string -> class_type_declaration -> t -> Ident.t * t

(* Initialize the cache of in-core module interfaces. *)
val reset_cache: unit -> unit

(* To be called before each toplevel phrase. *)
val reset_cache_toplevel: unit -> unit

(* Remember the name of the current compilation unit. *)
val set_unit_name: string -> unit
val get_unit_name: unit -> string

(* Read, save a signature to/from a file *)

val read_signature: string -> string -> signature
        (* Arguments: module name, file name. Results: signature. *)
val save_signature: signature -> string -> string -> signature
        (* Arguments: signature, module name, file name. *)
val save_signature_with_imports:
    signature -> string -> string -> (string * Digest.t option) list -> signature
        (* Arguments: signature, module name, file name,
           imported units with their CRCs. *)

(* Return the CRC of the interface of the given compilation unit *)

val crc_of_unit: string -> Digest.t

(* Return the set of compilation units imported, with their CRC *)

val imports: unit -> (string * Digest.t option) list

(* Direct access to the table of imported compilation units with their CRC *)

val crc_units: Consistbl.t
val add_import: string -> unit

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)

val summary: t -> summary

(* Return an equivalent environment where all fields have been reset,
   except the summary. The initial environment can be rebuilt from the
   summary, using Envaux.env_of_only_summary. *)

val keep_only_summary : t -> t
val env_of_only_summary : (summary -> Subst.t -> t) -> t -> t

(* Error report *)

type error =
  | Illegal_renaming of string * string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string
  | Missing_module of Location.t * Path.t * Path.t
  | Illegal_value_name of Location.t * string

exception Error of error

open Format

val report_error: formatter -> error -> unit


val mark_value_used: t -> string -> value_description -> unit
val mark_type_used: t -> string -> type_declaration -> unit

type constructor_usage = Positive | Pattern | Privatize
val mark_constructor_used:
    constructor_usage -> t -> string -> type_declaration -> string -> unit
val mark_constructor:
    constructor_usage -> t -> string -> constructor_description -> unit
val mark_extension_used:
    constructor_usage -> t -> extension_constructor -> string -> unit

val in_signature: t -> t
val implicit_coercion: t -> t

val set_value_used_callback:
    string -> value_description -> (unit -> unit) -> unit
val set_type_used_callback:
    string -> type_declaration -> ((unit -> unit) -> unit) -> unit

(* Forward declaration to break mutual recursion with Includemod. *)
val check_modtype_inclusion:
      (t -> module_type -> Path.t -> module_type -> unit) ref
(* Forward declaration to break mutual recursion with Typecore. *)
val add_delayed_check_forward: ((unit -> unit) -> unit) ref
(* Forward declaration to break mutual recursion with Mtype. *)
val strengthen: (t -> module_type -> Path.t -> module_type) ref

(** Folding over all identifiers (for analysis purpose) *)

val fold_values:
  (string -> Path.t -> value_description -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_types:
  (string -> Path.t -> type_declaration * type_descriptions -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_constructors:
  (constructor_description -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_labels:
  (label_description -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a

(** Persistent structures are only traversed if they are already loaded. *)
val fold_modules:
  (string -> Path.t -> module_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a

val fold_modtypes:
  (string -> Path.t -> modtype_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_classs:
  (string -> Path.t -> class_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a
val fold_cltypes:
  (string -> Path.t -> class_type_declaration -> 'a -> 'a) ->
  Longident.t option -> t -> 'a -> 'a

(** Utilities *)
val scrape_alias: t -> module_type -> module_type
val check_value_name: string -> Location.t -> unit
