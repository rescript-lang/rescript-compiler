open Format
open Outcometree

val out_value : (formatter -> out_value -> unit) ref [@@live]
val out_type : (formatter -> out_type -> unit) ref [@@live]
val out_class_type : (formatter -> out_class_type -> unit) ref [@@live]
val out_module_type : (formatter -> out_module_type -> unit) ref [@@live]
val out_sig_item : (formatter -> out_sig_item -> unit) ref [@@live]
val out_signature : (formatter -> out_sig_item list -> unit) ref [@@live]
val out_type_extension : (formatter -> out_type_extension -> unit) ref [@@live]
val out_phrase : (formatter -> out_phrase -> unit) ref [@@live]

val parenthesized_ident : string -> bool [@@live]