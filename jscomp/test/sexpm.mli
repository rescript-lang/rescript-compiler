
(* This file is free software, part of containers. See file "license" for more details. *)

(** {1 Simple and efficient S-expression parsing/printing}

    @since 0.7 *)

type 'a or_error = [ `Ok of 'a | `Error of string ]
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(** {2 Basics} *)

type t = [
  | `Atom of string
  | `List of t list
]
type sexp = t

(** {2 Serialization (encoding)} *)

val to_buf : Buffer.t -> t -> unit

val to_string : t -> string



val parse_string : string -> t or_error
(** Parse a string *)

