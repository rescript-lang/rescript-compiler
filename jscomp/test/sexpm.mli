(* This file is free software, part of containers. See file "license" for more
   details. *)

(** {1 Simple and efficient S-expression parsing/printing}
    @since 0.7 *)

type 'a or_error = [`Ok of 'a | `Error of string]
type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

(** {2 Basics} *)

type t = [`Atom of string | `List of t list]
type sexp = t

(** {2 Serialization (encoding)} *)

val to_buf : Buffer.t -> t -> unit
val to_string : t -> string
val to_file : string -> t -> unit

val to_file_seq : string -> t sequence -> unit
(** Print the given sequence of expressions to a file *)

val to_chan : out_channel -> t -> unit

val print : Format.formatter -> t -> unit
(** Pretty-printer nice on human eyes (including indentation) *)

val print_noindent : Format.formatter -> t -> unit
(** Raw, direct printing as compact as possible *)

(** {2 Deserialization (decoding)} *)

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

type 'a parse_result = ['a or_error | `End]
(** A parser of ['a] can return [`Ok x] when it parsed a value, or [`Error e]
    when a parse error was encountered, or [`End] if the input was empty *)

module MakeDecode (M : MONAD) : sig
  type t
  (** Decoder *)

  val make : ?bufsize:int -> (Bytes.t -> int -> int -> int M.t) -> t
  (** Make a decoder with the given function used to refill an internal buffer.
      The function might return [0] if the input is exhausted.
      @param bufsize size of internal buffer *)

  val next : t -> sexp parse_result M.t
  (** Parse the next S-expression or return an error if the input isn't long
      enough or isn't a proper S-expression *)
end

module ID_MONAD : MONAD with type 'a t = 'a
(** The monad that just uses blocking calls as bind @since 0.14 ['a t = 'a]
    contraint is @since 0.16 *)

module D : module type of MakeDecode (ID_MONAD)
(** Decoder that just blocks when input is not available
    @since 0.14 *)

val parse_string : string -> t or_error
(** Parse a string *)

val parse_chan : ?bufsize:int -> in_channel -> t or_error
(** Parse a S-expression from the given channel. Can read more data than
    necessary, so don't use this if you need finer-grained control (e.g. to
    read something else {b after} the S-exp) *)

val parse_chan_gen : ?bufsize:int -> in_channel -> t or_error gen
(** Parse a channel into a generator of S-expressions *)

val parse_chan_list : ?bufsize:int -> in_channel -> t list or_error

val parse_file : string -> t or_error
(** Open the file and read a S-exp from it *)

val parse_file_list : string -> t list or_error
(** Open the file and read a S-exp from it *)
