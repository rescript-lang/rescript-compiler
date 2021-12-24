(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Format API for Lwt-powered IOs

    @since 4.1.0 *)

(** This module bridges the gap between
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html} [Format]}
    and {!Lwt}. Although it is not required, it is recommended to use this
    module with the {{:http://erratique.ch/software/fmt} [Fmt]} library.

    Compared to regular formatting function, the main difference is that
    printing statements will now return promises instead of blocking.
*)

val printf : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
(** Returns a promise that prints on the standard output.
    Similar to
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#VALprintf}
    [Format.printf]}. *)

val eprintf : ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a
(** Returns a promise that prints on the standard error.
    Similar to
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#VALeprintf}
    [Format.eprintf]}. *)

(** {1 Formatters} *)

type formatter
(** Lwt enabled formatters *)

type order =
  | String of string * int * int (** [String (s, off, len)] indicate the output of [s] at offset [off] and length [len]. *)
  | Flush (** Flush operation *)

val make_stream : unit -> order Lwt_stream.t * formatter
(** [make_stream ()] returns a formatter and a stream of all the writing
    order given on that stream.
*)


val of_channel : Lwt_io.output_channel -> formatter
(** [of_channel oc] creates a formatter that writes to the channel [oc]. *)

val stdout : formatter
(** Formatter printing on {!Lwt_io.stdout}. *)

val stderr : formatter
(** Formatter printing on {!Lwt_io.stdout}. *)

val make_formatter :
  commit:(unit -> unit Lwt.t) -> fmt:Format.formatter -> unit -> formatter
(** [make_formatter ~commit ~fmt] creates a new lwt formatter based on the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#TYPEformatter}
    [Format.formatter]} [fmt]. The [commit] function will be called by the
    printing functions to update the underlying channel.
*)

val get_formatter : formatter -> Format.formatter
(** [get_formatter fmt] returns the underlying
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#TYPEformatter}
    [Format.formatter]}. To access the underlying formatter during printing, it
    isvrecommended to use [%t] and [%a].
*)

(** {2 Printing} *)

val fprintf : formatter -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val kfprintf :
  (formatter -> unit Lwt.t -> 'a) ->
  formatter -> ('b, Format.formatter, unit, 'a) format4 -> 'b

val ifprintf : formatter -> ('a, Format.formatter, unit, unit Lwt.t) format4 -> 'a

val ikfprintf :
  (formatter -> unit Lwt.t -> 'a) ->
  formatter -> ('b, Format.formatter, unit, 'a) format4 -> 'b

val flush : formatter -> unit Lwt.t
(** [flush fmt] flushes the formatter (as with
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html#VALpp_print_flush}
    [Format.pp_print_flush]}) and
    executes all the printing action on the underlying channel.
*)


(** Low level functions *)

val write_order : Lwt_io.output_channel -> order -> unit Lwt.t
(** [write_order oc o] applies the order [o] on the channel [oc]. *)

val write_pending : formatter -> unit Lwt.t
(** Write all the pending orders of a formatter.
    Warning: This function flush neither the internal format queues
    nor the underlying channel and is intended for low level use only.
    You should probably use {!flush} instead.
*)
