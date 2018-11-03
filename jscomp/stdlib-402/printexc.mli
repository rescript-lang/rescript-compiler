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

(** Facilities for printing exceptions and inspecting current call stack. *)

val to_string: exn -> string
(** [Printexc.to_string e] returns a string representation of
   the exception [e]. *)

val print: ('a -> 'b) -> 'a -> 'b
(** [Printexc.print fn x] applies [fn] to [x] and returns the result.
   If the evaluation of [fn x] raises any exception, the
   name of the exception is printed on standard error output,
   and the exception is raised again.
   The typical use is to catch and report exceptions that
   escape a function application. *)

val catch: ('a -> 'b) -> 'a -> 'b
(** [Printexc.catch fn x] is similar to {!Printexc.print}, but
   aborts the program with exit code 2 after printing the
   uncaught exception.  This function is deprecated: the runtime
   system is now able to print uncaught exceptions as precisely
   as [Printexc.catch] does.  Moreover, calling [Printexc.catch]
   makes it harder to track the location of the exception
   using the debugger or the stack backtrace facility.
   So, do not use [Printexc.catch] in new code.  *)

val print_backtrace: out_channel -> unit
(** [Printexc.print_backtrace oc] prints an exception backtrace
    on the output channel [oc].  The backtrace lists the program
    locations where the most-recently raised exception was raised
    and where it was propagated through function calls.
    @since 3.11.0
*)

val get_backtrace: unit -> string
(** [Printexc.get_backtrace ()] returns a string containing the
    same exception backtrace that [Printexc.print_backtrace] would
    print.
    @since 3.11.0
*)

val record_backtrace: bool -> unit
(** [Printexc.record_backtrace b] turns recording of exception backtraces
    on (if [b = true]) or off (if [b = false]).  Initially, backtraces
    are not recorded, unless the [b] flag is given to the program
    through the [OCAMLRUNPARAM] variable.
    @since 3.11.0
*)

val backtrace_status: unit -> bool
(** [Printexc.backtrace_status()] returns [true] if exception
    backtraces are currently recorded, [false] if not.
    @since 3.11.0
*)

val register_printer: (exn -> string option) -> unit
(** [Printexc.register_printer fn] registers [fn] as an exception
    printer.  The printer should return [None] or raise an exception
    if it does not know how to convert the passed exception, and [Some
    s] with [s] the resulting string if it can convert the passed
    exception. Exceptions raised by the printer are ignored.

    When converting an exception into a string, the printers will be invoked
    in the reverse order of their registrations, until a printer returns
    a [Some s] value (if no such printer exists, the runtime will use a
    generic printer).

    When using this mechanism, one should be aware that an exception backtrace
    is attached to the thread that saw it raised, rather than to the exception
    itself. Practically, it means that the code related to [fn] should not use
    the backtrace if it has itself raised an exception before.
    @since 3.11.2
*)

(** {6 Raw backtraces} *)

type raw_backtrace
(** The abstract type [raw_backtrace] stores a backtrace in
    a low-level format, instead of directly exposing them as string as
    the [get_backtrace()] function does.

    This allows delaying the formatting of backtraces to when they are
    actually printed, which may be useful if you record more
    backtraces than you print.

    Raw backtraces cannot be marshalled. If you need marshalling, you
    should use the array returned by the [backtrace_slots] function of
    the next section.

    @since 4.01.0
*)

val get_raw_backtrace: unit -> raw_backtrace
(** [Printexc.get_raw_backtrace ()] returns the same exception
    backtrace that [Printexc.print_backtrace] would print, but in
    a raw format.

    @since 4.01.0
*)

val print_raw_backtrace: out_channel -> raw_backtrace -> unit
(** Print a raw backtrace in the same format
    [Printexc.print_backtrace] uses.

    @since 4.01.0
*)

val raw_backtrace_to_string: raw_backtrace -> string
(** Return a string from a raw backtrace, in the same format
    [Printexc.get_backtrace] uses.

    @since 4.01.0
*)

(** {6 Current call stack} *)

val get_callstack: int -> raw_backtrace
(** [Printexc.get_callstack n] returns a description of the top of the
    call stack on the current program point (for the current thread),
    with at most [n] entries.  (Note: this function is not related to
    exceptions at all, despite being part of the [Printexc] module.)

    @since 4.01.0
*)

(** {6 Uncaught exceptions} *)

val set_uncaught_exception_handler: (exn -> raw_backtrace -> unit) -> unit
(** [Printexc.set_uncaught_exception_handler fn] registers [fn] as the handler
    for uncaught exceptions. The default handler prints the exception and
    backtrace on standard error output.

    Note that when [fn] is called all the functions registered with
    {!Pervasives.at_exit} have already been called. Because of this you must
    make sure any output channel [fn] writes on is flushed.

    Also note that exceptions raised by user code in the interactive toplevel
    are not passed to this function as they are caught by the toplevel itself.

    If [fn] raises an exception, both the exceptions passed to [fn] and raised
    by [fn] will be printed with their respective backtrace.

    @since 4.02.0
*)


(** {6 Manipulation of backtrace information}

    Those function allow to traverse the slots of a raw backtrace,
    extract information from them in a programmer-friendly format.
*)

type backtrace_slot
(** The abstract type [backtrace_slot] represents a single slot of
    a backtrace.

    @since 4.02
*)

val backtrace_slots : raw_backtrace -> backtrace_slot array option
(** Returns the slots of a raw backtrace, or [None] if none of them
    contain useful information.

    In the return array, the slot at index [0] corresponds to the most
    recent function call, raise, or primitive [get_backtrace] call in
    the trace.

    Some possible reasons for returning [None] are as follow:
    - none of the slots in the trace come from modules compiled with
    debug information ([-g])
    - the program is a bytecode program that has not been linked with
    debug information enabled ([ocamlc -g])

    @since 4.02.0
*)

type location = {
  filename : string;
  line_number : int;
  start_char : int;
  end_char : int;
}
(** The type of location information found in backtraces. [start_char]
    and [end_char] are positions relative to the beginning of the
    line.

    @since 4.02
*)

module Slot : sig
  type t = backtrace_slot

  val is_raise : t -> bool
  (** [is_raise slot] is [true] when [slot] refers to a raising
      point in the code, and [false] when it comes from a simple
      function call.

      @since 4.02
  *)

  val location : t -> location option
  (** [location slot] returns the location information of the slot,
      if available, and [None] otherwise.

      Some possible reasons for failing to return a location are as follow:
      - the slot corresponds to a compiler-inserted raise
      - the slot corresponds to a part of the program that has not been
      compiled with debug information ([-g])

      @since 4.02
  *)

  val format : int -> t -> string option
  (** [format pos slot] returns the string representation of [slot] as
      [raw_backtrace_to_string] would format it, assuming it is the
      [pos]-th element of the backtrace: the [0]-th element is
      pretty-printed differently than the others.

      Whole-backtrace printing functions also skip some uninformative
      slots; in that case, [format pos slot] returns [None].

      @since 4.02
  *)
end


(** {6 Raw backtrace slots} *)

type raw_backtrace_slot
(** This type allows direct access to raw backtrace slots, without any
    conversion in an OCaml-usable data-structure. Being
    process-specific, they must absolutely not be marshalled, and are
    unsafe to use for this reason (marshalling them may not fail, but
    un-marshalling and using the result will result in
    undefined behavior).

    Elements of this type can still be compared and hashed: when two
    elements are equal, then they represent the same source location
    (the converse is not necessarily true in presence of inlining,
    for example).

    @since 4.02.0
*)

val raw_backtrace_length : raw_backtrace -> int
(** [raw_backtrace_length bckt] returns the number of slots in the
    backtrace [bckt].

    @since 4.02
*)

val get_raw_backtrace_slot : raw_backtrace -> int -> raw_backtrace_slot
(** [get_slot bckt pos] returns the slot in position [pos] in the
    backtrace [bckt].

    @since 4.02
*)

val convert_raw_backtrace_slot : raw_backtrace_slot -> backtrace_slot
(** Extracts the user-friendly [backtrace_slot] from a low-level
    [raw_backtrace_slot].

    @since 4.02
*)


(** {6 Exception slots} *)

val exn_slot_id: exn -> int
(** [Printexc.exn_slot_id] returns an integer which uniquely identifies
    the constructor used to create the exception value [exn]
    (in the current runtime).

    @since 4.02.0
*)

val exn_slot_name: exn -> string
(** [Printexc.exn_slot_id exn] returns the internal name of the constructor
    used to create the exception value [exn].

    @since 4.02.0
*)
