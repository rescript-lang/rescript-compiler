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
[@@@ocaml.deprecated "This module is deprecated"]
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
