(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [Lwt_main.run p] calls the Lwt scheduler, performing I/O until [p]
      resolves. [Lwt_main.run p] returns the value in [p] if [p] is fulfilled.
      If [p] is rejected with an exception instead, [Lwt_main.run p] raises that
      exception.

      Every native and bytecode program that uses Lwt should call this function
      at its top level. It implements the Lwt main loop.

      Example:
      {[
let main () = Lwt_io.write_line Lwt_io.stdout "hello world"

let () = Lwt_main.run (main ())
      ]}

      [Lwt_main.run] is not available when targeting JavaScript, because the
      environment (such as Node.js or the browser's script engine) implements
      the I/O loop.

      On Unix, calling [Lwt_main.run] installs a [SIGCHLD] handler, which is
      needed for the implementations of {!Lwt_unix.waitpid} and
      {!Lwt_unix.wait4}. As a result, programs that call [Lwt_main.run] and also
      use non-Lwt system calls need to handle those system calls failing with
      [EINTR].

      Nested calls to [Lwt_main.run] are not allowed. That is, do not call
      [Lwt_main.run] in a callback triggered by a promise that is resolved by
      an outer invocation of [Lwt_main.run]. If your program makes such a call,
      [Lwt_main.run] will raise [Failure]. This should be considered a logic
      error (i.e., code making such a call is inherently broken).

      It is not safe to call [Lwt_main.run] in a function registered with
      [Pervasives.at_exit], use {!Lwt_main.at_exit} instead. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)



(** Hook sequences. Each module of this type is a set of hooks, to be run by Lwt
    at certain points during execution. See modules {!Enter_iter_hooks},
    {!Leave_iter_hooks}, and {!Exit_hooks}. *)
module type Hooks =
sig
  type 'return_value kind
  (** Hooks are functions of either type [unit -> unit] or [unit -> unit Lwt.t];
      this type constructor is used only to express both possibilities in one
      signature. *)

  type hook
  (** Values of type [hook] represent hooks that have been added, so that they
      can be removed later (if needed). *)

  val add_first : (unit -> unit kind) -> hook
  (** Adds a hook to the hook sequence underlying this module, to be run
      {e first}, before any other hooks already added. *)

  val add_last : (unit -> unit kind) -> hook
  (** Adds a hook to the hook sequence underlying this module, to be run
      {e last}, after any other hooks already added. *)

  val remove : hook -> unit
  (** Removes a hook added by {!add_first} or {!add_last}. *)

  val remove_all : unit -> unit
  (** Removes all hooks from the hook sequence underlying this module. *)
end

(** Hooks, of type [unit -> unit], that are called before each iteration of the
    Lwt main loop.

    @since 4.2.0 *)
module Enter_iter_hooks :
  Hooks with type 'return_value kind = 'return_value

(** Hooks, of type [unit -> unit], that are called after each iteration of the
    Lwt main loop.

    @since 4.2.0 *)
module Leave_iter_hooks :
  Hooks with type 'return_value kind = 'return_value

(** Promise-returning hooks, of type [unit -> unit Lwt.t], that are called at
    process exit. Exceptions raised by these hooks are ignored.

    @since 4.2.0 *)
module Exit_hooks :
  Hooks with type 'return_value kind = 'return_value Lwt.t



[@@@ocaml.warning "-3"]

val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  [@@ocaml.deprecated
    " Use module Lwt_main.Enter_iter_hooks."]
(** @deprecated Use module {!Enter_iter_hooks}. *)

val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  [@@ocaml.deprecated
    " Use module Lwt_main.Leave_iter_hooks."]
(** @deprecated Use module {!Leave_iter_hooks}. *)

val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  [@@ocaml.deprecated
    " Use module Lwt_main.Exit_hooks."]
(** @deprecated Use module {!Exit_hooks}. *)

[@@@ocaml.warning "+3"]



val at_exit : (unit -> unit Lwt.t) -> unit
(** [Lwt_main.at_exit hook] is the same as
    [ignore (Lwt_main.Exit_hooks.add_first hook)]. *)
