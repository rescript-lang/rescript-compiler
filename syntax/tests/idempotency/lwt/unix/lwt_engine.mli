(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Lwt unix main loop engine *)

(** {2 Events} *)

type event
  (** Type of events. An event represent a callback registered to be
      called when some event occurs. *)

val stop_event : event -> unit
  (** [stop_event event] stops the given event. *)

val fake_event : event
  (** Event which does nothing when stopped. *)

(** {2 Event loop functions} *)

val iter : bool -> unit
  (** [iter block] performs one iteration of the main loop. If [block]
      is [true] the function must block until one event becomes
      available, otherwise it should just check for available events
      and return immediately. *)

val on_readable : Unix.file_descr -> (event -> unit) -> event
  (** [on_readable fd f] calls [f] each time [fd] becomes readable. *)

val on_writable : Unix.file_descr -> (event -> unit) -> event
  (** [on_readable fd f] calls [f] each time [fd] becomes writable. *)

val on_timer : float -> bool -> (event -> unit) -> event
  (** [on_timer delay repeat f] calls [f] one time after [delay]
      seconds. If [repeat] is [true] then [f] is called each [delay]
      seconds, otherwise it is called only one time. *)

val readable_count : unit -> int
  (** Returns the number of events waiting for a file descriptor to
      become readable. *)

val writable_count : unit -> int
  (** Returns the number of events waiting for a file descriptor to
      become writable. *)

val timer_count : unit -> int
  (** Returns the number of registered timers. *)

val fake_io : Unix.file_descr -> unit
  (** Simulates activity on the given file descriptor. *)

(** {2 Engines} *)

(** An engine represents a set of functions used to register different
    kinds of callbacks for different kinds of events. *)

(** Abstract class for engines. *)

type ev_loop

module Ev_backend :
sig
  type t
  val default : t
  val select : t
  val poll : t
  val epoll : t
  val kqueue : t
  val devpoll : t
  val port : t

  val pp : Format.formatter -> t -> unit
end

  (** Type of libev loops. *)

(** {2 The current engine} *)

val get : unit -> t
  (** [get ()] returns the engine currently in use. *)

val set : ?transfer : bool -> ?destroy : bool -> t -> unit
  (** [set ?transfer ?destroy engine] replaces the current engine by
      the given one.

      If [transfer] is [true] (the default) all events from the
      current engine are transferred to the new one.

      If [destroy] is [true] (the default) then the current engine is
      destroyed before being replaced. *)
