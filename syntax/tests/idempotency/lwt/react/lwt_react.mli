(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** React utilities *)

(** This module is an overlay for the [React] module. You can open it
    instead of the [React] module in order to get all of [React]'s functions
    plus Lwt ones.

    This module is provided by OPAM package [lwt_react]. Link with ocamlfind
    package [lwt_react]. *)

type 'a event = 'a React.event
    (** Type of events. *)

type 'a signal = 'a React.signal
    (** Type of signals. *)

module E : sig
  include module type of React.E

  (** {2 Lwt-specific utilities} *)

  val with_finaliser : (unit -> unit) -> 'a event -> 'a event
    (** [with_finaliser f e] returns an event [e'] which behave as
        [e], except that [f] is called when [e'] is garbage
        collected. *)

  val next : 'a event -> 'a Lwt.t
  (** [next e] returns the next occurrence of [e].

      Avoid trying to create an "asynchronous loop" by calling [next e] again in
      a callback attached to the promise returned by [next e]:

      - The callback is called within the React update step, so calling [next e]
        within it will return a promise that is fulfilled with the same value as
        the current occurrence.
      - If you instead arrange for the React update step to end (for example, by
        calling [Lwt.pause ()] within the callback), multiple React update steps
        may occur before the callback calls [next e] again, so some occurrences
        can be effectively "lost."

      To robustly asynchronously process occurrences of [e] in a loop, use
      [to_stream e], and repeatedly call {!Lwt_stream.next} on the resulting
      stream. *)

  val limit : (unit -> unit Lwt.t) -> 'a event -> 'a event
    (** [limit f e] limits the rate of [e] with [f].

        For example, to limit the rate of an event to 1 per second you
        can use: [limit (fun () -> Lwt_unix.sleep 1.0) event]. *)

  val from : (unit -> 'a Lwt.t) -> 'a event
    (** [from f] creates an event which occurs each time [f ()]
        returns a value. If [f] raises an exception, the event is just
        stopped. *)

  val to_stream : 'a event -> 'a Lwt_stream.t
    (** Creates a stream holding all values occurring on the given
        event *)

  val of_stream : 'a Lwt_stream.t -> 'a event
    (** [of_stream stream] creates an event which occurs each time a
        value is available on the stream. *)

  val delay : 'a event Lwt.t -> 'a event
    (** [delay promise] is an event which does not occur until
        [promise] resolves. Then it behaves as the event returned by
        [promise]. *)

  val keep : 'a event -> unit
    (** [keep e] keeps a reference to [e] so it will never be garbage
        collected. *)

  (** {2 Threaded versions of React transformation functions} *)

  (** The following functions behave as their [React] counterpart,
      except that they take functions that may yield.

      As usual the [_s] suffix is used when calls are serialized, and
      the [_p] suffix is used when they are not.

      Note that [*_p] functions may not preserve event order. *)

  val app_s : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
  val app_p : ('a -> 'b Lwt.t) event -> 'a event -> 'b event

  val map_s : ('a -> 'b Lwt.t) -> 'a event -> 'b event
  val map_p: ('a -> 'b Lwt.t) -> 'a event -> 'b event

  val filter_s : ('a -> bool Lwt.t) -> 'a event -> 'a event
  val filter_p : ('a -> bool Lwt.t) -> 'a event -> 'a event

  val fmap_s : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
  val fmap_p : ('a -> 'b option Lwt.t) -> 'a event -> 'b event

  val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a event -> 'b event

  val accum_s : ('a -> 'a Lwt.t) event -> 'a -> 'a event

  val fold_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a event

  val merge_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event list -> 'a event

  val run_s : 'a Lwt.t event -> 'a event
  val run_p : 'a Lwt.t event -> 'a event
end

module S : sig
  include module type of React.S

  (** {2 Monadic interface} *)

  val return : 'a -> 'a signal
    (** Same as [const]. *)

  val bind : ?eq : ('b -> 'b -> bool) -> 'a signal -> ('a -> 'b signal) -> 'b signal
    (** [bind ?eq s f] is initially [f x] where [x] is the current
        value of [s]. Each time [s] changes to a new value [y], [bind
        signal f] is set to [f y], until the next change of
        [signal]. *)

  val bind_s : ?eq : ('b -> 'b -> bool) -> 'a signal -> ('a -> 'b signal Lwt.t) -> 'b signal Lwt.t
    (** Same as {!bind} except that [f] returns a promise. Calls to [f]
        are serialized. *)

  (** {2 Lwt-specific utilities} *)

  val with_finaliser : (unit -> unit) -> 'a signal -> 'a signal
    (** [with_finaliser f s] returns a signal [s'] which behaves as
        [s], except that [f] is called when [s'] is garbage
        collected. *)

  val limit : ?eq : ('a -> 'a -> bool) -> (unit -> unit Lwt.t) -> 'a signal -> 'a signal
    (** [limit f s] limits the rate of [s] update with [f].

        For example, to limit it to 1 per second, you can use: [limit
        (fun () -> Lwt_unix.sleep 1.0) s]. *)

  val keep : 'a signal -> unit
    (** [keep s] keeps a reference to [s] so it will never be garbage
        collected. *)

  (** {2 Threaded versions of React transformation functions} *)

  (** The following functions behave as their [React] counterpart,
      except that they take functions that may yield.

      The [_s] suffix means that calls are serialized.
  *)

  val app_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) signal -> 'a signal -> 'b signal Lwt.t

  val map_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'a signal -> 'b signal Lwt.t

  val filter_s : ?eq : ('a -> 'a -> bool) -> ('a -> bool Lwt.t) -> 'a -> 'a signal -> 'a signal Lwt.t

  val fmap_s : ?eq:('b -> 'b -> bool) -> ('a -> 'b option Lwt.t) -> 'b -> 'a signal -> 'b signal Lwt.t

  val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a signal -> 'b event

  val sample_s : ('b -> 'a -> 'c Lwt.t) -> 'b event -> 'a signal -> 'c event

  val accum_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'a Lwt.t) event -> 'a -> 'a signal

  val fold_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a signal

  val merge_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b signal list -> 'a signal Lwt.t

  val l1_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'a signal -> 'b signal Lwt.t
  val l2_s : ?eq : ('c -> 'c -> bool) -> ('a -> 'b -> 'c Lwt.t) -> 'a signal -> 'b signal -> 'c signal Lwt.t
  val l3_s : ?eq : ('d -> 'd -> bool) -> ('a -> 'b -> 'c -> 'd Lwt.t) -> 'a signal -> 'b signal -> 'c signal -> 'd signal Lwt.t
  val l4_s : ?eq : ('e -> 'e -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) -> 'a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal Lwt.t
  val l5_s : ?eq : ('f -> 'f -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) -> 'a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal Lwt.t
  val l6_s : ?eq : ('g -> 'g -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) -> 'a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal -> 'g signal Lwt.t

  val run_s : ?eq : ('a -> 'a -> bool) -> 'a Lwt.t signal -> 'a signal Lwt.t
end

(**/**)

val opaque_identity : 'a -> 'a
