(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Rate limiters.

    A rate limiter allows generating sets of promises that will be resolved in
    the future, at a maximum rate of N promises per second.

    The rate limiters in this module support multiple {e channels}, each given a
    different key by the user. The rate limit applies to each channel
    independently. *)

module type S = sig
  type key
  type t

  val create : rate:int -> max:int -> n:int -> t
  (** Creates a rate limiter.

      @param rate Maximum number of promise resolutions per second, per channel.
      @param max Maximum number of pending promises allowed at once, over all
        channels.
      @param n Initial size of the internal channel hash table. This should be
        approximately the number of different channels that will be used. *)

  val wait : t -> key -> bool Lwt.t
  (** [Lwt_throttle.wait limiter channel] returns a new promise associated with
      the given rate limiter and channel.

      If the maximum number of pending promises for [limiter] has {e not} been
      reached, the promise starts pending. It will be resolved with [true] at
      some future time, such that the rate limit of [limiter] is not exceeded,
      with respect to other promises in the same [channel].

      If the maximum number of pending promises has been reached, the returned
      promise is already resolved with [false]. *)
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t
