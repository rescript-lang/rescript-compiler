(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY METAWEB TECHNOLOGIES ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL METAWEB TECHNOLOGIES BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(** Conditions *)

(** Condition variables to synchronize between threads. *)

type 'a t
    (** Condition variable type. The type parameter denotes the type of
        value propagated from notifier to waiter. *)

val create : unit -> 'a t
    (** [create ()] creates a new condition variable. *)

val wait : ?mutex:Lwt_mutex.t -> 'a t -> 'a Lwt.t
    (** [wait mutex condvar] will cause the current thread to block,
        awaiting notification for a condition variable, [condvar]. If
        provided, the [mutex] must have been previously locked (within
        the scope of [Lwt_mutex.with_lock], for example) and is
        temporarily unlocked until the condition is notified. Upon
        notification, [mutex] is re-locked before [wait] returns and
        the thread's activity is resumed. When the awaited condition
        is notified, the value parameter passed to [signal] is
        returned. *)

val signal : 'a t -> 'a -> unit
    (** [signal condvar value] notifies that a condition is ready. A
        single waiting thread will be awoken and will receive the
        notification value which will be returned from [wait]. Note
        that condition notification is not "sticky", i.e. if there is
        no waiter when [signal] is called, the notification will be
        missed and the value discarded. *)

val broadcast : 'a t -> 'a -> unit
    (** [broadcast condvar value] notifies all waiting threads. Each
        will be awoken in turn and will receive the same notification
        value. *)

val broadcast_exn : 'a t -> exn -> unit
    (** [broadcast_exn condvar exn] fails all waiting threads with exception
        [exn].

        @since 2.6.0 *)
