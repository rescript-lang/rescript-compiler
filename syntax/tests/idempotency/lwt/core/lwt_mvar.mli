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

(** Mailbox variables *)

(** "Mailbox" variables implement a synchronising variable, used for
    communication between concurrent threads. *)

type 'a t
  (** The type of a mailbox variable. Mailbox variables are used to
      communicate values between threads in a synchronous way. The
      type parameter specifies the type of the value propagated from
      [put] to [take]. *)

val create : 'a -> 'a t
  (** [create v] creates a new mailbox variable containing value [v]. *)

val create_empty : unit -> 'a t
  (** [create ()] creates a new empty mailbox variable. *)

val put : 'a t -> 'a -> unit Lwt.t
  (** [put mvar value] puts a value into a mailbox variable. This
      value will remain in the mailbox until [take] is called to
      remove it. If the mailbox is not empty, the current thread will
      block until it is emptied. *)

val take : 'a t -> 'a Lwt.t
  (** [take mvar] will take any currently available value from the
      mailbox variable. If no value is currently available, the
      current thread will block, awaiting a value to be [put] by
      another thread. *)

val take_available : 'a t -> 'a option
(** [take_available mvar] immediately takes the value from [mvar] without
    blocking, returning [None] if the mailbox is empty.

    @since 3.2.0 *)

val is_empty : 'a t -> bool
(** [is_empty mvar] indicates if [put mvar] can be called without blocking.

    @since 3.2.0 *)
