(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
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

(* This code is adapted from
   https://web.archive.org/web/20101001215425/http://eigenclass.org:80/hiki/lightweight-threads-with-lwt. *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

type 'a t = {
  mutable mvar_contents : 'a option;
  (* Current contents *)

  writers : ('a * unit Lwt.u) Lwt_sequence.t;
  (* Threads waiting to put a value *)

  readers : 'a Lwt.u Lwt_sequence.t;
  (* Threads waiting for a value *)
}

let create_empty () =
  { mvar_contents = None;
    writers = Lwt_sequence.create ();
    readers = Lwt_sequence.create () }

let create v =
  { mvar_contents = Some v;
    writers = Lwt_sequence.create ();
    readers = Lwt_sequence.create () }

let put mvar v =
  match mvar.mvar_contents with
  | None ->
    begin match Lwt_sequence.take_opt_l mvar.readers with
      | None ->
        mvar.mvar_contents <- Some v
      | Some w ->
        Lwt.wakeup_later w v
    end;
    Lwt.return_unit
  | Some _ ->
    let (res, w) = Lwt.task () in
    let node = Lwt_sequence.add_r (v, w) mvar.writers in
    Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
    res

let next_writer mvar =
  match Lwt_sequence.take_opt_l mvar.writers with
  | Some(v', w) ->
    mvar.mvar_contents <- Some v';
    Lwt.wakeup_later w ()
  | None ->
    mvar.mvar_contents <- None

let take_available mvar =
  match mvar.mvar_contents with
  | Some v ->
    next_writer mvar;
    Some v
  | None ->
    None

let take mvar =
  match take_available mvar with
  | Some v -> Lwt.return v
  | None -> (Lwt.add_task_r [@ocaml.warning "-3"]) mvar.readers

let is_empty mvar =
  match mvar.mvar_contents with
  | Some _ -> false
  | None -> true
