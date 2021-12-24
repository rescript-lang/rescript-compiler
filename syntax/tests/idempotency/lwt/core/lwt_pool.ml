(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

open Lwt.Infix

type 'a t = {
  create : unit -> 'a Lwt.t;
  (* Create a new pool member. *)
  check : 'a -> (bool -> unit) -> unit;
  (* Check validity of a pool member when use resulted in failed promise. *)
  validate : 'a -> bool Lwt.t;
  (* Validate an existing free pool member before use. *)
  dispose : 'a -> unit Lwt.t;
  (* Dispose of a pool member. *)
  cleared : bool ref ref;
  (* Have the current pool elements been cleared out? *)
  max : int;
  (* Size of the pool. *)
  mutable count : int;
  (* Number of elements in the pool. *)
  list : 'a Queue.t;
  (* Available pool members. *)
  waiters : 'a Lwt.u Lwt_sequence.t;
  (* Promise resolvers waiting for a free member. *)
}

let create m ?(validate = fun _ -> Lwt.return_true) ?(check = fun _ f -> f true) ?(dispose = fun _ -> Lwt.return_unit) create =
  { max = m;
    create = create;
    validate = validate;
    check = check;
    dispose = dispose;
    cleared = ref (ref false);
    count = 0;
    list = Queue.create ();
    waiters = Lwt_sequence.create () }

(* Create a pool member. *)
let create_member p =
  Lwt.catch
    (fun () ->
       (* Must be done before p.create to prevent other resolvers from
          creating new members if the limit is reached. *)
       p.count <- p.count + 1;
       p.create ())
    (fun exn ->
       (* Creation failed, so don't increment count. *)
       p.count <- p.count - 1;
       Lwt.fail exn)

(* Release a pool member. *)
let release p c =
  match Lwt_sequence.take_opt_l p.waiters with
  | Some wakener ->
    (* A promise resolver is waiting, give it the pool member. *)
    Lwt.wakeup_later wakener c
  | None ->
    (* No one is waiting, queue it. *)
    Queue.push c p.list

(* Dispose of a pool member. *)
let dispose p c =
  p.dispose c >>= fun () ->
  p.count <- p.count - 1;
  Lwt.return_unit

(* Create a new member when one is thrown away. *)
let replace_disposed p =
  match Lwt_sequence.take_opt_l p.waiters with
  | None ->
    (* No one is waiting, do not create a new member to avoid
       losing an error if creation fails. *)
    ()
  | Some wakener ->
    Lwt.on_any
      (Lwt.apply p.create ())
      (fun c ->
         Lwt.wakeup_later wakener c)
      (fun exn ->
         (* Creation failed, notify the waiter of the failure. *)
         Lwt.wakeup_later_exn wakener exn)

(* Verify a member is still valid before using it. *)
let validate_and_return p c =
  Lwt.try_bind
      (fun () ->
         p.validate c)
      (function
        | true ->
          Lwt.return c
        | false ->
          (* Remove this member and create a new one. *)
          dispose p c >>= fun () ->
          create_member p)
      (fun e ->
         (* Validation failed: create a new member if at least one
            resolver is waiting. *)
         dispose p c >>= fun () ->
         replace_disposed p;
         Lwt.fail e)

(* Acquire a pool member. *)
let acquire p =
  if Queue.is_empty p.list then
    (* No more available member. *)
    if p.count < p.max then
      (* Limit not reached: create a new one. *)
      create_member p
    else
      (* Limit reached: wait for a free one. *)
      (Lwt.add_task_r [@ocaml.warning "-3"]) p.waiters >>= validate_and_return p
  else
    (* Take the first free member and validate it. *)
    let c = Queue.take p.list in
    validate_and_return p c

(* Release a member when use resulted in failed promise if the member
   is still valid. *)
let check_and_release p c cleared =
  let ok = ref false in
  p.check c (fun result -> ok := result);
  if cleared || not !ok then (
    (* Element is not ok or the pool was cleared - dispose of it *)
    dispose p c
  )
  else (
    (* Element is ok - release it back to the pool *)
    release p c;
    Lwt.return_unit
  )

let use p f =
  acquire p >>= fun c ->
  (* Capture the current cleared state so we can see if it changes while this
     element is in use *)
  let cleared = !(p.cleared) in
  let promise =
    Lwt.catch
      (fun () -> f c)
      (fun e ->
         check_and_release p c !cleared >>= fun () ->
         Lwt.fail e)
  in
  promise >>= fun _ ->
  if !cleared then (
    (* p was cleared while promise was resolving - dispose of this element *)
    dispose p c >>= fun () ->
    promise
  )
  else (
    release p c;
    promise
  )

let clear p =
  let elements = Queue.fold (fun l element -> element :: l) [] p.list in
  Queue.clear p.list;
  (* Indicate to any currently in-use elements that we cleared the pool *)
  let old_cleared = !(p.cleared) in
  old_cleared := true;
  p.cleared := ref false;
  Lwt_list.iter_s (dispose p) elements

let wait_queue_length p = Lwt_sequence.length p.waiters
