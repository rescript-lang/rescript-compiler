(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Infix

let opaque_identity x =
  Sys.opaque_identity x

type 'a event = 'a React.event
type 'a signal = 'a React.signal

module E = struct
  include React.E

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f event =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map (fun x -> ignore (opaque_identity r); x) event

  let next ev =
    let waiter, wakener = Lwt.task () in
    let ev = map (fun x -> Lwt.wakeup wakener x) (once ev) in
    Lwt.on_cancel waiter (fun () -> stop ev);
    waiter

  let limit f e =
    (* Thread which prevents [e] from occurring while it is sleeping *)
    let limiter = ref Lwt.return_unit in

    (* The occurrence that is delayed until the limiter returns. *)
    let delayed = ref None in

    (* The resulting event. *)
    let event, push = create () in

    let iter =
      fmap
        (fun x ->
           if Lwt.is_sleeping !limiter then begin
             (* The limiter is sleeping, we queue the event for later
                delivering. *)
             match !delayed with
             | Some cell ->
               (* An occurrence is already queued, replace it. *)
               cell := x;
               None
             | None ->
               let cell = ref x in
               delayed := Some cell;
               Lwt.on_success !limiter (fun () ->
                  if Lwt.is_sleeping !limiter then
                    delayed := None
                  else
                    let x = !cell in
                    delayed := None;
                    limiter := f ();
                    push x);
               None
           end else begin
             (* Set the limiter for future events. *)
             limiter := f ();
             (* Send the occurrence now. *)
             push x;
             None
           end)
        e
    in

    select [iter; event]

  let cancel_thread t () =
    Lwt.cancel t

  let from f =
    let event, push = create () in
    let rec loop () =
      f () >>= fun x ->
      push x;
      loop ()
    in
    let t = Lwt.pause () >>= loop in
    with_finaliser (cancel_thread t) event

  let to_stream event =
    let stream, push, set_ref = Lwt_stream.create_with_reference () in
    set_ref (map (fun x -> push (Some x)) event);
    stream

  let of_stream stream =
    let event, push = create () in
    let t =
      Lwt.pause () >>= fun () -> Lwt_stream.iter (fun v -> push v) stream in
    with_finaliser (cancel_thread t) event

  let delay thread =
    match Lwt.poll thread with
    | Some e ->
      e
    | None ->
      let event, send = create () in
      Lwt.on_success thread (fun e -> send e; stop event);
      switch never event

  let keeped = ref []

  let keep e =
    keeped := map ignore e :: !keeped

  (* +---------------------------------------------------------------+
     | Event transformations                                         |
     +---------------------------------------------------------------+ *)

  let run_p e =
    let event, push = create () in
    let iter = fmap (fun t -> Lwt.on_success t (fun v -> push v); None) e in
    select [iter; event]

  let run_s e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun t ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> t))
             (fun v -> push v);
           None) e
    in
    select [iter; event]

  let map_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> Lwt.on_success (f x) (fun v -> push v); None) e in
    select [iter; event]

  let map_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun x ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x))
             (fun v -> push v);
           None) e
    in
    select [iter; event]

  let app_p ef e =
    let event, push = create () in
    let iter =
      fmap
        (fun (f, x) ->
           Lwt.on_success (f x) (fun v -> push v);
           None)
        (app (map (fun f x -> (f, x)) ef) e)
    in
    select [iter; event]

  let app_s ef e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun (f, x) ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x))
             (fun v -> push v);
           None)
        (app (map (fun f x -> (f, x)) ef) e)
    in
    select [iter; event]

  let filter_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> Lwt.on_success (f x) (function true -> push x | false -> ()); None) e in
    select [iter; event]

  let filter_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function true -> push x | false -> ()); None) e in
    select [iter; event]

  let fmap_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> Lwt.on_success (f x) (function Some x -> push x | None -> ()); None) e in
    select [iter; event]

  let fmap_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function Some x -> push x | None -> ()); None) e in
    select [iter; event]

  let diff_s f e =
    let previous = ref None in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun x ->
           match !previous with
           | None ->
             previous := Some x;
             None
           | Some y ->
             previous := Some x;
             Lwt.on_success
               (Lwt_mutex.with_lock mutex (fun () -> f x y))
               (fun v -> push v);
             None)
        e
    in
    select [iter; event]

  let accum_s ef acc =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun f -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f !acc)) (fun x -> acc := x; push x); None) ef in
    select [iter; event]

  let fold_s f acc e =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f !acc x)) (fun x -> acc := x; push x); None) e in
    select [iter; event]

  let rec rev_fold f acc = function
    | [] ->
      Lwt.return acc
    | x :: l ->
      rev_fold f acc l >>= fun acc ->
      f acc x

  let merge_s f acc el =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun l ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc l))
             (fun v -> push v);
           None)
        (merge (fun acc x -> x :: acc) [] el)
    in
    select [iter; event]
end

module S = struct
  include React.S

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f signal =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map
      (fun x -> ignore (Sys.opaque_identity r); x)
      signal

  let limit ?eq f s =
    (* Thread which prevent [s] to changes while it is sleeping *)
    let limiter = ref (f ()) in

    (* The occurrence that is delayed until the limiter returns. *)
    let delayed = ref None in

    (* The resulting event. *)
    let event, push = E.create () in

    let iter =
      E.fmap
        (fun x ->
           if Lwt.is_sleeping !limiter then begin
             (* The limiter is sleeping, we queue the event for later
                delivering. *)
             match !delayed with
             | Some cell ->
               (* An occurrence is already queued, replace it. *)
               cell := x;
               None
             | None ->
               let cell = ref x in
               delayed := Some cell;
               Lwt.on_success !limiter (fun () ->
                if Lwt.is_sleeping !limiter then
                  delayed := None
                else
                  let x = !cell in
                  delayed := None;
                  limiter := f ();
                  push x);
               None
           end else begin
             (* Set the limiter for future events. *)
             limiter := f ();
             (* Send the occurrence now. *)
             push x;
             None
           end)
        (changes s)
    in

    hold ?eq (value s) (E.select [iter; event])

  let keeped = ref []

  let keep s =
    keeped := map ignore s :: !keeped

  (* +---------------------------------------------------------------+
     | Signal transformations                                        |
     +---------------------------------------------------------------+ *)

  let run_s ?eq s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun t ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> t))
             (fun v -> push v);
           None)
        (changes s)
    in
    Lwt_mutex.with_lock mutex (fun () -> value s) >>= fun x ->
    Lwt.return (hold ?eq x (E.select [iter; event]))

  let map_s ?eq f s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x)) (fun v -> push v);
           None)
        (changes s)
    in
    Lwt_mutex.with_lock mutex (fun () -> f (value s)) >>= fun x ->
    Lwt.return (hold ?eq x (E.select [iter; event]))

  let app_s ?eq sf s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun (f, x) ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x))
             (fun v -> push v);
           None)
        (E.app (E.map (fun f x -> (f, x)) (changes sf)) (changes s))
    in
    Lwt_mutex.with_lock mutex (fun () -> (value sf) (value s)) >>= fun x ->
    Lwt.return (hold ?eq x (E.select [iter; event]))

  let filter_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function true -> push x | false -> ()); None) (changes s) in
    let x = value s in
    Lwt_mutex.with_lock mutex (fun () -> f x) >>= function
    | true ->
      Lwt.return (hold ?eq x (E.select [iter; event]))
    | false ->
      Lwt.return (hold ?eq i (E.select [iter; event]))

  let fmap_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> Lwt.on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function Some x -> push x | None -> ()); None) (changes s) in
    Lwt_mutex.with_lock mutex (fun () -> f (value s)) >>= function
    | Some x ->
      Lwt.return (hold ?eq x (E.select [iter; event]))
    | None ->
      Lwt.return (hold ?eq i (E.select [iter; event]))

  let diff_s f s =
    let previous = ref (value s) in
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           let y = !previous in
           previous := x;
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x y))
             (fun v -> push v);
           None)
        (changes s)
    in
    E.select [iter; event]

  let sample_s f e s =
    E.map_s (fun x -> f x (value s)) e

  let accum_s ?eq ef i =
    hold ?eq i (E.accum_s ef i)

  let fold_s ?eq f i e =
    hold ?eq i (E.fold_s f i e)

  let rec rev_fold f acc = function
    | [] ->
      Lwt.return acc
    | x :: l ->
      rev_fold f acc l >>= fun acc ->
      f acc x

  let merge_s ?eq f acc sl =
    let s = merge (fun acc x -> x :: acc) [] sl in
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun l ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc l))
             (fun v -> push v);
           None)
        (changes s)
    in
    Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc (value s)) >>= fun x ->
    Lwt.return (hold ?eq x (E.select [iter; event]))

  let l1_s ?eq f s1 =
    map_s ?eq f s1

  let l2_s ?eq f s1 s2 =
    map_s ?eq (fun (x1, x2) -> f x1 x2) (l2 (fun x1 x2 -> (x1, x2)) s1 s2)

  let l3_s ?eq f s1 s2 s3 =
    map_s ?eq (fun (x1, x2, x3) -> f x1 x2 x3) (l3 (fun x1 x2 x3-> (x1, x2, x3)) s1 s2 s3)

  let l4_s ?eq f s1 s2 s3 s4 =
    map_s ?eq (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4) (l4 (fun x1 x2 x3 x4-> (x1, x2, x3, x4)) s1 s2 s3 s4)

  let l5_s ?eq f s1 s2 s3 s4 s5 =
    map_s ?eq (fun (x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5) (l5 (fun x1 x2 x3 x4 x5-> (x1, x2, x3, x4, x5)) s1 s2 s3 s4 s5)

  let l6_s ?eq f s1 s2 s3 s4 s5 s6 =
    map_s ?eq (fun (x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6) (l6 (fun x1 x2 x3 x4 x5 x6-> (x1, x2, x3, x4, x5, x6)) s1 s2 s3 s4 s5 s6)

  (* +---------------------------------------------------------------+
     | Monadic interface                                             |
     +---------------------------------------------------------------+ *)

  let return =
    const

  let bind_s ?eq s f =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           Lwt.on_success
             (Lwt_mutex.with_lock mutex (fun () -> f x))
             (fun v -> push v);
           None)
        (changes s)
    in
    Lwt_mutex.with_lock mutex (fun () -> f (value s)) >>= fun x ->
    Lwt.return (switch ?eq (hold ~eq:( == ) x (E.select [iter; event])))
end
