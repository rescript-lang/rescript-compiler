(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   David Nowak and Xavier Leroy, projet Cristal, INRIA Rocquencourt     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Events *)
type 'a basic_event =
  { poll: unit -> bool;
      (* If communication can take place immediately, return true. *)
    suspend: unit -> unit;
      (* Offer the communication on the channel and get ready
         to suspend current process. *)
    result: unit -> 'a }
      (* Return the result of the communication *)

type 'a behavior = int ref -> Condition.t -> int -> 'a basic_event

type 'a event =
    Communication of 'a behavior
  | Choose of 'a event list
  | WrapAbort of 'a event * (unit -> unit)
  | Guard of (unit -> 'a event)

(* Communication channels *)
type 'a channel =
  { mutable writes_pending: 'a communication Queue.t;
                        (* All offers to write on it *)
    mutable reads_pending:  'a communication Queue.t }
                        (* All offers to read from it *)

(* Communication offered *)
and 'a communication =
  { performed: int ref;  (* -1 if not performed yet, set to the number *)
                         (* of the matching communication after rendez-vous. *)
    condition: Condition.t;             (* To restart the blocked thread. *)
    mutable data: 'a option;            (* The data sent or received. *)
    event_number: int }                 (* Event number in select *)

(* Create a channel *)

let new_channel () =
  { writes_pending = Queue.create();
    reads_pending = Queue.create() }

(* Basic synchronization function *)

let masterlock = Mutex.create()

let do_aborts abort_env genev performed =
  if abort_env <> [] then begin
    if performed >= 0 then begin
      let ids_done = snd genev.(performed) in
      List.iter
        (fun (id,f) -> if not (List.mem id ids_done) then f ())
        abort_env
    end else begin
      List.iter (fun (_,f) -> f ()) abort_env
    end
  end

let basic_sync abort_env genev =
  let performed = ref (-1) in
  let condition = Condition.create() in
  let bev = Array.make (Array.length genev)
                         (fst (genev.(0)) performed condition 0) in
  for i = 1 to Array.length genev - 1 do
    bev.(i) <- (fst genev.(i)) performed condition i
  done;
  (* See if any of the events is already activable *)
  let rec poll_events i =
    if i >= Array.length bev
    then false
    else bev.(i).poll() || poll_events (i+1) in
  Mutex.lock masterlock;
  if not (poll_events 0) then begin
    (* Suspend on all events *)
    for i = 0 to Array.length bev - 1 do bev.(i).suspend() done;
    (* Wait until the condition is signalled *)
    Condition.wait condition masterlock
  end;
  Mutex.unlock masterlock;
  (* Extract the result *)
  if abort_env = [] then
    (* Preserve tail recursion *)
    bev.(!performed).result()
  else begin
    let num = !performed in
    let result = bev.(num).result() in
    (* Handle the aborts and return the result *)
    do_aborts abort_env genev num;
    result
  end

(* Apply a random permutation on an array *)

let scramble_array a =
  let len = Array.length a in
  if len = 0 then invalid_arg "Event.choose";
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp
  done;
  a

(* Main synchronization function *)

let gensym = let count = ref 0 in fun () -> incr count; !count

let rec flatten_event
      (abort_list : int list)
      (accu : ('a behavior * int list) list)
      (accu_abort : (int * (unit -> unit)) list)
      ev =
  match ev with
     Communication bev -> ((bev,abort_list) :: accu) , accu_abort
  | WrapAbort (ev,fn) ->
      let id = gensym () in
      flatten_event (id :: abort_list) accu ((id,fn)::accu_abort) ev
  | Choose evl ->
      let rec flatten_list accu' accu_abort'= function
         ev :: l ->
           let (accu'',accu_abort'') =
             flatten_event abort_list accu' accu_abort' ev in
           flatten_list accu'' accu_abort'' l
       | [] -> (accu',accu_abort') in
      flatten_list accu accu_abort evl
  | Guard fn -> flatten_event abort_list accu accu_abort (fn ())

let sync ev =
  let (evl,abort_env) = flatten_event [] [] [] ev in
  basic_sync abort_env (scramble_array(Array.of_list evl))

(* Event polling -- like sync, but non-blocking *)

let basic_poll abort_env genev =
  let performed = ref (-1) in
  let condition = Condition.create() in
  let bev = Array.make(Array.length genev)
                        (fst genev.(0) performed condition 0) in
  for i = 1 to Array.length genev - 1 do
    bev.(i) <- fst genev.(i) performed condition i
  done;
  (* See if any of the events is already activable *)
  let rec poll_events i =
    if i >= Array.length bev
    then false
    else bev.(i).poll() || poll_events (i+1) in
  Mutex.lock masterlock;
  let ready = poll_events 0 in
  if ready then begin
    (* Extract the result *)
    Mutex.unlock masterlock;
    let result = Some(bev.(!performed).result()) in
    do_aborts abort_env genev !performed; result
  end else begin
    (* Cancel the communication offers *)
    performed := 0;
    Mutex.unlock masterlock;
    do_aborts abort_env genev (-1);
    None
  end

let poll ev =
  let (evl,abort_env) = flatten_event [] [] [] ev in
  basic_poll abort_env (scramble_array(Array.of_list evl))

(* Remove all communication opportunities already synchronized *)

let cleanup_queue q =
  let q' = Queue.create() in
  Queue.iter (fun c -> if !(c.performed) = -1 then Queue.add c q') q;
  q'

(* Event construction *)

let always data =
  Communication(fun performed condition evnum ->
    { poll = (fun () -> performed := evnum; true);
      suspend = (fun () -> ());
      result = (fun () -> data) })

let send channel data =
  Communication(fun performed condition evnum ->
    let wcomm =
      { performed = performed;
        condition = condition;
        data = Some data;
        event_number = evnum } in
    { poll = (fun () ->
        let rec poll () =
          let rcomm = Queue.take channel.reads_pending in
          if !(rcomm.performed) >= 0 then
            poll ()
          else begin
            rcomm.data <- wcomm.data;
            performed := evnum;
            rcomm.performed := rcomm.event_number;
            Condition.signal rcomm.condition
          end in
        try
          poll();
          true
        with Queue.Empty ->
          false);
      suspend = (fun () ->
        channel.writes_pending <- cleanup_queue channel.writes_pending;
        Queue.add wcomm channel.writes_pending);
      result = (fun () -> ()) })

let receive channel =
  Communication(fun performed condition evnum ->
    let rcomm =
      { performed = performed;
        condition = condition;
        data = None;
        event_number = evnum } in
    { poll = (fun () ->
        let rec poll () =
          let wcomm = Queue.take channel.writes_pending in
          if !(wcomm.performed) >= 0 then
            poll ()
          else begin
            rcomm.data <- wcomm.data;
            performed := evnum;
            wcomm.performed := wcomm.event_number;
            Condition.signal wcomm.condition
          end in
        try
          poll();
          true
        with Queue.Empty ->
          false);
    suspend = (fun () ->
      channel.reads_pending <- cleanup_queue channel.reads_pending;
      Queue.add rcomm channel.reads_pending);
    result = (fun () ->
      match rcomm.data with
        None -> invalid_arg "Event.receive"
      | Some res -> res) })

let choose evl = Choose evl

let wrap_abort ev fn = WrapAbort(ev,fn)

let guard fn = Guard fn

let rec wrap ev fn =
  match ev with
    Communication genev ->
      Communication(fun performed condition evnum ->
        let bev = genev performed condition evnum in
        { poll = bev.poll;
          suspend = bev.suspend;
          result = (fun () -> fn(bev.result())) })
  | Choose evl ->
      Choose(List.map (fun ev -> wrap ev fn) evl)
  | WrapAbort (ev, f') ->
      WrapAbort (wrap ev fn, f')
  | Guard gu ->
      Guard(fun () -> wrap (gu()) fn)

(* Convenience functions *)

let select evl = sync(Choose evl)
