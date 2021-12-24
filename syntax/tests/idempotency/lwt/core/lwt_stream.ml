(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Infix

exception Closed
exception Full
exception Empty

(* A node in a queue of pending data. *)
type 'a node = {
  mutable next : 'a node;
  (* Next node in the queue. For the last node it points to itself. *)
  mutable data : 'a option;
  (* Data of this node. For the last node it is always [None]. *)
}

(* Note: a queue for an exhausted stream is represented by a node
   containing [None] followed by a node with itself as next and [None]
   as data. *)

let new_node () =
  let rec node = { next = node; data = None } in
  node

(* Type of a stream source using a function to create new elements. *)
type 'a from = {
  from_create : unit -> 'a option Lwt.t;
  (* Function used to create new elements. *)
  mutable from_thread : unit Lwt.t;
  (* Thread which:

     - wait for the thread returned by the last call to [from_next],
     - add the next element to the end of the queue.

     If it is a sleeping thread, then it must be used instead of creating a
     new one with [from_create]. *)
}

(* Type of a stream source for push streams. *)
type push = {
  mutable push_signal : unit Lwt.t;
  (* Thread signaled when a new element is added to the stream. *)
  mutable push_waiting : bool;
  (* Is a thread waiting on [push_signal] ? *)
  mutable push_external : Obj.t;
  (* Reference to an external source. *)
}

(* Type of a stream source for bounded-push streams. *)
type 'a push_bounded = {
  mutable pushb_signal : unit Lwt.t;
  (* Thread signaled when a new element is added to the stream. *)
  mutable pushb_waiting : bool;
  (* Is a thread waiting on [pushb_signal] ? *)
  mutable pushb_size : int;
  (* Size of the queue. *)
  mutable pushb_count : int;
  (* Current length of the queue. *)
  mutable pushb_pending : 'a option;
  (* The next element to push if a thread blocked on push. We store it
     here to be sure it will be the first element to be added when
     space becomes available. *)
  mutable pushb_push_waiter : unit Lwt.t;
  mutable pushb_push_wakener : unit Lwt.u;
  (* Thread blocked on push. *)
  mutable pushb_external : Obj.t;
  (* Reference to an external source. *)
}

(* Source of a stream. *)
type 'a source =
  | From of 'a from
  | From_direct of (unit -> 'a option)
  | Push of push
  | Push_bounded of 'a push_bounded

type 'a t = {
  source : 'a source;
  (* The source of the stream. *)
  close : unit Lwt.u;
  (* A waiter for a thread that sleeps until the stream is closed. *)
  mutable node : 'a node;
  (* Pointer to first pending element, or to [last] if there is no
     pending element. *)
  last : 'a node ref;
  (* Node marking the end of the queue of pending elements. *)
}

(* class type ['a] bounded_push = object *)
  (* method size : int *)
  (* method resize : int -> unit *)
  (* method push : 'a -> unit Lwt.t *)
  (* method close : unit *)
  (* method count : int *)
  (* method blocked : bool *)
  (* method closed : bool *)
  (* method set_reference : 'a. 'a -> unit *)
(* end *)

(* The only difference between two clones is the pointer to the first
   pending element. *)
let clone s =
  (match s.source with
   | Push_bounded _ -> invalid_arg "Lwt_stream.clone"
   | From _ | From_direct _ | Push _ -> ());
  {
    source = s.source;
    close = s.close;
    node = s.node;
    last = s.last;
  }

let from_source source =
  let last = new_node () in
  let _, close = Lwt.wait () in
  { source = source
  ; close = close
  ; node = last
  ; last = ref last
  }

let from f =
  from_source (From { from_create = f; from_thread = Lwt.return_unit })

let from_direct f =
  from_source (From_direct f)

let closed s =
  (Lwt.waiter_of_wakener [@ocaml.warning "-3"]) s.close

let is_closed s =
  not (Lwt.is_sleeping (closed s))

let on_termination s f =
  Lwt.async (fun () -> closed s >|= f)

let on_terminate = on_termination

let enqueue' e last =
  let node = !last
  and new_last = new_node () in
  node.data <- e;
  node.next <- new_last;
  last := new_last

let enqueue e s =
  enqueue' e s.last

let create_with_reference () =
  (* Create the source for notifications of new elements. *)
  let source, push_signal_resolver =
    let push_signal, push_signal_resolver = Lwt.wait () in
    ({ push_signal;
       push_waiting = false;
       push_external = Obj.repr () },
     ref push_signal_resolver)
  in
  let t = from_source (Push source) in
  (* [push] should not close over [t] so that it can be garbage collected even
   * there are still references to [push]. Unpack all the components of [t]
   * that [push] needs and reference those identifiers instead. *)
  let close = t.close and last = t.last in
  (* The push function. It does not keep a reference to the stream. *)
  let push x =
    let waiter_of_wakener = Lwt.waiter_of_wakener [@ocaml.warning "-3"] in
    if not (Lwt.is_sleeping (waiter_of_wakener close)) then raise Closed;
    (* Push the element at the end of the queue. *)
    enqueue' x last;
    (* Send a signal if at least one thread is waiting for a new
       element. *)
    if source.push_waiting then begin
      source.push_waiting <- false;
      (* Update threads. *)
      let old_push_signal_resolver = !push_signal_resolver in
      let new_waiter, new_push_signal_resolver = Lwt.wait () in
      source.push_signal <- new_waiter;
      push_signal_resolver := new_push_signal_resolver;
      (* Signal that a new value has been received. *)
      Lwt.wakeup_later old_push_signal_resolver ()
    end;
    (* Do this at the end in case one of the function raise an
       exception. *)
    if x = None then Lwt.wakeup close ()
  in
  (t, push, fun x -> source.push_external <- Obj.repr x)

let of_seq s =
  let s = ref s in
  let get () =
    match !s () with
    | Seq.Nil -> None
    | Seq.Cons (elt, s') -> s := s'; Some elt
  in
  from_direct get

let create () =
  let source, push, _ = create_with_reference () in
  (source, push)

let of_iter iter i =
  let stream, push = create () in
  iter (fun x -> push (Some x)) i;
  push None;
  stream

let of_list l =
  of_iter List.iter l

let of_array a =
  of_iter Array.iter a

let of_string s =
  of_iter String.iter s

(* Add the pending element to the queue and notify the blocked pushed.

   Precondition: info.pushb_pending = Some _

   This does not modify info.pushb_count. *)
let notify_pusher info last =
  (* Push the element at the end of the queue. *)
  enqueue' info.pushb_pending last;
  (* Clear pending element. *)
  info.pushb_pending <- None;
  (* Wakeup the pusher. *)
  let old_wakener = info.pushb_push_wakener in
  let waiter, wakener = Lwt.task () in
  info.pushb_push_waiter <- waiter;
  info.pushb_push_wakener <- wakener;
  Lwt.wakeup_later old_wakener ()

(* class ['a] bounded_push_impl (info : 'a push_bounded) wakener_cell last close = object *)
  (* val mutable closed = false *)

  (* method size = *)
    (* info.pushb_size *)

  (* method resize size = *)
    (* if size < 0 then invalid_arg "Lwt_stream.bounded_push#resize"; *)
    (* info.pushb_size <- size; *)
    (* if info.pushb_count < info.pushb_size && info.pushb_pending <> None then begin *)
      (* info.pushb_count <- info.pushb_count + 1; *)
      (* notify_pusher info last *)
    (* end *)

  (* method push x = *)
    (* if closed then *)
      (* Lwt.fail Closed *)
    (* else if info.pushb_pending <> None then *)
      (* Lwt.fail Full *)
    (* else if info.pushb_count >= info.pushb_size then begin *)
      (* info.pushb_pending <- Some x; *)
      (* Lwt.catch *)
        (* (fun () -> info.pushb_push_waiter) *)
        (* (fun exn -> *)
           (* match exn with *)
           (* | Lwt.Canceled -> *)
             (* info.pushb_pending <- None; *)
             (* let waiter, wakener = Lwt.task () in *)
             (* info.pushb_push_waiter <- waiter; *)
             (* info.pushb_push_wakener <- wakener; *)
             (* Lwt.fail exn *)
           (* | _ -> *)
             (* Lwt.fail exn) *)
    (* end else begin *)
      (* (* Push the element at the end of the queue. *) *)
      (* enqueue' (Some x) last; *)
      (* info.pushb_count <- info.pushb_count + 1; *)
      (* (* Send a signal if at least one thread is waiting for a new *)
         (* element. *) *)
      (* if info.pushb_waiting then begin *)
        (* info.pushb_waiting <- false; *)
        (* (* Update threads. *) *)
        (* let old_wakener = !wakener_cell in *)
        (* let new_waiter, new_wakener = Lwt.wait () in *)
        (* info.pushb_signal <- new_waiter; *)
        (* wakener_cell := new_wakener; *)
        (* (* Signal that a new value has been received. *) *)
        (* Lwt.wakeup_later old_wakener () *)
      (* end; *)
      (* Lwt.return_unit *)
    (* end *)

  (* method close = *)
    (* if not closed then begin *)
      (* closed <- true; *)
      (* let node = !last and new_last = new_node () in *)
      (* node.data <- None; *)
      (* node.next <- new_last; *)
      (* last := new_last; *)
      (* if info.pushb_pending <> None then begin *)
        (* info.pushb_pending <- None; *)
        (* Lwt.wakeup_later_exn info.pushb_push_wakener Closed *)
      (* end; *)
      (* (* Send a signal if at least one thread is waiting for a new *)
         (* element. *) *)
      (* if info.pushb_waiting then begin *)
        (* info.pushb_waiting <- false; *)
        (* let old_wakener = !wakener_cell in *)
        (* (* Signal that a new value has been received. *) *)
        (* Lwt.wakeup_later old_wakener () *)
      (* end; *)
      (* Lwt.wakeup close (); *)
    (* end *)

  (* method count = *)
    (* info.pushb_count *)

  (* method blocked = *)
    (* info.pushb_pending <> None *)

  (* method closed = *)
    (* closed *)

  (* method set_reference : 'a. 'a -> unit = *)
    (* fun x -> info.pushb_external <- Obj.repr x *)
(* end *)

let create_bounded size =
  if size < 0 then invalid_arg "Lwt_stream.create_bounded";
  (* Create the source for notifications of new elements. *)
  let info, wakener_cell =
    let waiter, wakener = Lwt.wait () in
    let push_waiter, push_wakener = Lwt.task () in
    ({ pushb_signal = waiter;
       pushb_waiting = false;
       pushb_size = size;
       pushb_count = 0;
       pushb_pending = None;
       pushb_push_waiter = push_waiter;
       pushb_push_wakener = push_wakener;
       pushb_external = Obj.repr () },
     ref wakener)
  in
  let t = from_source (Push_bounded info) in
  (t, bounded_push_impl info wakener_cell t.last t.close)

(* Wait for a new element to be added to the queue of pending element
   of the stream. *)
let feed s =
  match s.source with
  | From from ->
    (* There is already a thread started to create a new element,
       wait for this one to terminate. *)
    if Lwt.is_sleeping from.from_thread then
      Lwt.protected from.from_thread
    else begin
      (* Otherwise request a new element. *)
      let thread =
        from.from_create () >>= fun x ->
        (* Push the element to the end of the queue. *)
        enqueue x s;
        if x = None then Lwt.wakeup s.close ();
        Lwt.return_unit
      in
      (* Allow other threads to access this thread. *)
      from.from_thread <- thread;
      Lwt.protected thread
    end
  | From_direct f ->
    let x = f () in
    (* Push the element to the end of the queue. *)
    enqueue x s;
    if x = None then Lwt.wakeup s.close ();
    Lwt.return_unit
  | Push push ->
    push.push_waiting <- true;
    Lwt.protected push.push_signal
  | Push_bounded push ->
    push.pushb_waiting <- true;
    Lwt.protected push.pushb_signal

(* Remove [node] from the top of the queue, or do nothing if it was
   already consumed.

   Precondition: node.data <> None
*)
let consume s node =
  if node == s.node then begin
    s.node <- node.next;
    match s.source with
    | Push_bounded info ->
      if info.pushb_pending = None then
        info.pushb_count <- info.pushb_count - 1
      else
        notify_pusher info s.last
    | From _ | From_direct _ | Push _ ->
      ()
  end

let rec peek_rec s node =
  if node == !(s.last) then
    feed s >>= fun () -> peek_rec s node
  else
    Lwt.return node.data

let peek s = peek_rec s s.node

let rec npeek_rec node acc n s =
  if n <= 0 then
    Lwt.return (List.rev acc)
  else if node == !(s.last) then
    feed s >>= fun () -> npeek_rec node acc n s
  else
    match node.data with
    | Some x ->
      npeek_rec node.next (x :: acc) (n - 1) s
    | None ->
      Lwt.return (List.rev acc)

let npeek n s = npeek_rec s.node [] n s

let rec get_rec s node =
  if node == !(s.last) then
    feed s >>= fun () -> get_rec s node
  else begin
    if node.data <> None then consume s node;
    Lwt.return node.data
  end

let get s = get_rec s s.node

type 'a result =
  | Value of 'a
  | Error of exn

let rec get_exn_rec s node =
  if node == !(s.last) then
    Lwt.try_bind
      (fun () -> feed s)
      (fun () -> get_exn_rec s node)
      (fun exn -> Lwt.return (Some (Error exn : _ result)))
      (* Note: the [Error] constructor above is from [Lwt_stream.result], not
         [Pervasives.result], nor its alias [Lwt.result]. [Lwt_stream.result] is
         a deprecated type, defined right above this function.

         The type constraint is necessary to avoid a warning about an ambiguous
         constructor. *)
  else
    match node.data with
    | Some value ->
      consume s node;
      Lwt.return (Some (Value value))
    | None ->
      Lwt.return_none

let map_exn s = from (fun () -> get_exn_rec s s.node)

let rec get_exn_rec' s node =
  if node == !(s.last) then
    Lwt.try_bind
      (fun () -> feed s)
      (fun () -> get_exn_rec' s node)
      (fun exn -> Lwt.return (Some (Result.Error exn)))
  else
    match node.data with
    | Some value ->
      consume s node;
      Lwt.return (Some (Result.Ok value))
    | None ->
      Lwt.return_none

let wrap_exn s = from (fun () -> get_exn_rec' s s.node)

let rec nget_rec node acc n s =
  if n <= 0 then
    Lwt.return (List.rev acc)
  else if node == !(s.last) then
    feed s >>= fun () -> nget_rec node acc n s
  else
    match s.node.data with
    | Some x ->
      consume s node;
      nget_rec node.next (x :: acc) (n - 1) s
    | None ->
      Lwt.return (List.rev acc)

let nget n s = nget_rec s.node [] n s

let rec get_while_rec node acc f s =
  if node == !(s.last) then
    feed s >>= fun () -> get_while_rec node acc f s
  else
    match node.data with
    | Some x ->
      let test = f x in
      if test then begin
        consume s node;
        get_while_rec node.next (x :: acc) f s
      end else
        Lwt.return (List.rev acc)
    | None ->
      Lwt.return (List.rev acc)

let get_while f s = get_while_rec s.node [] f s

let rec get_while_s_rec node acc f s =
  if node == !(s.last) then
    feed s >>= fun () -> get_while_s_rec node acc f s
  else
    match node.data with
    | Some x -> begin
        f x >>= function
        | true ->
          consume s node;
          get_while_s_rec node.next (x :: acc) f s
        | false ->
          Lwt.return (List.rev acc)
      end
    | None ->
      Lwt.return (List.rev acc)

let get_while_s f s = get_while_s_rec s.node [] f s

let rec next_rec s node =
  if node == !(s.last) then
    feed s >>= fun () -> next_rec s node
  else
    match node.data with
    | Some x ->
      consume s node;
      Lwt.return x
    | None ->
      Lwt.fail Empty

let next s = next_rec s s.node

let rec last_new_rec node x s =
  if node == !(s.last) then
    let thread = feed s in
    match Lwt.state thread with
    | Lwt.Return _ ->
      last_new_rec node x s
    | Lwt.Fail exn ->
      Lwt.fail exn
    | Lwt.Sleep ->
      Lwt.return x
  else
    match node.data with
    | Some x ->
      consume s node;
      last_new_rec node.next x s
    | None ->
      Lwt.return x

let last_new s =
  let node = s.node in
  if node == !(s.last) then
    let thread = next s in
    match Lwt.state thread with
    | Lwt.Return x ->
      last_new_rec node x s
    | Lwt.Fail _ | Lwt.Sleep ->
      thread
  else
    match node.data with
    | Some x ->
      consume s node;
      last_new_rec node.next x s
    | None ->
      Lwt.fail Empty

let rec to_list_rec node acc s =
  if node == !(s.last) then
    feed s >>= fun () -> to_list_rec node acc s
  else
    match node.data with
    | Some x ->
      consume s node;
      to_list_rec node.next (x :: acc) s
    | None ->
      Lwt.return (List.rev acc)

let to_list s = to_list_rec s.node [] s

let rec to_string_rec node buf s =
  if node == !(s.last) then
    feed s >>= fun () -> to_string_rec node buf s
  else
    match node.data with
    | Some x ->
      consume s node;
      Buffer.add_char buf x;
      to_string_rec node.next buf s
    | None ->
      Lwt.return (Buffer.contents buf)

let to_string s = to_string_rec s.node (Buffer.create 128) s

let junk s =
  let node = s.node in
  if node == !(s.last) then begin
    feed s >>= fun () ->
    if node.data <> None then consume s node;
    Lwt.return_unit
  end else begin
    if node.data <> None then consume s node;
    Lwt.return_unit
  end

let rec njunk_rec node n s =
  if n <= 0 then
    Lwt.return_unit
  else if node == !(s.last) then
    feed s >>= fun () -> njunk_rec node n s
  else
    match node.data with
    | Some _ ->
      consume s node;
      njunk_rec node.next (n - 1) s
    | None ->
      Lwt.return_unit

let njunk n s = njunk_rec s.node n s

let rec junk_while_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> junk_while_rec node f s
  else
    match node.data with
    | Some x ->
      let test = f x in
      if test then begin
        consume s node;
        junk_while_rec node.next f s
      end else
        Lwt.return_unit
    | None ->
      Lwt.return_unit

let junk_while f s = junk_while_rec s.node f s

let rec junk_while_s_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> junk_while_s_rec node f s
  else
    match node.data with
    | Some x -> begin
        f x >>= function
        | true ->
          consume s node;
          junk_while_s_rec node.next f s
        | false ->
          Lwt.return_unit
      end
    | None ->
      Lwt.return_unit

let junk_while_s f s = junk_while_s_rec s.node f s

let rec junk_old_rec node s =
  if node == !(s.last) then
    let thread = feed s in
    match Lwt.state thread with
    | Lwt.Return _ ->
      junk_old_rec node s
    | Lwt.Fail exn ->
      Lwt.fail exn
    | Lwt.Sleep ->
      Lwt.return_unit
  else
    match node.data with
    | Some _ ->
      consume s node;
      junk_old_rec node.next s
    | None ->
      Lwt.return_unit

let junk_old s = junk_old_rec s.node s

let rec get_available_rec node acc s =
  if node == !(s.last) then
    let thread = feed s in
    match Lwt.state thread with
    | Lwt.Return _ ->
      get_available_rec node acc s
    | Lwt.Fail exn ->
      raise exn
    | Lwt.Sleep ->
      List.rev acc
  else
    match node.data with
    | Some x ->
      consume s node;
      get_available_rec node.next (x :: acc) s
    | None ->
      List.rev acc

let get_available s = get_available_rec s.node [] s

let rec get_available_up_to_rec node acc n s =
  if n <= 0 then
    List.rev acc
  else if node == !(s.last) then
    let thread = feed s in
    match Lwt.state thread with
    | Lwt.Return _ ->
      get_available_up_to_rec node acc n s
    | Lwt.Fail exn ->
      raise exn
    | Lwt.Sleep ->
      List.rev acc
  else
    match s.node.data with
    | Some x ->
      consume s node;
      get_available_up_to_rec node.next (x :: acc) (n - 1) s
    | None ->
      List.rev acc

let get_available_up_to n s = get_available_up_to_rec s.node [] n s

let rec is_empty s =
  if s.node == !(s.last) then
    feed s >>= fun () -> is_empty s
  else
    Lwt.return (s.node.data = None)

let map f s =
  from (fun () -> get s >|= function
  | Some x ->
    let x = f x in
    Some x
  | None ->
    None)

let map_s f s =
  from (fun () -> get s >>= function
  | Some x ->
    f x >|= (fun x -> Some x)
  | None ->
    Lwt.return_none)

let filter f s =
  let rec next () =
    let t = get s in
    t >>= function
    | Some x ->
      let test = f x in
      if test then
        t
      else
        next ()
    | None ->
      Lwt.return_none
  in
  from next

let filter_s f s =
  let rec next () =
    let t = get s in
    t >>= function
    | Some x -> begin
        f x >>= function
        | true ->
          t
        | false ->
          next ()
      end
    | None ->
      t
  in
  from next

let filter_map f s =
  let rec next () =
    get s >>= function
    | Some x ->
      let x = f x in
      (match x with
       | Some _ ->
         Lwt.return x
       | None ->
         next ())
    | None ->
      Lwt.return_none
  in
  from next

let filter_map_s f s =
  let rec next () =
    get s >>= function
    | Some x ->
      let t = f x in
      (t >>= function
       | Some _ ->
         t
       | None ->
         next ())
    | None ->
      Lwt.return_none
  in
  from next

let map_list f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
    | [] ->
      (get s >>= function
       | Some x ->
         let l = f x in
         pendings := l;
         next ()
       | None ->
         Lwt.return_none)
    | x :: l ->
      pendings := l;
      Lwt.return (Some x)
  in
  from next

let map_list_s f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
    | [] ->
      (get s >>= function
       | Some x ->
         f x >>= fun l ->
         pendings := l;
         next ()
       | None ->
         Lwt.return_none)
    | x :: l ->
      pendings := l;
      Lwt.return (Some x)
  in
  from next

let flatten s =
  map_list (fun l -> l) s

let rec fold_rec node f s acc =
  if node == !(s.last) then
    feed s >>= fun () -> fold_rec node f s acc
  else
    match node.data with
    | Some x ->
      consume s node;
      let acc = f x acc in
      fold_rec node.next f s acc
    | None ->
      Lwt.return acc

let fold f s acc = fold_rec s.node f s acc

let rec fold_s_rec node f s acc =
  if node == !(s.last) then
    feed s >>= fun () -> fold_s_rec node f s acc
  else
    match node.data with
    | Some x ->
      consume s node;
      f x acc >>= fun acc ->
      fold_s_rec node.next f s acc
    | None ->
      Lwt.return acc

let fold_s f s acc = fold_s_rec s.node f s acc

let rec iter_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> iter_rec node f s
  else
    match node.data with
    | Some x ->
      consume s node;
      let () = f x in
      iter_rec node.next f s
    | None ->
      Lwt.return_unit

let iter f s = iter_rec s.node f s

let rec iter_s_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> iter_s_rec node f s
  else
    match node.data with
    | Some x ->
      consume s node;
      f x >>= fun () ->
      iter_s_rec node.next f s
    | None ->
      Lwt.return_unit

let iter_s f s = iter_s_rec s.node f s

let rec iter_p_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> iter_p_rec node f s
  else
    match node.data with
    | Some x ->
      consume s node;
      let res = f x in
      let rest = iter_p_rec node.next f s in
      res >>= fun () -> rest
    | None ->
      Lwt.return_unit

let iter_p f s = iter_p_rec s.node f s

let iter_n ?(max_concurrency = 1) f stream =
  begin
    if max_concurrency <= 0 then
      let message =
        Printf.sprintf
          "Lwt_stream.iter_n: max_concurrency must be > 0, %d given"
          max_concurrency
      in
      invalid_arg message
  end;
  let rec loop running available =
    begin
      if available > 0 then (
        Lwt.return (running, available)
      )
      else (
        Lwt.nchoose_split running >>= fun (complete, running) ->
        Lwt.return (running, available + List.length complete)
      )
    end >>= fun (running, available) ->
    get stream >>= function
    | None ->
      Lwt.join running
    | Some elt ->
      loop (f elt :: running) (pred available)
  in
  loop [] max_concurrency

let rec find_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> find_rec node f s
  else
    match node.data with
    | Some x as opt ->
      consume s node;
      let test = f x in
      if test then
        Lwt.return opt
      else
        find_rec node.next f s
    | None ->
      Lwt.return_none

let find f s = find_rec s.node f s

let rec find_s_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> find_s_rec node f s
  else
    match node.data with
    | Some x as opt -> begin
        consume s node;
        f x >>= function
        | true ->
          Lwt.return opt
        | false ->
          find_s_rec node.next f s
      end
    | None ->
      Lwt.return_none

let find_s f s = find_s_rec s.node f s

let rec find_map_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> find_map_rec node f s
  else
    match node.data with
    | Some x ->
      consume s node;
      let x = f x in
      if x = None then
        find_map_rec node.next f s
      else
        Lwt.return x
    | None ->
      Lwt.return_none

let find_map f s = find_map_rec s.node f s

let rec find_map_s_rec node f s =
  if node == !(s.last) then
    feed s >>= fun () -> find_map_s_rec node f s
  else
    match node.data with
    | Some x ->
      consume s node;
      let t = f x in
      (t >>= function
       | None ->
         find_map_s_rec node.next f s
       | Some _ ->
         t)
    | None ->
      Lwt.return_none

let find_map_s f s = find_map_s_rec s.node f s

let combine s1 s2 =
  let next () =
    let t1 = get s1 and t2 = get s2 in
    t1 >>= fun n1 ->
    t2 >>= fun n2 ->
    match n1, n2 with
    | Some x1, Some x2 ->
      Lwt.return (Some(x1, x2))
    | _ ->
      Lwt.return_none
  in
  from next

let append s1 s2 =
  let current_s = ref s1 in
  let rec next () =
    let t = get !current_s in
    t >>= function
    | Some _ ->
      t
    | None ->
      if !current_s == s2 then
        Lwt.return_none
      else begin
        current_s := s2;
        next ()
      end
  in
  from next

let concat s_top =
  let current_s = ref (from (fun () -> Lwt.return_none)) in
  let rec next () =
    let t = get !current_s in
    t >>= function
    | Some _ ->
      t
    | None ->
      get s_top >>= function
      | Some s ->
        current_s := s;
        next ()
      | None ->
        Lwt.return_none
  in
  from next

let choose streams =
  let source s = (s, get s >|= fun x -> (s, x)) in
  let streams = ref (List.map source streams) in
  let rec next () =
    match !streams with
    | [] ->
      Lwt.return_none
    | l ->
      Lwt.choose (List.map snd l) >>= fun (s, x) ->
      let l = List.remove_assq s l in
      match x with
      | Some _ ->
        streams := source s :: l;
        Lwt.return x
      | None ->
        streams := l;
        next ()
  in
  from next

let parse s f =
  (match s.source with
   | Push_bounded _ -> invalid_arg "Lwt_stream.parse"
   | From _ | From_direct _ | Push _ -> ());
  let node = s.node in
  Lwt.catch
    (fun () -> f s)
    (fun exn ->
       s.node <- node;
       Lwt.fail exn)

let hexdump stream =
  let buf = Buffer.create 80 and num = ref 0 in
  from begin fun _ ->
    nget 16 stream >>= function
    | [] ->
      Lwt.return_none
    | l ->
      Buffer.clear buf;
      Printf.bprintf buf "%08x|  " !num;
      num := !num + 16;
      let rec bytes pos = function
        | [] ->
          blanks pos
        | x :: l ->
          if pos = 8 then Buffer.add_char buf ' ';
          Printf.bprintf buf "%02x " (Char.code x);
          bytes (pos + 1) l
      and blanks pos =
        if pos < 16 then begin
          if pos = 8 then
            Buffer.add_string buf "    "
          else
            Buffer.add_string buf "   ";
          blanks (pos + 1)
        end
      in
      bytes 0 l;
      Buffer.add_string buf " |";
      List.iter (fun ch -> Buffer.add_char buf (if ch >= '\x20' && ch <= '\x7e' then ch else '.')) l;
      Buffer.add_char buf '|';
      Lwt.return (Some(Buffer.contents buf))
  end
