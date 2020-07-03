(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



type t =
  { mutable delay : int; action : unit -> unit;
    mutable prev : t; mutable next : t }

let make delay action =
  let rec x = { delay = delay; action = action; prev = x; next = x } in
  x

let lst_empty () = make (-1) (fun () -> ())

let lst_remove x =
  let p = x.prev in
  let n = x.next in
  p.next <- n;
  n.prev <- p;
  x.next <- x;
  x.prev <- x

let lst_insert p x =
  let n = p.next in
  p.next <- x;
  x.prev <- p;
  x.next <- n;
  n.prev <- x

let lst_in_list x = x.next != x

let lst_is_empty set = set.next == set

let lst_peek s = let x = s.next in lst_remove x; x

(****)

let count = ref 0

let buckets = ref [||]

let curr = ref 0

let stopped = ref true

let size l =
  let len = Array.length !buckets in
  if l >= len then begin
    let b = Array.init (l + 1) (fun _ -> lst_empty ()) in
    Array.blit !buckets !curr b 0 (len - !curr);
    Array.blit !buckets 0 b (len - !curr) !curr;
    buckets := b; curr := 0;
  end

(****)

let handle_exn =
  ref
    (fun exn ->
      !Lwt.async_exception_hook exn)

let set_exn_handler f = handle_exn := f

let rec loop () =
  stopped := false;
  Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
    let s = !buckets.(!curr) in
    while not (lst_is_empty s) do
      let x = lst_peek s in
      decr count;
      (*XXX Should probably report any exception *)
      try
        x.action ()
      with e -> !handle_exn e
    done;
    curr := (!curr + 1) mod (Array.length !buckets);
    if !count > 0 then loop () else begin stopped := true; Lwt.return_unit end)

let start x =
  let in_list = lst_in_list x in
  let slot = (!curr + x.delay) mod (Array.length !buckets) in
  lst_remove x;
  lst_insert !buckets.(slot) x;
  if not in_list then begin
    incr count;
    if !count = 1 && !stopped then ignore (loop ())
  end

let create delay action =
  if delay < 1 then invalid_arg "Lwt_timeout.create";
  let x = make delay action in
  size delay;
  x

let stop x =
  if lst_in_list x then begin
    lst_remove x;
    decr count
  end

let change x delay =
  if delay < 1 then invalid_arg "Lwt_timeout.change";
  x.delay <- delay;
  size delay;
  if lst_in_list x then start x
