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

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

type _event = {
  stop : unit Lazy.t;
  (* The stop method of the event. *)
  node : Obj.t Lwt_sequence.node;
  (* The node in the sequence of registered events. *)
}

type event = _event ref

external cast_node : 'a Lwt_sequence.node -> Obj.t Lwt_sequence.node = "%identity"

let stop_event ev =
  let ev = !ev in
  Lwt_sequence.remove ev.node;
  Lazy.force ev.stop

let _fake_event = {
  stop = lazy ();
  node = Lwt_sequence.add_l (Obj.repr ()) (Lwt_sequence.create ());
}

let fake_event = ref _fake_event

(* +-----------------------------------------------------------------+
   | Engines                                                         |
   +-----------------------------------------------------------------+ *)


(* +-----------------------------------------------------------------+
   | The libev engine                                                |
   +-----------------------------------------------------------------+ *)

type ev_loop
type ev_io
type ev_timer

module Ev_backend =
struct
  type t =
    | EV_DEFAULT
    | EV_SELECT
    | EV_POLL
    | EV_EPOLL
    | EV_KQUEUE
    | EV_DEVPOLL
    | EV_PORT

  let default = EV_DEFAULT
  let select = EV_SELECT
  let poll = EV_POLL
  let epoll = EV_EPOLL
  let kqueue = EV_KQUEUE
  let devpoll = EV_DEVPOLL
  let port = EV_PORT

  let name = function
    | EV_DEFAULT -> "EV_DEFAULT"
    | EV_SELECT -> "EV_SELECT"
    | EV_POLL -> "EV_POLL"
    | EV_EPOLL -> "EV_EPOLL"
    | EV_KQUEUE -> "EV_KQUEUE"
    | EV_DEVPOLL -> "EV_DEVPOLL"
    | EV_PORT -> "EV_PORT"

  let pp fmt t = Format.pp_print_string fmt (name t)
end

external ev_init : Ev_backend.t -> ev_loop = "lwt_libev_init"
external ev_stop : ev_loop -> unit = "lwt_libev_stop"
external ev_loop : ev_loop -> bool -> unit = "lwt_libev_loop"
external ev_unloop : ev_loop -> unit = "lwt_libev_unloop"
external ev_readable_init : ev_loop -> Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_readable_init"
external ev_writable_init : ev_loop -> Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_writable_init"
external ev_io_stop : ev_loop -> ev_io -> unit = "lwt_libev_io_stop"
external ev_timer_init : ev_loop -> float -> bool -> (unit -> unit) -> ev_timer = "lwt_libev_timer_init"
external ev_timer_stop : ev_loop -> ev_timer -> unit  = "lwt_libev_timer_stop"

(* +-----------------------------------------------------------------+
   | Select/poll based engines                                       |
   +-----------------------------------------------------------------+ *)

(* Type of a sleeper for the select engine. *)
type sleeper = {
  mutable time : float;
  (* The time at which the sleeper should be wakeup. *)

  mutable stopped : bool;
  (* [true] iff the event has been stopped. *)

  action : unit -> unit;
  (* The action for the sleeper. *)
}

module Sleep_queue =
  Lwt_pqueue.Make(struct
    type t = sleeper
    let compare {time = t1; _} {time = t2; _} = compare t1 t2
  end)
  [@@ocaml.warning "-3"]

module Fd_map = Map.Make(struct type t = Unix.file_descr let compare = compare end)

let rec restart_actions sleep_queue now =
  match Sleep_queue.lookup_min sleep_queue with
  | Some{ stopped = true; _ } ->
    restart_actions (Sleep_queue.remove_min sleep_queue) now
  | Some{ time = time; action = action; _ } when time <= now ->
    (* We have to remove the sleeper to the queue before performing
       the action. The action can change the sleeper's time, and this
       might break the priority queue invariant if the sleeper is
       still in the queue. *)
    let q = Sleep_queue.remove_min sleep_queue in
    action ();
    restart_actions q now
  | _ ->
    sleep_queue

let rec get_next_timeout sleep_queue =
  match Sleep_queue.lookup_min sleep_queue with
  | Some{ stopped = true; _ } ->
    get_next_timeout (Sleep_queue.remove_min sleep_queue)
  | Some{ time = time; _ } ->
    max 0. (time -. Unix.gettimeofday ())
  | None ->
    -1.

let bad_fd fd =
  try
    let _ = Unix.fstat fd in
    false
  with Unix.Unix_error (_, _, _) ->
    true

let invoke_actions fd map =
  match try Some(Fd_map.find fd map) with Not_found -> None with
  | Some actions -> Lwt_sequence.iter_l (fun f -> f ()) actions
  | None -> ()

(* +-----------------------------------------------------------------+
   | The current engine                                              |
   +-----------------------------------------------------------------+ *)

let current =
  if Lwt_config._HAVE_LIBEV && Lwt_config.libev_default then
    ref (libev () :> t)
  else
    ref (select :> t)

let get () =
  !current

let set ?(transfer=true) ?(destroy=true) engine =
  if transfer then !current##transfer (engine : t);
  if destroy then !current##destroy;
  current := (engine : t)

let iter block = !current##iter block
let on_readable fd f = !current##on_readable fd f
let on_writable fd f = !current##on_writable fd f
let on_timer delay repeat f = !current##on_timer delay repeat f
let fake_io fd = !current##fake_io fd
let readable_count () = !current##readable_count
let writable_count () = !current##writable_count
let timer_count () = !current##timer_count
