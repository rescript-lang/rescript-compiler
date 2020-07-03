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

exception Channel_closed of string

(* Minimum size for buffers: *)
let min_buffer_size = 16

let check_buffer_size fun_name buffer_size =
  if buffer_size < min_buffer_size then
    Printf.ksprintf invalid_arg "Lwt_io.%s: too small buffer size" fun_name
  else if buffer_size > Sys.max_string_length then
    Printf.ksprintf invalid_arg "Lwt_io.%s: too big buffer size" fun_name
  else
    ()

let check_buffer fun_name buffer =
  check_buffer_size fun_name (Lwt_bytes.length buffer)

let default_buffer_size = ref 4096

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type input
type output

type 'a mode =
  | Input : input mode
  | Output : output mode

let input : input mode = Input
let output : output mode = Output

(* A channel state *)
type 'mode state =
  | Busy_primitive
  (* A primitive is running on the channel *)

  | Busy_atomic of 'mode channel
  (* An atomic operations is being performed on the channel. The
     argument is the temporary atomic wrapper. *)

  | Waiting_for_busy
  (* A queued operation has not yet started. *)

  | Idle
  (* The channel is unused *)

  | Closed
  (* The channel has been closed *)

  | Invalid
  (* The channel is a temporary channel created for an atomic
     operation which has terminated. *)

(* A wrapper, which ensures that io operations are atomic: *)
and 'mode channel = {
  mutable state : 'mode state;

  channel : 'mode _channel;
  (* The real channel *)

  mutable queued : unit Lwt.u Lwt_sequence.t;
  (* Queued operations *)
}

and 'mode _channel = {
  mutable buffer : Lwt_bytes.t;
  mutable length : int;

  mutable ptr : int;
  (* Current position *)

  mutable max : int;
  (* Position of the end of data int the buffer. It is equal to
     [length] for output channels. *)

  abort_waiter : int Lwt.t;
  (* Thread which is wakeup with an exception when the channel is
     closed. *)
  abort_wakener : int Lwt.u;

  mutable auto_flushing : bool;
  (* Whether the auto-flusher is currently running or not *)

  main : 'mode channel;
  (* The main wrapper *)

  close : unit Lwt.t Lazy.t;
  (* Close function *)

  mode : 'mode mode;
  (* The channel mode *)

  mutable offset : int64;
  (* Number of bytes really read/written *)

  typ : typ;
  (* Type of the channel. *)
}

and typ =
  | Type_normal of
    (Lwt_bytes.t -> int -> int -> int Lwt.t) *
      (int64 -> Unix.seek_command -> int64 Lwt.t)
  (* The channel has been created with [make]. The first argument
     is the refill/flush function and the second is the seek
     function. *)
  | Type_bytes
  (* The channel has been created with [of_bytes]. *)

type input_channel = input channel
type output_channel = output channel

type direct_access = {
  da_buffer : Lwt_bytes.t;
  mutable da_ptr : int;
  mutable da_max : int;
  da_perform : unit -> int Lwt.t;
}

let mode wrapper = wrapper.channel.mode

(* +-----------------------------------------------------------------+
   | Creations, closing, locking, ...                                |
   +-----------------------------------------------------------------+ *)

(* This strange hash function is fine because Lwt_io only ever:
    - adds distinct channels to the hash set,
    - folds over the hash set.
   Lwt_io never looks up individual elements. The constant function is not
   suitable, because then all channels will end up in the same hash bucket.

   A weak hash set is used instead of a weak array to avoid having to include
   resizing and compaction code in Lwt_io. *)
let hash_output_channel =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    !index

module Outputs = Weak.Make(struct
    type t = output_channel
    let hash _ = hash_output_channel ()
    let equal = ( == )
  end)

(* Table of all opened output channels. On exit they are all
   flushed: *)
let outputs = Outputs.create 32

let position : type mode. mode channel -> int64 = fun wrapper ->
  let ch = wrapper.channel in
  match ch.mode with
  | Input ->
    Int64.sub ch.offset (Int64.of_int (ch.max - ch.ptr))
  | Output ->
    Int64.add ch.offset (Int64.of_int ch.ptr)

let name : type mode. mode _channel -> string = fun ch ->
  match ch.mode with
  | Input -> "input"
  | Output -> "output"

let closed_channel ch = Channel_closed (name ch)
let invalid_channel ch =
  Failure (Printf.sprintf "temporary atomic channel %s no more valid" (name ch))

let is_busy ch =
  match ch.state with
  | Invalid ->
    raise (invalid_channel ch.channel)
  | Idle | Closed ->
    false
  | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
    true

(* Flush/refill the buffer. No race condition could happen because
   this function is always called atomically: *)
let perform_io : type mode. mode _channel -> int Lwt.t = fun ch ->
  match ch.main.state with
  | Closed ->
    Lwt.fail (closed_channel ch)

  | Invalid ->
    Lwt.fail (invalid_channel ch)

  | Idle
  | Waiting_for_busy ->
    assert false

  | Busy_primitive
  | Busy_atomic _ ->
    match ch.typ with
    | Type_normal (perform, _) ->
      let ptr, len =
        match ch.mode with
        | Input ->
          (* Size of data in the buffer *)
          let size = ch.max - ch.ptr in
          (* If there are still data in the buffer, keep them: *)
          if size > 0 then
            Lwt_bytes.unsafe_blit ch.buffer ch.ptr ch.buffer 0 size;
          (* Update positions: *)
          ch.ptr <- 0;
          ch.max <- size;
          (size, ch.length - size)
        | Output ->
          (0, ch.ptr)
      in
      let perform =
        if Sys.win32 then
          Lwt.catch
            (fun () ->
              perform ch.buffer ptr len)
            (function
              | Unix.Unix_error (Unix.EPIPE, _, _) ->
                Lwt.return 0
              | exn -> Lwt.fail exn) [@ocaml.warning "-4"]
        else
          perform ch.buffer ptr len
      in
      Lwt.pick [ch.abort_waiter; perform] >>= fun n ->
      (* Never trust user functions... *)
      if n < 0 || n > len then
        Lwt.fail
          (Failure
            (Printf.sprintf
              "Lwt_io.perform_io: invalid result of the [%s] function"
              (match ch.mode with Input -> "read" | Output -> "write")))
      else begin
        (* Update the global offset: *)
        ch.offset <- Int64.add ch.offset (Int64.of_int n);
        (* Update buffer positions: *)
        begin match ch.mode with
          | Input ->
            ch.max <- ch.max + n
          | Output ->
            (* Shift remaining data: *)
            let len = len - n in
            Lwt_bytes.unsafe_blit ch.buffer n ch.buffer 0 len;
            ch.ptr <- len
        end;
        Lwt.return n
      end

    | Type_bytes ->
      begin match ch.mode with
      | Input ->
        Lwt.return 0
      | Output ->
        Lwt.fail
          (Failure "cannot flush a channel created with Lwt_io.of_string")
      end

let refill = perform_io
let flush_partial = perform_io

let rec flush_total oc =
  if oc.ptr > 0 then
    flush_partial oc >>= fun _ ->
    flush_total oc
  else
    Lwt.return_unit

let safe_flush_total oc =
  Lwt.catch
    (fun () -> flush_total oc)
    (fun _  -> Lwt.return_unit)

let deepest_wrapper ch =
  let rec loop wrapper =
    match wrapper.state with
    | Busy_atomic wrapper ->
      loop wrapper
    | Busy_primitive | Waiting_for_busy | Idle | Closed | Invalid ->
      wrapper
  in
  loop ch.main

let auto_flush oc =
  Lwt.pause () >>= fun () ->
  let wrapper = deepest_wrapper oc in
  match wrapper.state with
  | Busy_primitive | Waiting_for_busy ->
    (* The channel is used, cancel auto flushing. It will be
       restarted when the channel Lwt.returns to the [Idle] state: *)
    oc.auto_flushing <- false;
    Lwt.return_unit

  | Busy_atomic _ ->
    (* Cannot happen since we took the deepest wrapper: *)
    assert false

  | Idle ->
    oc.auto_flushing <- false;
    wrapper.state <- Busy_primitive;
    safe_flush_total oc >>= fun () ->
    if wrapper.state = Busy_primitive then
      wrapper.state <- Idle;
    if not (Lwt_sequence.is_empty wrapper.queued) then
      Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ();
    Lwt.return_unit

  | Closed | Invalid ->
    Lwt.return_unit

(* A ``locked'' channel is a channel in the state [Busy_primitive] or
   [Busy_atomic] *)

let unlock : type m. m channel -> unit = fun wrapper -> match wrapper.state with
  | Busy_primitive | Busy_atomic _ ->
    if Lwt_sequence.is_empty wrapper.queued then
      wrapper.state <- Idle
    else begin
      wrapper.state <- Waiting_for_busy;
      Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ()
    end;
    (* Launches the auto-flusher: *)
    let ch = wrapper.channel in
    if (* Launch the auto-flusher only if the channel is not busy: *)
      (wrapper.state = Idle &&
       (* Launch the auto-flusher only for output channel: *)
       (match ch.mode with Input -> false | Output -> true) &&
       (* Do not launch two auto-flusher: *)
       not ch.auto_flushing &&
       (* Do not launch the auto-flusher if operations are queued: *)
       Lwt_sequence.is_empty wrapper.queued) then begin
      ch.auto_flushing <- true;
      ignore (auto_flush ch)
    end

  | Closed | Invalid ->
    (* Do not change channel state if the channel has been closed *)
    if not (Lwt_sequence.is_empty wrapper.queued) then
      Lwt.wakeup_later (Lwt_sequence.take_l wrapper.queued) ()

  | Idle | Waiting_for_busy ->
    (* We must never unlock an unlocked channel *)
    assert false

(* Wrap primitives into atomic io operations: *)
let primitive f wrapper = match wrapper.state with
  | Idle ->
    wrapper.state <- Busy_primitive;
    Lwt.finalize
      (fun () -> f wrapper.channel)
      (fun () ->
         unlock wrapper;
         Lwt.return_unit)

  | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
    (Lwt.add_task_r [@ocaml.warning "-3"]) wrapper.queued >>= fun () ->
    begin match wrapper.state with
      | Closed ->
        (* The channel has been closed while we were waiting *)
        unlock wrapper;
        Lwt.fail (closed_channel wrapper.channel)

      | Idle | Waiting_for_busy ->
        wrapper.state <- Busy_primitive;
        Lwt.finalize
          (fun () -> f wrapper.channel)
          (fun () ->
             unlock wrapper;
             Lwt.return_unit)

      | Invalid ->
        Lwt.fail (invalid_channel wrapper.channel)

      | Busy_primitive | Busy_atomic _ ->
        assert false
    end

  | Closed ->
    Lwt.fail (closed_channel wrapper.channel)

  | Invalid ->
    Lwt.fail (invalid_channel wrapper.channel)

(* Wrap a sequence of io operations into an atomic operation: *)
let atomic f wrapper = match wrapper.state with
  | Idle ->
    let tmp_wrapper = { state = Idle;
                        channel = wrapper.channel;
                        queued = Lwt_sequence.create () } in
    wrapper.state <- Busy_atomic tmp_wrapper;
    Lwt.finalize
      (fun () -> f tmp_wrapper)
      (fun () ->
         (* The temporary wrapper is no more valid: *)
         tmp_wrapper.state <- Invalid;
         unlock wrapper;
         Lwt.return_unit)

  | Busy_primitive | Busy_atomic _ | Waiting_for_busy ->
    (Lwt.add_task_r [@ocaml.warning "-3"]) wrapper.queued >>= fun () ->
    begin match wrapper.state with
      | Closed ->
        (* The channel has been closed while we were waiting *)
        unlock wrapper;
        Lwt.fail (closed_channel wrapper.channel)

      | Idle | Waiting_for_busy ->
        let tmp_wrapper = { state = Idle;
                            channel = wrapper.channel;
                            queued = Lwt_sequence.create () } in
        wrapper.state <- Busy_atomic tmp_wrapper;
        Lwt.finalize
          (fun () -> f tmp_wrapper)
          (fun () ->
             tmp_wrapper.state <- Invalid;
             unlock wrapper;
             Lwt.return_unit)

      | Invalid ->
        Lwt.fail (invalid_channel wrapper.channel)

      | Busy_primitive | Busy_atomic _ ->
        assert false
    end

  | Closed ->
    Lwt.fail (closed_channel wrapper.channel)

  | Invalid ->
    Lwt.fail (invalid_channel wrapper.channel)

let rec abort wrapper = match wrapper.state with
  | Busy_atomic tmp_wrapper ->
    (* Close the depest opened wrapper: *)
    abort tmp_wrapper
  | Closed ->
    (* Double close, just returns the same thing as before *)
    Lazy.force wrapper.channel.close
  | Invalid ->
    Lwt.fail (invalid_channel wrapper.channel)
  | Idle | Busy_primitive | Waiting_for_busy ->
    wrapper.state <- Closed;
    (* Abort any current real reading/writing operation on the
       channel: *)
    Lwt.wakeup_exn
      wrapper.channel.abort_wakener (closed_channel wrapper.channel);
    Lazy.force wrapper.channel.close

let close : type mode. mode channel -> unit Lwt.t = fun wrapper ->
  let channel = wrapper.channel in
  if channel.main != wrapper then
    Lwt.fail
      (Failure
        "Lwt_io.close: cannot close a channel obtained via Lwt_io.atomic")
  else
    match channel.mode with
    | Input ->
      (* Just close it now: *)
      abort wrapper
    | Output ->
      Lwt.catch
        (fun () ->
           (* Performs all pending actions, flush the buffer, then close it: *)
           primitive (fun channel ->
             safe_flush_total channel >>= fun () -> abort wrapper) wrapper)
        (fun _ ->
           abort wrapper)

let is_closed wrapper =
  match wrapper.state with
  | Closed -> true
  | Busy_primitive | Busy_atomic _ | Waiting_for_busy | Idle | Invalid -> false

let flush_all () =
  let wrappers = Outputs.fold (fun x l -> x :: l) outputs [] in
  Lwt_list.iter_p
    (fun wrapper ->
       Lwt.catch
         (fun () -> primitive safe_flush_total wrapper)
         (fun _  -> Lwt.return_unit))
    wrappers

let () =
  (* Flush all opened output channels on exit: *)
  Lwt_main.at_exit flush_all

let no_seek _pos _cmd =
  Lwt.fail (Failure "Lwt_io.seek: seek not supported on this channel")

let make :
    type m.
    ?buffer : Lwt_bytes.t ->
    ?close : (unit -> unit Lwt.t) ->
    ?seek : (int64 -> Unix.seek_command -> int64 Lwt.t) ->
    mode : m mode ->
    (Lwt_bytes.t -> int -> int -> int Lwt.t) ->
      m channel =
    fun ?buffer ?(close=Lwt.return) ?(seek=no_seek) ~mode perform_io ->
  let (buffer, size) =
    match buffer with
    | Some buffer ->
      check_buffer "Lwt_io.make" buffer;
      (buffer, Lwt_bytes.length buffer)
    | None ->
      let size = !default_buffer_size in
      (Lwt_bytes.create size, size)
  in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec ch = {
    buffer = buffer;
    length = size;
    ptr = 0;
    max = (match mode with
      | Input -> 0
      | Output -> size);
    close = lazy(Lwt.catch close Lwt.fail);
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    main = wrapper;
    auto_flushing = false;
    mode = mode;
    offset = 0L;
    typ =
      Type_normal
        (perform_io, fun pos cmd -> try seek pos cmd with e -> Lwt.fail e);
  } and wrapper = {
    state = Idle;
    channel = ch;
    queued = Lwt_sequence.create ();
  } in
  (match mode with
   | Input -> ()
   | Output -> Outputs.add outputs wrapper);
  wrapper

let of_bytes (type m) ~(mode : m mode) bytes =
  let length = Lwt_bytes.length bytes in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec ch = {
    buffer = bytes;
    length = length;
    ptr = 0;
    max = length;
    close = lazy(Lwt.return_unit);
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    main = wrapper;
    (* Auto flush is set to [true] to prevent writing functions from
       trying to launch the auto-fllushed. *)
    auto_flushing = true;
    mode = mode;
    offset = (match mode with
        | Output -> 0L
        | Input -> Int64.of_int length);
    typ = Type_bytes;
  } and wrapper = {
    state = Idle;
    channel = ch;
    queued = Lwt_sequence.create ();
  } in
  wrapper

let of_fd :
    type m.
    ?buffer : Lwt_bytes.t ->
    ?close : (unit -> unit Lwt.t) ->
    mode : m mode ->
    Lwt_unix.file_descr ->
      m channel =
    fun ?buffer ?close ~mode fd ->
  let perform_io = match mode with
    | Input -> Lwt_bytes.read fd
    | Output -> Lwt_bytes.write fd
  in
  make
    ?buffer
    ~close:(match close with
      | Some f -> f
      | None -> (fun () -> Lwt_unix.close fd))
    ~seek:(fun pos cmd -> Lwt_unix.LargeFile.lseek fd pos cmd)
    ~mode
    perform_io

let of_unix_fd :
    type m.
    ?buffer : Lwt_bytes.t ->
    ?close : (unit -> unit Lwt.t) ->
    mode : m mode -> Unix.file_descr ->
      m channel =
    fun ?buffer ?close ~mode fd ->
  of_fd ?buffer ?close ~mode (Lwt_unix.of_unix_file_descr fd)

let buffered : type m. m channel -> int = fun ch ->
  match ch.channel.mode with
  | Input -> ch.channel.max - ch.channel.ptr
  | Output -> ch.channel.ptr

let buffer_size ch = ch.channel.length

let resize_buffer : type m. m channel -> int -> unit Lwt.t = fun wrapper len ->
  if len < min_buffer_size then
    invalid_arg "Lwt_io.resize_buffer: buffer size too small";
  match wrapper.channel.typ with
  | Type_bytes ->
    Lwt.fail
      (Failure
        ("Lwt_io.resize_buffer: cannot resize the buffer of a channel " ^
         "created with Lwt_io.of_string"))
  | Type_normal _ ->
    let f : type m. m _channel -> unit Lwt.t = fun ch ->
      match ch.mode with
      | Input ->
        let unread_count = ch.max - ch.ptr in
        (* Fail if we want to decrease the buffer size and there is
           too much unread data in the buffer: *)
        if len < unread_count then
          Lwt.fail
            (Failure
              ("Lwt_io.resize_buffer: cannot decrease buffer size, too much " ^
               "unread data"))
        else begin
          let buffer = Lwt_bytes.create len in
          Lwt_bytes.unsafe_blit ch.buffer ch.ptr buffer 0 unread_count;
          ch.buffer <- buffer;
          ch.length <- len;
          ch.ptr <- 0;
          ch.max <- unread_count;
          Lwt.return_unit
        end
      | Output ->
        (* If we decrease the buffer size, flush the buffer until
           the number of buffered bytes fits into the new buffer: *)
        let rec loop () =
          if ch.ptr > len then
            flush_partial ch >>= fun _ ->
            loop ()
          else
            Lwt.return_unit
        in
        loop () >>= fun () ->
        let buffer = Lwt_bytes.create len in
        Lwt_bytes.unsafe_blit ch.buffer 0 buffer 0 ch.ptr;
        ch.buffer <- buffer;
        ch.length <- len;
        ch.max <- len;
        Lwt.return_unit
    in
    primitive f wrapper

module Primitives =
struct

  (* This module contains all primitives operations. The operates
     without protection regarding locking, they are wrapped after into
     safe operations. *)

  (* +---------------------------------------------------------------+
     | Reading                                                       |
     +---------------------------------------------------------------+ *)

  let rec read_char ic =
    let ptr = ic.ptr in
    if ptr = ic.max then
      refill ic >>= function
      | 0 -> Lwt.fail End_of_file
      | _ -> read_char ic
    else begin
      ic.ptr <- ptr + 1;
      Lwt.return (Lwt_bytes.unsafe_get ic.buffer ptr)
    end

  let read_char_opt ic =
    Lwt.catch
      (fun () -> read_char ic >|= fun ch -> Some ch)
      (function
        | End_of_file -> Lwt.return_none
        | exn -> Lwt.fail exn)

  let read_line ic =
    let buf = Buffer.create 128 in
    let rec loop cr_read =
      Lwt.try_bind (fun _ -> read_char ic)
        (function
          | '\n' ->
            Lwt.return(Buffer.contents buf)
          | '\r' ->
            if cr_read then Buffer.add_char buf '\r';
            loop true
          | ch ->
            if cr_read then Buffer.add_char buf '\r';
            Buffer.add_char buf ch;
            loop false)
        (function
          | End_of_file ->
            if cr_read then Buffer.add_char buf '\r';
            Lwt.return(Buffer.contents buf)
          | exn ->
            Lwt.fail exn)
    in
    read_char ic >>= function
    | '\r' -> loop true
    | '\n' -> Lwt.return ""
    | ch -> Buffer.add_char buf ch; loop false

  let read_line_opt ic =
    Lwt.catch
      (fun () -> read_line ic >|= fun ch -> Some ch)
      (function
        | End_of_file -> Lwt.return_none
        | exn -> Lwt.fail exn)

  let unsafe_read_into ic buf ofs len =
    let avail = ic.max - ic.ptr in
    if avail > 0 then begin
      let len = min len avail in
      Lwt_bytes.unsafe_blit_to_bytes ic.buffer ic.ptr buf ofs len;
      ic.ptr <- ic.ptr + len;
      Lwt.return len
    end else begin
      refill ic >>= fun n ->
      let len = min len n in
      Lwt_bytes.unsafe_blit_to_bytes ic.buffer 0 buf ofs len;
      ic.ptr <- len;
      ic.max <- n;
      Lwt.return len
    end

  let read_into ic buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Lwt_io.read_into")
    else begin
      if len = 0 then
        Lwt.return 0
      else
        unsafe_read_into ic buf ofs len
    end

  let rec unsafe_read_into_exactly ic buf ofs len =
    unsafe_read_into ic buf ofs len >>= function
    | 0 ->
      Lwt.fail End_of_file
    | n ->
      let len = len - n in
      if len = 0 then
        Lwt.return_unit
      else
        unsafe_read_into_exactly ic buf (ofs + n) len

  let read_into_exactly ic buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Lwt_io.read_into_exactly")
    else begin
      if len = 0 then
        Lwt.return_unit
      else
        unsafe_read_into_exactly ic buf ofs len
    end

  let rev_concat len l =
    let buf = Bytes.create len in
    let _ =
      List.fold_left
        (fun ofs str ->
           let len = String.length str in
           let ofs = ofs - len in
           String.unsafe_blit str 0 buf ofs len;
           ofs)
        len l
    in
    buf

  let rec read_all ic total_len acc =
    let len = ic.max - ic.ptr in
    let buf = Bytes.create len in
    Lwt_bytes.unsafe_blit_to_bytes ic.buffer ic.ptr buf 0 len;
    let str = Bytes.unsafe_to_string buf in
    ic.ptr <- ic.max;
    refill ic >>= function
    | 0 ->
      Lwt.return (rev_concat (len + total_len) (str :: acc))
    | _ ->
      read_all ic (len + total_len) (str :: acc)

  let read count ic =
    match count with
    | None ->
      read_all ic 0 [] >|= Bytes.unsafe_to_string
    | Some len ->
      let buf = Bytes.create len in
      unsafe_read_into ic buf 0 len >>= fun real_len ->
      if real_len < len then
        Lwt.return Bytes.(sub buf 0 real_len |> unsafe_to_string)
      else
        Lwt.return (Bytes.unsafe_to_string buf)

  let read_value ic =
    let header = Bytes.create 20 in
    unsafe_read_into_exactly ic header 0 20 >>= fun () ->
    let bsize = Marshal.data_size header 0 in
    let buffer = Bytes.create (20 + bsize) in
    Bytes.unsafe_blit header 0 buffer 0 20;
    unsafe_read_into_exactly ic buffer 20 bsize >>= fun () ->
    (* Marshal.from_bytes should be used here, but we want 4.01
       compat. *)
    Lwt.return (Marshal.from_string (Bytes.unsafe_to_string buffer) 0)

  (* +---------------------------------------------------------------+
     | Writing                                                       |
     +---------------------------------------------------------------+ *)

  let flush = flush_total

  let rec write_char oc ch =
    let ptr = oc.ptr in
    if ptr < oc.length then begin
      oc.ptr <- ptr + 1;
      Lwt_bytes.unsafe_set oc.buffer ptr ch;
      Lwt.return_unit
    end else
      flush_partial oc >>= fun _ ->
      write_char oc ch

  let rec unsafe_write_from oc str ofs len =
    let avail = oc.length - oc.ptr in
    if avail >= len then begin
      Lwt_bytes.unsafe_blit_from_bytes str ofs oc.buffer oc.ptr len;
      oc.ptr <- oc.ptr + len;
      Lwt.return 0
    end else begin
      Lwt_bytes.unsafe_blit_from_bytes str ofs oc.buffer oc.ptr avail;
      oc.ptr <- oc.length;
      flush_partial oc >>= fun _ ->
      let len = len - avail in
      if oc.ptr = 0 then begin
        if len = 0 then
          Lwt.return 0
        else
          (* Everything has been written, try to write more: *)
          unsafe_write_from oc str (ofs + avail) len
      end else
        (* Not everything has been written, just what is
           remaining: *)
        Lwt.return len
    end

  let write_from oc buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Lwt_io.write_from")
    else begin
      if len = 0 then
        Lwt.return 0
      else
        unsafe_write_from oc buf ofs len >>= fun remaining ->
        Lwt.return (len - remaining)
    end

  let write_from_string oc buf ofs len =
    let buf = Bytes.unsafe_of_string buf in
    write_from oc buf ofs len

  let rec unsafe_write_from_exactly oc buf ofs len =
    unsafe_write_from oc buf ofs len >>= function
    | 0 ->
      Lwt.return_unit
    | n ->
      unsafe_write_from_exactly oc buf (ofs + len - n) n

  let write_from_exactly oc buf ofs len =
    if ofs < 0 || len < 0 || ofs + len > Bytes.length buf then
      Lwt.fail (Invalid_argument "Lwt_io.write_from_exactly")
    else begin
      if len = 0 then
        Lwt.return_unit
      else
        unsafe_write_from_exactly oc buf ofs len
    end

  let write_from_string_exactly oc buf ofs len =
    let buf = Bytes.unsafe_of_string buf in
    write_from_exactly oc buf ofs len

  let write oc str =
    let buf = Bytes.unsafe_of_string str in
    unsafe_write_from_exactly oc buf 0 (Bytes.length buf)

  let write_line oc str =
    let buf = Bytes.unsafe_of_string str in
    unsafe_write_from_exactly oc buf 0 (Bytes.length buf) >>= fun () ->
    write_char oc '\n'

  let write_value oc ?(flags=[]) x =
    write oc (Marshal.to_string x flags)

  (* +---------------------------------------------------------------+
     | Low-level access                                              |
     +---------------------------------------------------------------+ *)

  let rec read_block_unsafe ic size f =
    if ic.max - ic.ptr < size then
      refill ic >>= function
      | 0 ->
        Lwt.fail End_of_file
      | _ ->
        read_block_unsafe ic size f
    else begin
      let ptr = ic.ptr in
      ic.ptr <- ptr + size;
      f ic.buffer ptr
    end

  let rec write_block_unsafe oc size f =
    if oc.max - oc.ptr < size then
      flush_partial oc >>= fun _ ->
      write_block_unsafe oc size f
    else begin
      let ptr = oc.ptr in
      oc.ptr <- ptr + size;
      f oc.buffer ptr
    end

  let block :
      type m.
      m _channel ->
      int ->
      (Lwt_bytes.t -> int -> 'a Lwt.t) ->
        'a Lwt.t =
      fun ch size f ->
    if size < 0 || size > min_buffer_size then
      Lwt.fail (Invalid_argument "Lwt_io.block")
    else
    if ch.max - ch.ptr >= size then begin
      let ptr = ch.ptr in
      ch.ptr <- ptr + size;
      f ch.buffer ptr
    end else
      match ch.mode with
      | Input ->
        read_block_unsafe ch size f
      | Output ->
        write_block_unsafe ch size f

  let perform token da ch =
    if !token then begin
      if da.da_max <> ch.max || da.da_ptr < ch.ptr || da.da_ptr > ch.max then
        Lwt.fail (Invalid_argument "Lwt_io.direct_access.da_perform")
      else begin
        ch.ptr <- da.da_ptr;
        perform_io ch >>= fun count ->
        da.da_ptr <- ch.ptr;
        da.da_max <- ch.max;
        Lwt.return count
      end
    end else
      Lwt.fail
        (Failure
          ("Lwt_io.perform: this function can not be called outside " ^
           "Lwt_io.direct_access"))

  let direct_access ch f =
    let token = ref true in
    let rec da = {
      da_ptr = ch.ptr;
      da_max = ch.max;
      da_buffer = ch.buffer;
      da_perform = (fun _ -> perform token da ch);
    } in
    f da >>= fun x ->
    token := false;
    if da.da_max <> ch.max || da.da_ptr < ch.ptr || da.da_ptr > ch.max then
      Lwt.fail (Failure "Lwt_io.direct_access: invalid result of [f]")
    else begin
      ch.ptr <- da.da_ptr;
      Lwt.return x
    end

  module MakeNumberIO (Endian : EndianBigstring.EndianBigstringSig) =
  struct
    (* +-------------------------------------------------------------+
       | Reading numbers                                             |
       +-------------------------------------------------------------+ *)

    let read_int ic =
      read_block_unsafe ic 4
        (fun buffer ptr ->
          Lwt.return (Int32.to_int (Endian.get_int32 buffer ptr)))

    let read_int16 ic =
      read_block_unsafe ic 2
        (fun buffer ptr ->
          Lwt.return (Endian.get_int16 buffer ptr))

    let read_int32 ic =
      read_block_unsafe ic 4
        (fun buffer ptr ->
          Lwt.return (Endian.get_int32 buffer ptr))

    let read_int64 ic =
      read_block_unsafe ic 8
        (fun buffer ptr ->
          Lwt.return (Endian.get_int64 buffer ptr))

    let read_float32 ic =
      read_int32 ic >>= fun x -> Lwt.return (Int32.float_of_bits x)
    let read_float64 ic =
      read_int64 ic >>= fun x -> Lwt.return (Int64.float_of_bits x)

    (* +-------------------------------------------------------------+
       | Writing numbers                                             |
       +-------------------------------------------------------------+ *)

    let write_int oc v =
      write_block_unsafe oc 4
        (fun buffer ptr ->
          Endian.set_int32 buffer ptr (Int32.of_int v);
          Lwt.return_unit)

    let write_int16 oc v =
      write_block_unsafe oc 2
        (fun buffer ptr ->
          Endian.set_int16 buffer ptr v;
          Lwt.return_unit)

    let write_int32 oc v =
      write_block_unsafe oc 4
        (fun buffer ptr ->
          Endian.set_int32 buffer ptr v;
          Lwt.return_unit)

    let write_int64 oc v =
      write_block_unsafe oc 8
        (fun buffer ptr ->
          Endian.set_int64 buffer ptr v;
          Lwt.return_unit)

    let write_float32 oc v = write_int32 oc (Int32.bits_of_float v)
    let write_float64 oc v = write_int64 oc (Int64.bits_of_float v)
  end

  (* +---------------------------------------------------------------+
     | Random access                                                 |
     +---------------------------------------------------------------+ *)

  let do_seek fun_name seek pos =
    seek pos Unix.SEEK_SET >>= fun offset ->
    if offset <> pos then
      Lwt.fail (Failure (Printf.sprintf "Lwt_io.%s: seek failed" fun_name))
    else
      Lwt.return_unit

  let set_position :
      type m.
      m _channel ->
      int64 ->
        unit Lwt.t =
      fun ch pos ->
    match ch.typ, ch.mode with
    | Type_normal(_, seek), Output ->
      flush_total ch >>= fun () ->
      do_seek "set_position" seek pos >>= fun () ->
      ch.offset <- pos;
      Lwt.return_unit
    | Type_normal(_, seek), Input ->
      let current = Int64.sub ch.offset (Int64.of_int (ch.max - ch.ptr)) in
      if pos >= current && pos <= ch.offset then begin
        ch.ptr <- ch.max - (Int64.to_int (Int64.sub ch.offset pos));
        Lwt.return_unit
      end else begin
        do_seek "set_position" seek pos >>= fun () ->
        ch.offset <- pos;
        ch.ptr <- 0;
        ch.max <- 0;
        Lwt.return_unit
      end
    | Type_bytes, _ ->
      if pos < 0L || pos > Int64.of_int ch.length then
        Lwt.fail (Failure "Lwt_io.set_position: out of bounds")
      else begin
        ch.ptr <- Int64.to_int pos;
        Lwt.return_unit
      end

  let length ch = match ch.typ with
    | Type_normal(_, seek) ->
      seek 0L Unix.SEEK_END >>= fun len ->
      do_seek "length" seek ch.offset >>= fun () ->
      Lwt.return len
    | Type_bytes ->
      Lwt.return (Int64.of_int ch.length)
end

(* +-----------------------------------------------------------------+
   | Primitive operations                                            |
   +-----------------------------------------------------------------+ *)

let read_char wrapper =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  (* Speed-up in case a character is available in the buffer. It
     increases performances by 10x. *)
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Lwt.return (Lwt_bytes.unsafe_get channel.buffer ptr)
  end else
    primitive Primitives.read_char wrapper

let read_char_opt wrapper =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Lwt.return (Some(Lwt_bytes.unsafe_get channel.buffer ptr))
  end else
    primitive Primitives.read_char_opt wrapper

let read_line ic =
  primitive Primitives.read_line ic

let read_line_opt ic =
  primitive Primitives.read_line_opt ic

let read ?count ic =
  primitive (fun ic -> Primitives.read count ic) ic

let read_into ic str ofs len =
  primitive (fun ic -> Primitives.read_into ic str ofs len) ic

let read_into_exactly ic str ofs len =
  primitive (fun ic -> Primitives.read_into_exactly ic str ofs len) ic

let read_value ic =
  primitive Primitives.read_value ic

let flush oc = primitive Primitives.flush oc

let write_char wrapper x =
  let channel = wrapper.channel in
  let ptr = channel.ptr in
  if wrapper.state = Idle && ptr < channel.max then begin
    channel.ptr <- ptr + 1;
    Lwt_bytes.unsafe_set channel.buffer ptr x;
    (* Fast launching of the auto flusher: *)
    if not channel.auto_flushing then begin
      channel.auto_flushing <- true;
      ignore (auto_flush channel);
      Lwt.return_unit
    end else
      Lwt.return_unit
  end else
    primitive (fun oc -> Primitives.write_char oc x) wrapper

let write oc str =
  primitive (fun oc -> Primitives.write oc str) oc

let write_line oc x =
  primitive (fun oc -> Primitives.write_line oc x) oc

let write_from oc str ofs len =
  primitive (fun oc -> Primitives.write_from oc str ofs len) oc

let write_from_string oc str ofs len =
  primitive (fun oc -> Primitives.write_from_string oc str ofs len) oc

let write_from_exactly oc str ofs len =
  primitive (fun oc -> Primitives.write_from_exactly oc str ofs len) oc

let write_from_string_exactly oc str ofs len =
  primitive (fun oc -> Primitives.write_from_string_exactly oc str ofs len) oc

let write_value oc ?flags x =
  primitive (fun oc -> Primitives.write_value oc ?flags x) oc

let block ch size f =
  primitive (fun ch -> Primitives.block ch size f) ch

let direct_access ch f =
  primitive (fun ch -> Primitives.direct_access ch f) ch

let set_position ch pos =
  primitive (fun ch -> Primitives.set_position ch pos) ch

let length ch =
  primitive Primitives.length ch

module type NumberIO = sig
  val read_int : input_channel -> int Lwt.t
  val read_int16 : input_channel -> int Lwt.t
  val read_int32 : input_channel -> int32 Lwt.t
  val read_int64 : input_channel -> int64 Lwt.t
  val read_float32 : input_channel -> float Lwt.t
  val read_float64 : input_channel -> float Lwt.t
  val write_int : output_channel -> int -> unit Lwt.t
  val write_int16 : output_channel -> int -> unit Lwt.t
  val write_int32 : output_channel -> int32 -> unit Lwt.t
  val write_int64 : output_channel -> int64 -> unit Lwt.t
  val write_float32 : output_channel -> float -> unit Lwt.t
  val write_float64 : output_channel -> float -> unit Lwt.t
end

module MakeNumberIO (Endian : EndianBigstring.EndianBigstringSig) =
struct
  module Primitives = Primitives.MakeNumberIO (Endian)

  let read_int ic = primitive Primitives.read_int ic
  let read_int16 ic = primitive Primitives.read_int16 ic
  let read_int32 ic = primitive Primitives.read_int32 ic
  let read_int64 ic = primitive Primitives.read_int64 ic
  let read_float32 ic = primitive Primitives.read_float32 ic
  let read_float64 ic = primitive Primitives.read_float64 ic

  let write_int oc x = primitive (fun oc -> Primitives.write_int oc x) oc
  let write_int16 oc x = primitive (fun oc -> Primitives.write_int16 oc x) oc
  let write_int32 oc x = primitive (fun oc -> Primitives.write_int32 oc x) oc
  let write_int64 oc x = primitive (fun oc -> Primitives.write_int64 oc x) oc
  let write_float32 oc x =
    primitive (fun oc -> Primitives.write_float32 oc x) oc
  let write_float64 oc x =
    primitive (fun oc -> Primitives.write_float64 oc x) oc
end

module LE = MakeNumberIO (EndianBigstring.LittleEndian_unsafe)
module BE = MakeNumberIO (EndianBigstring.BigEndian_unsafe)

type byte_order = Lwt_sys.byte_order = Little_endian | Big_endian
let system_byte_order = Lwt_sys.byte_order

include (val (match system_byte_order with
  | Little_endian -> (module LE : NumberIO)
  | Big_endian -> (module BE : NumberIO)) : NumberIO)

(* +-----------------------------------------------------------------+
   | Other                                                           |
   +-----------------------------------------------------------------+ *)

let read_chars ic = Lwt_stream.from (fun _ -> read_char_opt ic)
let write_chars oc chars =
  Lwt_stream.iter_s (fun char -> write_char oc char) chars
let read_lines ic = Lwt_stream.from (fun _ -> read_line_opt ic)
let write_lines oc lines =
  Lwt_stream.iter_s (fun line -> write_line oc line) lines

let zero =
  make
    ~mode:input
    ~buffer:(Lwt_bytes.create min_buffer_size)
    (fun str ofs len -> Lwt_bytes.fill str ofs len '\x00'; Lwt.return len)

let null =
  make
    ~mode:output
    ~buffer:(Lwt_bytes.create min_buffer_size)
    (fun _str _ofs len -> Lwt.return len)

(* Do not close standard ios on close, otherwise uncaught exceptions
   will not be printed *)
let stdin = of_fd ~mode:input Lwt_unix.stdin
let stdout = of_fd ~mode:output Lwt_unix.stdout
let stderr = of_fd ~mode:output Lwt_unix.stderr

let fprint oc txt = write oc txt
let fprintl oc txt = write_line oc txt
let fprintf oc fmt = Printf.ksprintf (fun txt -> write oc txt) fmt
let fprintlf oc fmt = Printf.ksprintf (fun txt -> write_line oc txt) fmt

let print txt = write stdout txt
let printl txt = write_line stdout txt
let printf fmt = Printf.ksprintf print fmt
let printlf fmt = Printf.ksprintf printl fmt

let eprint txt = write stderr txt
let eprintl txt = write_line stderr txt
let eprintf fmt = Printf.ksprintf eprint fmt
let eprintlf fmt = Printf.ksprintf eprintl fmt

let pipe ?in_buffer ?out_buffer _ =
  let fd_r, fd_w = Lwt_unix.pipe () in
  (of_fd ?buffer:in_buffer ~mode:input fd_r,
   of_fd ?buffer:out_buffer ~mode:output fd_w)

type file_name = string

let open_file :
    type m.
    ?buffer : Lwt_bytes.t ->
    ?flags : Unix.open_flag list ->
    ?perm : Unix.file_perm ->
    mode : m mode ->
    file_name ->
      m channel Lwt.t =
    fun ?buffer ?flags ?perm ~mode filename ->
  let flags = match flags, mode with
    | Some l, _ ->
      l
    | None, Input ->
      [Unix.O_RDONLY; Unix.O_NONBLOCK]
    | None, Output ->
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK]
  and perm = match perm, mode with
    | Some p, _ ->
      p
    | None, Input ->
      0
    | None, Output ->
      0o666
  in
  Lwt_unix.openfile filename flags perm >>= fun fd ->
  Lwt.return (of_fd ?buffer ~mode fd)

let with_file ?buffer ?flags ?perm ~mode filename f =
  open_file ?buffer ?flags ?perm ~mode filename >>= fun ic ->
  Lwt.finalize
    (fun () -> f ic)
    (fun () -> close ic)

let prng = lazy (Random.State.make_self_init ())

let temp_file_name temp_dir prefix suffix =
  let rnd = Random.State.int (Lazy.force prng) 0x1000000 in
  Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let open_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix ?(suffix = "") () =
  let flags =
    match flags with
    | None -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC]
    | Some flags -> flags
  in
  let dir =
    match temp_dir with
    | None -> Filename.get_temp_dir_name ()
    | Some dirname -> dirname
  in
  let prefix =
    match prefix with
    | None -> "lwt_io_temp_file_"
    | Some prefix -> prefix
  in

  let rec attempt n =
    let fname = temp_file_name dir prefix suffix in
    Lwt.catch
      (fun () ->
        open_file ?buffer ~flags ?perm ~mode:Output fname >>= fun chan ->
        Lwt.return (fname, chan))
      (function
        | Unix.Unix_error _ when n < 1000 -> attempt (n + 1)
        | exn -> Lwt.fail exn)
  in
  attempt 0

let with_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix ?suffix f =
  open_temp_file
    ?buffer ?flags ?perm ?temp_dir ?prefix ?suffix () >>= fun (fname, chan) ->
  Lwt.finalize
    (fun () ->
      f (fname, chan))
    (fun () ->
      close chan >>= fun () ->
      Lwt_unix.unlink fname)

let create_temp_dir
    ?(perm = 0o755)
    ?(parent = Filename.get_temp_dir_name ())
    ?(prefix = "lwt_io_temp_dir_")
    ?(suffix = "")
    () =
  let rec attempt n =
    let name = temp_file_name parent prefix suffix in
    Lwt.catch
      (fun () ->
        Lwt_unix.mkdir name perm >>= fun () ->
        Lwt.return name)
      (function
      | Unix.Unix_error (Unix.EEXIST, _, _) when n < 1000 -> attempt (n + 1)
      | exn -> Lwt.fail exn)
  in
  attempt 0

(* This is likely VERY slow for directories with many files. That is probably
   best addressed by switching to blocking calls run inside a worker thread,
   i.e. with Lwt_preemptive. *)
let rec delete_recursively directory =
  Lwt_unix.files_of_directory directory
  |> Lwt_stream.iter_s begin fun entry ->
    if entry = Filename.current_dir_name ||
       entry = Filename.parent_dir_name then
      Lwt.return ()
    else
      let path = Filename.concat directory entry in
      Lwt_unix.lstat path >>= fun stat ->
      if stat.Lwt_unix.st_kind = Lwt_unix.S_DIR then
        delete_recursively path
      else
        Lwt_unix.unlink path
  end >>= fun () ->
  Lwt_unix.rmdir directory

let with_temp_dir ?perm ?parent ?prefix ?suffix f =
  create_temp_dir ?perm ?parent ?prefix ?suffix () >>= fun name ->
  Lwt.finalize
    (fun () ->
      f name)
    (fun () ->
      delete_recursively name)

let file_length filename =
  Lwt_unix.stat filename >>= fun stat ->
  if stat.Unix.st_kind = Unix.S_DIR then
    Lwt.fail (Unix.(Unix_error (EISDIR, "file_length", filename)))
  else
    with_file ~mode:input filename length

let close_socket fd =
  Lwt.finalize
    (fun () ->
       Lwt.catch
         (fun () ->
            Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
            Lwt.return_unit)
         (function
           (* Occurs if the peer closes the connection first. *)
           | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return_unit
           | exn -> Lwt.fail exn) [@ocaml.warning "-4"])
    (fun () ->
       Lwt_unix.close fd)

let open_connection ?fd ?in_buffer ?out_buffer sockaddr =
  let fd =
    match fd with
    | None ->
      Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    | Some fd ->
      fd
  in
  let close = lazy (close_socket fd) in
  Lwt.catch
    (fun () ->
       Lwt_unix.connect fd sockaddr >>= fun () ->
       (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
       Lwt.return (make ?buffer:in_buffer
                     ~close:(fun _ -> Lazy.force close)
                     ~mode:input (Lwt_bytes.read fd),
                   make ?buffer:out_buffer
                     ~close:(fun _ -> Lazy.force close)
                     ~mode:output (Lwt_bytes.write fd)))
    (fun exn ->
       Lwt_unix.close fd >>= fun () ->
       Lwt.fail exn)

let with_close_connection f (ic, oc) =
  (* If the user already tried to close the socket and got an exception, we
     don't want to raise that exception again during implicit close. *)
  let close_if_not_closed channel =
    if is_closed channel then Lwt.return_unit else close channel in

  Lwt.finalize
    (fun () -> f (ic, oc))
    (fun () -> close_if_not_closed ic <&> close_if_not_closed oc)

let with_connection ?fd ?in_buffer ?out_buffer sockaddr f =
  open_connection ?fd ?in_buffer ?out_buffer sockaddr >>= fun channels ->
  with_close_connection f channels

type server = {
  shutdown : unit Lwt.t Lazy.t;
}

let shutdown_server server = Lazy.force server.shutdown

let shutdown_server_deprecated server =
  Lwt.async (fun () -> shutdown_server server)

(* There are several variants of establish_server that have accumulated over the
   years in Lwt_io. This is their underlying implementation. The functions
   exposed in the API are various wrappers around this one. *)
let establish_server_generic
    bind_function
    ?fd:preexisting_socket_for_listening
    ?(backlog = Lwt_unix.somaxconn () [@ocaml.warning "-3"])
    listening_address
    connection_handler_callback =

  let listening_socket =
    match preexisting_socket_for_listening with
    | None ->
      Lwt_unix.socket
        (Unix.domain_of_sockaddr listening_address) Unix.SOCK_STREAM 0
    | Some socket ->
      socket
  in
  Lwt_unix.setsockopt listening_socket Unix.SO_REUSEADDR true;

  (* This promise gets resolved with `Should_stop when the user calls
     Lwt_io.shutdown_server. This begins the shutdown procedure. *)
  let should_stop, notify_should_stop =
    Lwt.wait () in

  (* Some time after Lwt_io.shutdown_server is called, this function
     establish_server_generic will actually close the listening socket. At that
     point, this promise is resolved. This ends the shutdown procedure. *)
  let wait_until_listening_socket_closed, notify_listening_socket_closed =
    Lwt.wait () in

  let rec accept_loop () =
    let try_to_accept =
      Lwt_unix.accept listening_socket >|= fun x ->
      `Accepted x
    in

    Lwt.pick [try_to_accept; should_stop] >>= function
    | `Accepted (client_socket, client_address) ->
      begin
        try Lwt_unix.set_close_on_exec client_socket
        with Invalid_argument _ -> ()
      end;

      connection_handler_callback client_address client_socket;

      accept_loop ()

    | `Should_stop ->
      Lwt_unix.close listening_socket >>= fun () ->

      begin match listening_address with
      | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
        Unix.unlink path
      | _ ->
        ()
      end [@ocaml.warning "-4"];

      Lwt.wakeup_later notify_listening_socket_closed ();
      Lwt.return_unit
  in

  let server =
    {shutdown =
      lazy begin
        Lwt.wakeup_later notify_should_stop `Should_stop;
        wait_until_listening_socket_closed
      end}
  in

  (* Actually start the server. *)
  let server_has_started =
    bind_function listening_socket listening_address >>= fun () ->
    Lwt_unix.listen listening_socket backlog;

    Lwt.async accept_loop;

    Lwt.return_unit
  in

  server, server_has_started

let establish_server_with_client_socket
    ?server_fd ?backlog ?(no_close = false) sockaddr f =
  let handler client_address client_socket =
    Lwt.async begin fun () ->
      (* Not using Lwt.finalize here, to make sure that exceptions from [f]
         reach !Lwt.async_exception_hook before exceptions from closing the
         channels. *)
      Lwt.catch
        (fun () -> f client_address client_socket)
        (fun exn ->
          !Lwt.async_exception_hook exn;
          Lwt.return_unit)

      >>= fun () ->
      if no_close then Lwt.return_unit
      else
        if Lwt_unix.state client_socket = Lwt_unix.Closed then
          Lwt.return_unit
        else
          Lwt.catch
            (fun () -> close_socket client_socket)
            (fun exn ->
              !Lwt.async_exception_hook exn;
              Lwt.return_unit)
    end
  in

  let server, server_started =
    establish_server_generic
      Lwt_unix.bind ?fd:server_fd ?backlog sockaddr handler
  in
  server_started >>= fun () ->
  Lwt.return server

let establish_server_with_client_address_generic
    bind_function
    ?fd
    ?(buffer_size = !default_buffer_size)
    ?backlog
    ?(no_close = false)
    sockaddr
    handler =

  let best_effort_close channel =
    (* First, check whether the channel is closed. f may have already tried to
       close the channel, received an exception, and handled it somehow. If so,
       trying to close the channel here will trigger the same exception, which
       will go to !Lwt.async_exception_hook, despite the user's efforts. *)
    (* The Invalid state is not possible on the channel, because it was not
       created using Lwt_io.atomic. *)
    if is_closed channel then
      Lwt.return_unit
    else
      Lwt.catch
        (fun () -> close channel)
        (fun exn ->
          !Lwt.async_exception_hook exn;
          Lwt.return_unit)
  in

  let handler client_address client_socket =
    Lwt.async (fun () ->
      let close = lazy (close_socket client_socket) in
      let input_channel =
        of_fd
          ~buffer:(Lwt_bytes.create buffer_size)
          ~mode:input
          ~close:(fun () -> Lazy.force close)
          client_socket
      in
      let output_channel =
        of_fd
          ~buffer:(Lwt_bytes.create buffer_size)
          ~mode:output
          ~close:(fun () -> Lazy.force close)
          client_socket
      in

      (* Not using Lwt.finalize here, to make sure that exceptions from [f]
         reach !Lwt.async_exception_hook before exceptions from closing the
         channels. *)
      Lwt.catch
        (fun () ->
          handler client_address (input_channel, output_channel))
        (fun exn ->
          !Lwt.async_exception_hook exn;
          Lwt.return_unit)

      >>= fun () ->
      if no_close then Lwt.return_unit
      else
        best_effort_close input_channel >>= fun () ->
        best_effort_close output_channel)
  in

  establish_server_generic bind_function ?fd ?backlog sockaddr handler

let establish_server_with_client_address
    ?fd ?buffer_size ?backlog ?no_close sockaddr handler =
  let server, server_started =
    establish_server_with_client_address_generic
      Lwt_unix.bind ?fd ?buffer_size ?backlog ?no_close sockaddr handler
  in
  server_started >>= fun () ->
  Lwt.return server

let establish_server ?fd ?buffer_size ?backlog ?no_close sockaddr f =
  let f _addr c = f c in
  establish_server_with_client_address
    ?fd ?buffer_size ?backlog ?no_close sockaddr f

(* Old, deprecated version of [establish_server]. This function has to persist
   for a while, in some form, until it is no longer exposed as
   [Lwt_io.Versioned.establish_server_1]. *)
let establish_server_deprecated ?fd ?buffer_size ?backlog sockaddr f =
  let blocking_bind fd addr =
    Lwt.return (Lwt_unix.Versioned.bind_1 fd addr) [@ocaml.warning "-3"]
  in
  let f _addr c =
    f c;
    Lwt.return_unit
  in

  let server, server_started =
    establish_server_with_client_address_generic
      blocking_bind ?fd ?buffer_size ?backlog ~no_close:true sockaddr f
  in

  (* Poll for exceptions in server startup that may have occurred synchronously.
     This emulates an old, deprecated behavior. *)
  Lwt.ignore_result server_started;
  server

let ignore_close ch =
  ignore (close ch)

let make_stream f lazy_ic =
  let lazy_ic =
    lazy(Lazy.force lazy_ic >>= fun ic ->
         Gc.finalise ignore_close ic;
         Lwt.return ic)
  in
  Lwt_stream.from (fun _ ->
    Lazy.force lazy_ic >>= fun ic ->
    f ic >>= fun x ->
    if x = None then
      close ic >>= fun () ->
      Lwt.return x
    else
      Lwt.return x)

let lines_of_file filename =
  make_stream read_line_opt (lazy(open_file ~mode:input filename))

let lines_to_file filename lines =
  with_file ~mode:output filename (fun oc -> write_lines oc lines)

let chars_of_file filename =
  make_stream read_char_opt (lazy(open_file ~mode:input filename))

let chars_to_file filename chars =
  with_file ~mode:output filename (fun oc -> write_chars oc chars)

let hexdump_stream oc stream = write_lines oc (Lwt_stream.hexdump stream)
let hexdump oc buf = hexdump_stream oc (Lwt_stream.of_string buf)

let set_default_buffer_size size =
  check_buffer_size "set_default_buffer_size" size;
  default_buffer_size := size
let default_buffer_size _ = !default_buffer_size

module Versioned =
struct
  let establish_server_1 = establish_server_deprecated
  let establish_server_2 = establish_server

  let shutdown_server_1 = shutdown_server_deprecated
  let shutdown_server_2 = shutdown_server
end
