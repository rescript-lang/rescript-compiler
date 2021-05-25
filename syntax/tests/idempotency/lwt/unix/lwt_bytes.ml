(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create size = Array1.create char c_layout size
let length bytes = Array1.dim bytes

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

[@@@ocaml.warning "-3"]
external unsafe_fill : t -> int -> int -> char -> unit = "noalloc"
[@@@ocaml.warning "+3"]

let fill bytes ofs len ch =
  if ofs < 0 || len < 0 || ofs > length bytes - len then
    invalid_arg "Lwt_bytes.fill"
  else
    unsafe_fill bytes ofs len ch

(* +-----------------------------------------------------------------+
   | Blitting                                                        |
   +-----------------------------------------------------------------+ *)

[@@@ocaml.warning "-3"]
external unsafe_blit_from_bytes : Bytes.t -> int -> t -> int -> int -> unit = "noalloc"
external unsafe_blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit = "noalloc"
external unsafe_blit : t -> int -> t -> int -> int -> unit = "noalloc"
[@@@ocaml.warning "+3"]

let blit_from_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > Bytes.length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Lwt_bytes.blit_from_bytes"
  else
    unsafe_blit_from_bytes src_buf src_ofs dst_buf dst_ofs len

let blit_to_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > Bytes.length dst_buf - len) then
    invalid_arg "Lwt_bytes.blit_to_bytes"
  else
    unsafe_blit_to_bytes src_buf src_ofs dst_buf dst_ofs len

let blit src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Lwt_bytes.blit"
  else
    unsafe_blit src_buf src_ofs dst_buf dst_ofs len

let of_bytes buf =
  let len = Bytes.length buf in
  let bytes = create len in
  unsafe_blit_from_bytes buf 0 bytes 0 len;
  bytes

let of_string str = of_bytes (Bytes.unsafe_of_string str)

let to_bytes bytes =
  let len = length bytes in
  let str = Bytes.create len in
  unsafe_blit_to_bytes bytes 0 str 0 len;
  str

let to_string bytes = Bytes.unsafe_to_string (to_bytes bytes)

let proxy = Array1.sub

let extract buf ofs len =
  if ofs < 0 || len < 0 || ofs > length buf - len then
    invalid_arg "Lwt_bytes.extract"
  else begin
    let buf' = create len in
    blit buf ofs buf' 0 len;
    buf'
  end

let copy buf =
  let len = length buf in
  let buf' = create len in
  blit buf 0 buf' 0 len;
  buf'

(* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ *)

open Lwt_unix

let read =
  Lwt_unix.read_bigarray "Lwt_bytes.read" [@ocaml.warning "-3"]

let write =
  Lwt_unix.write_bigarray "Lwt_bytes.write" [@ocaml.warning "-3"]

external stub_recv : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_recv"

let recv fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.recv"
  else
    wrap_syscall Read fd (fun () -> stub_recv (unix_file_descr fd) buf pos len flags)

external stub_send : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_send"

let send fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.send"
  else
    wrap_syscall Write fd (fun () -> stub_send (unix_file_descr fd) buf pos len flags)

type io_vector = {
  iov_buffer : t;
  iov_offset : int;
  iov_length : int;
}

let io_vector ~buffer ~offset ~length = ({
  iov_buffer = buffer;
  iov_offset = offset;
  iov_length = length;
} : io_vector)

let convert_io_vectors old_io_vectors =
  let io_vectors = IO_vectors.create () in
  old_io_vectors
  |> List.iter (fun ({iov_buffer; iov_offset; iov_length} : io_vector) ->
    IO_vectors.append_bigarray io_vectors iov_buffer iov_offset iov_length);
  io_vectors

let recv_msg ~socket ~io_vectors =
  Lwt_unix.recv_msg ~socket ~io_vectors:(convert_io_vectors io_vectors)

let send_msg ~socket ~io_vectors ~fds =
  Lwt_unix.send_msg ~socket ~io_vectors:(convert_io_vectors io_vectors) ~fds

external stub_recvfrom : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr = "lwt_unix_bytes_recvfrom"

let recvfrom fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.recvfrom"
  else
    wrap_syscall Read fd (fun () -> stub_recvfrom (unix_file_descr fd) buf pos len flags)

external stub_sendto : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int = "lwt_unix_bytes_sendto"

let sendto fd buf pos len flags addr =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.sendto"
  else
    wrap_syscall Write fd (fun () -> stub_sendto (unix_file_descr fd) buf pos len flags addr)

(* +-----------------------------------------------------------------+
   | Memory mapped files                                             |
   +-----------------------------------------------------------------+ *)

let map_file ~fd ?pos ~shared ?(size=(-1)) () =
  Mmap.V1.map_file fd ?pos char c_layout shared [|size|]
  |> Bigarray.array1_of_genarray

[@@@ocaml.warning "-3"]
external mapped : t -> bool = "noalloc"
[@@@ocaml.warning "+3"]

type advice =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED
  | MADV_MERGEABLE
  | MADV_UNMERGEABLE
  | MADV_HUGEPAGE
  | MADV_NOHUGEPAGE

external stub_madvise : t -> int -> int -> advice -> unit = "lwt_unix_madvise"

let madvise buf pos len advice =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.madvise"
  else
    stub_madvise buf pos len advice

external get_page_size : unit -> int = "lwt_unix_get_page_size"

let page_size = get_page_size ()

external stub_mincore : t -> int -> int -> bool array -> unit = "lwt_unix_mincore"

let mincore buffer offset states =
  if (offset mod page_size <> 0
      || offset < 0
      || length buffer - offset < (Array.length states - 1) * page_size + 1)
  then
    invalid_arg "Lwt_bytes.mincore"
  else
    stub_mincore buffer offset (Array.length states * page_size) states

external wait_mincore_job : t -> int -> unit job = "lwt_unix_wait_mincore_job"

let wait_mincore buffer offset =
  if offset < 0 || offset >= length buffer then
    invalid_arg "Lwt_bytes.wait_mincore"
  else begin
    let state = [|false|] in
    mincore buffer (offset - (offset mod page_size)) state;
    if state.(0) then
      Lwt.return_unit
    else
      run_job (wait_mincore_job buffer offset)
  end
