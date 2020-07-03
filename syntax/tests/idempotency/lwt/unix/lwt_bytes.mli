(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Byte arrays *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** Type of array of bytes. *)

val create : int -> t
  (** Creates a new byte array of the given size. *)

val length : t -> int
  (** Returns the length of the given byte array. *)

(** {2 Access} *)

val get : t -> int -> char
  (** [get buffer offset] returns the byte at offset [offset] in
      [buffer]. *)

val set : t -> int -> char -> unit
  (** [get buffer offset value] changes the value of the byte at
      offset [offset] in [buffer] to [value]. *)

val unsafe_get : t -> int -> char
  (** Same as {!get} but without bounds checking. *)

val unsafe_set : t -> int -> char -> unit
  (** Same as {!set} but without bounds checking. *)

(** {2 Conversions} *)

val of_bytes : bytes -> t
  (** [of_bytes buf] returns a newly allocated byte array with the
      same contents as [buf]. *)

val of_string : string -> t
  (** [of_string buf] returns a newly allocated byte array with the
      same contents as [buf]. *)

val to_bytes : t -> bytes
  (** [to_bytes buf] returns newly allocated bytes with the same
      contents as [buf]. *)

val to_string : t -> string
  (** [to_string buf] returns a newly allocated string with the same
      contents as [buf]. *)

(** {2 Copying} *)

val blit : t -> int -> t -> int -> int -> unit
  (** [blit buf1 ofs1 buf2 ofs2 len] copies [len] bytes from [buf1]
      starting at offset [ofs1] to [buf2] starting at offset [ofs2]. *)

val blit_from_bytes : bytes -> int -> t -> int -> int -> unit
  (** Same as {!blit} but the first buffer is a string instead of a byte
      array. *)

val blit_to_bytes : t -> int -> bytes -> int -> int -> unit
  (** Same as {!blit} but the second buffer is a string instead of a byte
      array. *)

val unsafe_blit : t -> int -> t -> int -> int -> unit
  (** Same as {!blit} but without bound checking. *)

val unsafe_blit_from_bytes : bytes -> int -> t -> int -> int -> unit
  (** Same as {!Lwt_bytes.blit_from_bytes} but without bounds checking. *)

val unsafe_blit_to_bytes : t -> int -> bytes -> int -> int -> unit
  (** Same as {!Lwt_bytes.blit_to_bytes} but without bounds checking. *)

val proxy : t -> int -> int -> t
  (** [proxy buffer offset length] creates a ``proxy''. The returned
      byte array share the data of [buffer] but with different
      bounds. *)

val extract : t -> int -> int -> t
  (** [extract buffer offset length] creates a new byte array of
      length [length] and copy the [length] bytes of [buffer] at
      [offset] into it.  *)

val copy : t -> t
  (** [copy buffer] creates a copy of the given byte array. *)

(** {2 Filling} *)

val fill : t -> int -> int -> char -> unit
  (** [fill buffer offset length value] puts [value] in all [length]
      bytes of [buffer] starting at offset [offset]. *)

external unsafe_fill : t -> int -> int -> char -> unit = "lwt_unix_fill_bytes" "noalloc"
  [@@ocaml.warning "-3"]
  (** Same as {!fill} but without bounds checking. *)

(** {2 IOs} *)

(** The following functions behave similarly to the ones in {!Lwt_unix}, except
    they use byte arrays instead of strings, and they never perform extra copies
    of the data. *)

val read : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t
val write : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t

val recv : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t
(** Not implemented on Windows. *)

val send : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t
(** Not implemented on Windows. *)

val recvfrom : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> (int * Unix.sockaddr) Lwt.t
(** Not implemented on Windows. *)

val sendto : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int Lwt.t
(** Not implemented on Windows. *)

type io_vector = {
  iov_buffer : t;
  iov_offset : int;
  iov_length : int;
}

val io_vector : buffer : t -> offset : int -> length : int -> io_vector

val recv_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> (int * Unix.file_descr list) Lwt.t
  [@@ocaml.deprecated " Use Lwt_unix.Versioned.recv_msg_2."]
(** Not implemented on Windows.

    @deprecated Use {!Lwt_unix.Versioned.recv_msg_2}. *)

val send_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> fds : Unix.file_descr list -> int Lwt.t
  [@@ocaml.deprecated " Use Lwt_unix.Versioned.send_msg_2."]
(** Not implemented on Windows.

    @deprecated Use {!Lwt_unix.Versioned.send_msg_2}. *)

(** {2 Memory mapped files} *)

val map_file : fd : Unix.file_descr -> ?pos : int64 -> shared : bool -> ?size : int -> unit -> t
  (** [map_file ~fd ?pos ~shared ?size ()] maps the file descriptor
      [fd] to an array of bytes. *)

external mapped : t -> bool = "lwt_unix_mapped" "noalloc"
  [@@ocaml.warning "-3"]
  (** [mapped buffer] returns [true] iff [buffer] is a memory mapped
      file. *)

(** Type of advise that can be sent to the kernel by the program. See
    the manual madvise(2) for a description of each. *)
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

val madvise : t -> int -> int -> advice -> unit
  (** [madvise buffer pos len advice] advises the kernel how the
      program will use the memory mapped file between [pos] and
      [pos + len].

      This call is not available on windows. *)

val page_size : int
  (** Size of pages. *)

val mincore : t -> int -> bool array -> unit
  (** [mincore buffer offset states] tests whether the given pages are
      in the system memory (the RAM). The [offset] argument must be a
      multiple of {!page_size}. [states] is used to store the result;
      each cases is [true] if the corresponding page is in RAM and
      [false] otherwise.

      This call is not available on windows and cygwin. *)

val wait_mincore : t -> int -> unit Lwt.t
  (** [wait_mincore buffer offset] waits until the page containing the
      byte at offset [offset] is in RAM.

      This functions is not available on windows and cygwin. *)
