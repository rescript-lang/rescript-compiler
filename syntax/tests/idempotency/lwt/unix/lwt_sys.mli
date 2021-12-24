(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** System informations. *)

exception Not_available of string
  (** [Not_available(feature)] is an exception that may be raised when
      a feature is not available on the current system. *)

(** Features that can be tested. *)
type feature =
    [ `wait4
    | `get_cpu
    | `get_affinity
    | `set_affinity
    | `recv_msg
    | `send_msg
    | `fd_passing
    | `get_credentials
    | `mincore
    | `madvise
    | `fdatasync
    | `libev ]

val have : feature -> bool
  (** Test whether the given feature is available on the current
      system. *)

type byte_order = Little_endian | Big_endian
    (** Type of byte order *)

val byte_order : byte_order
  (** The byte order used by the computer running the program. *)

val windows : bool
  [@@ocaml.deprecated " Use Sys.win32."]
  (** @deprecated Use [Sys.win32]. *)
