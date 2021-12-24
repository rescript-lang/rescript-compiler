(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



exception Not_available of string

let () = Callback.register_exception "lwt:not-available" (Not_available "")

let windows = Sys.win32

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

let have = function
  | `wait4
  | `recv_msg
  | `send_msg
  | `madvise -> not Sys.win32
  | `mincore -> not (Sys.win32 || Sys.cygwin)
  | `get_cpu -> Lwt_config._HAVE_GETCPU
  | `get_affinity
  | `set_affinity -> Lwt_config._HAVE_AFFINITY
  | `fd_passing -> Lwt_config._HAVE_FD_PASSING
  | `get_credentials -> Lwt_config._HAVE_GET_CREDENTIALS
  | `fdatasync -> Lwt_config._HAVE_FDATASYNC
  | `libev -> Lwt_config._HAVE_LIBEV

type byte_order = Little_endian | Big_endian

external get_byte_order : unit -> byte_order = "lwt_unix_system_byte_order"

let byte_order = get_byte_order ()
