(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



include Lwt_features

let _HAVE_GET_CREDENTIALS =
  _HAVE_GET_CREDENTIALS_LINUX ||
  _HAVE_GET_CREDENTIALS_NETBSD ||
  _HAVE_GET_CREDENTIALS_OPENBSD ||
  _HAVE_GET_CREDENTIALS_FREEBSD ||
  _HAVE_GETPEEREID
