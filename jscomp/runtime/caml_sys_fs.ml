(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type fsStats

external statSync : string -> fsStats = "" [@@bs.module "fs"]

external existsSync : string -> Js.boolean = "" [@@bs.module "fs"]

external isDirectory : fsStats -> unit -> Js.boolean = "" [@@bs.send]

external error_code : Js_exn.t -> string Js.undefined = "code" [@@bs.get]

let caml_sys_is_directory p =
  try
    Js.to_bool @@ isDirectory (statSync p) ()
  with
  | Js_exn.Error err when Some "ENOENT" = Js_undefined.to_opt @@ error_code err
    ->
    raise @@ Sys_error (p ^ ": No such file or directory")

let caml_sys_file_exists p =
  Js.to_bool @@ existsSync p


external fs_unlinkSync : string -> 'a Js_undefined.t = "unlinkSync" [@@bs.module "fs"]

let caml_sys_remove (path : string) : unit =
  ignore @@ fs_unlinkSync path


(* this must match the definition in ../stdlib/pervasives.ml *)
type open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

external fs_O_RDONLY : int = "O_RDONLY" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_WRONLY : int = "O_WRONLY" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_RDWR : int = "O_RDWR" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_CREAT : int = "O_CREAT" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_EXCL : int = "O_EXCL" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_TRUNC : int = "O_TRUNC" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_APPEND : int = "O_APPEND" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_NONBLOCK : int = "O_NONBLOCK" [@@bs.module "fs"] [@@bs.scope "constants"]

let int_of_open_flag = function
  | Open_rdonly -> fs_O_RDONLY
  | Open_wronly -> fs_O_WRONLY
  | Open_append -> fs_O_APPEND
  | Open_creat -> fs_O_CREAT
  | Open_trunc -> fs_O_TRUNC
  | Open_excl -> fs_O_EXCL
  | Open_nonblock -> fs_O_NONBLOCK
    (* Open_binary and Open_text don't seem to have node equivalents *)
  | Open_binary
  | Open_text -> 0

(* let int_of_open_flags flags = List.fold_left (fun acc f -> acc lor (int_of_open_flag f)) 0 flags *)
[@@@warning "-20"]
let int_of_open_flags : open_flag list -> int = [%bs.raw {|
  function (int_of_open_flag) {
    return function (flags) {
      var res = 0;
      while (flags instanceof Array) {
        res |= int_of_open_flag(flags[0]);
        flags = flags[1];
      }
      return res;
    };
  }
|}] int_of_open_flag

external fs_openSync : string -> int -> int -> int = "openSync" [@@bs.module "fs"]

let caml_sys_open (file : string) (flags : open_flag list) (mode : int) : int =
  fs_openSync file (int_of_open_flags flags) mode


external fs_writeSync : int -> string -> string -> int = "writeSync" [@@bs.module "fs"]

let caml_ml_open_descriptor_out (fd : int)  : Caml_io.out_channel =
  Caml_io.{
    fd = fd;
    buffer = "";
    output = (fun _ s ->
      ignore @@ fs_writeSync fd s "binary")
  }


external fs_closeSync : int -> 'a Js_undefined.t = "closeSync" [@@bs.module "fs"]

let caml_ml_close_channel (oc : Caml_io.out_channel) : unit =
  ignore @@ fs_closeSync Caml_io.(oc.fd)

