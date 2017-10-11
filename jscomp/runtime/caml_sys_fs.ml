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

(* wrap a thunk such that any JS errors it throws are caught and translated to
 * Sys_error, to match standard OCaml behavior *)
let with_sys_error (f : unit -> 'a) : 'a =
  try f () with
  | Js_exn.Error err ->
    let message = (match Js_exn.message err with
        | Some m -> m
        | None -> "Unknown error")
    in
    raise (Sys_error message)

type fsStats

external statSync : string -> fsStats = "" [@@bs.module "fs"]

external existsSync : string -> Js.boolean = "" [@@bs.module "fs"]

external isDirectory : fsStats -> unit -> Js.boolean = "" [@@bs.send]

let caml_sys_is_directory p =
  with_sys_error (fun () ->
      Js.to_bool @@ isDirectory (statSync p) ()
    )

let caml_sys_file_exists p =
  with_sys_error (fun () ->
      Js.to_bool @@ existsSync p
    )


external unlinkSync : string -> 'a Js_undefined.t = "" [@@bs.module "fs"]

let caml_sys_remove (path : string) : unit =
  with_sys_error (fun () ->
      ignore @@ unlinkSync path
    )


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

external fs_O_RDONLY : int Js_undefined.t   = "O_RDONLY" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_WRONLY : int Js_undefined.t   = "O_WRONLY" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_RDWR : int Js_undefined.t     = "O_RDWR" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_CREAT : int Js_undefined.t    = "O_CREAT" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_EXCL : int Js_undefined.t     = "O_EXCL" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_TRUNC : int Js_undefined.t    = "O_TRUNC" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_APPEND : int Js_undefined.t   = "O_APPEND" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_NONBLOCK : int Js_undefined.t = "O_NONBLOCK" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_TEXT : int Js_undefined.t     = "O_TEXT" [@@bs.module "fs"] [@@bs.scope "constants"]
external fs_O_BINARY : int Js_undefined.t   = "O_BINARY" [@@bs.module "fs"] [@@bs.scope "constants"]

let fs_flag_of_open_flag : open_flag -> int Js_undefined.t = function
  | Open_rdonly -> fs_O_RDONLY
  | Open_wronly -> fs_O_WRONLY
  | Open_append -> fs_O_APPEND
  | Open_creat -> fs_O_CREAT
  | Open_trunc -> fs_O_TRUNC
  | Open_excl -> fs_O_EXCL
  | Open_binary -> fs_O_BINARY
  | Open_text -> fs_O_TEXT
  | Open_nonblock -> fs_O_NONBLOCK

let int_of_open_flag f =
  match Js_undefined.to_opt (fs_flag_of_open_flag f) with
  | Some i -> i
  | None -> 0

let int_of_open_flags : open_flag list -> int =
  let go : (open_flag -> int) -> open_flag list -> int = [%bs.raw {|
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
  |}] in
  go int_of_open_flag

external openSync : string -> int -> int -> int = "" [@@bs.module "fs"]

let caml_sys_open (file : string) (flags : open_flag list) (mode : int) : int =
  with_sys_error (fun () ->
      openSync file (int_of_open_flags flags) mode
    )


external writeSync :
  int       (* fd *)
  -> string (* string *)
  -> string (* encoding *)
  -> int    (* bytes written *)
  = "" [@@bs.module "fs"]

let caml_ml_open_descriptor_out (fd : int)  : Caml_io.out_channel =
  Caml_io.{
    fd = Some fd;
    buffer = "";
    output = (fun _ s ->
        with_sys_error (fun () ->
            let to_write = Bs_string.length s in
            let written = ref 0 in
            let rest = ref s in
            while !written < to_write do
              rest := Bs_string.slice_rest s !written;
              written := !written + writeSync fd !rest "binary"
            done
          )
      )
  }


external closeSync : int -> unit = "" [@@bs.module "fs"]

let caml_ml_close_channel (oc : Caml_io.out_channel) : unit =
  with_sys_error (fun () ->
      let open Caml_io in
      match oc.fd with
      | Some i ->
        closeSync i;
        oc.fd <- None
      | None ->
        ()
    )

