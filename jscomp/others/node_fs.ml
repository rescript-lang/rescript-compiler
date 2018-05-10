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

(** Node FS API

@see <https://blogs.janestreet.com/a-and-a/> refernce documentation
*)

(** Most fs functions let you omit the callback argument. If you do, a default
    callback is used that rethrows errors. To get a trace to the original call
    site, set the `NODE_DEBUG` environment variable:
*)
external readdirSync : string -> string array  = "" [@@bs.module "fs"]    

external renameSync : string -> string = "" [@@bs.module "fs"]    

type fd = private int 

type path = string
(**
   The relative path to a filename can be used. Remember, however, that this path
   will be relative to [process.cwd()].
*)  

module Watch = struct 
  type t
  type config 
  external config :
    ?persistent: bool ->
    ?recursive: bool -> 
    ?encoding: Js_string.t -> 
    unit -> config = 
    "" [@@bs.obj]

  external watch :
    string -> 
    ?config:config -> 
    unit -> 
    t = "" 
  [@@bs.module "fs"]
  (** there is no need to accept listener, since we return a [watcher] 
      back it can register event listener there .
      Currently we introduce a type [string_buffer], for the 
      [filename], it will be [Buffer] when the encoding is `utf8. 
      This is dependent type which can be tracked by GADT in some way, 
      but to make things simple, let's just introduce an or type
  *)
  external on :
    ([
      `change of (string (*eventType*) -> Node.string_buffer (* filename *) -> unit  [@bs])
    | `error of (unit -> unit [@bs])
    ] [@bs.string]
    ) -> t = "" 
  [@@bs.send.pipe: t]


  external close :  unit = "" [@@bs.send.pipe:t]
end

external ftruncateSync : fd -> int -> unit = "" [@@bs.module "fs"]    

external truncateSync : string -> int -> unit = "" [@@bs.module "fs"]    

external chownSync : string -> uid:int -> gid:int -> unit = "" [@@bs.module "fs"]    

external fchownSync : fd -> uid:int -> gid:int -> unit = "" [@@bs.module "fs"]    

external readlinkSync : string -> string  = "" [@@bs.module "fs"]

external unlinkSync : string -> unit  = "" [@@bs.module "fs"]

external rmdirSync : string -> unit = ""  [@@bs.module "fs"]

(* TODO: [flags] support *)
external openSync : 
  path ->
  (
    [ `Read [@bs.as "r"]
    | `Read_write [@bs.as "r+"]
    | `Read_write_sync [@bs.as "rs+"]
    | `Write [@bs.as "w"]
    | `Write_fail_if_exists [@bs.as "wx"]
    | `Write_read [@bs.as "w+"]
    | `Write_read_fail_if_exists [@bs.as "wx+"]
    | `Append [@bs.as "a"]
    | `Append_fail_if_exists [@bs.as "ax"]
    | `Append_read [@bs.as "a+"]
    | `Append_read_fail_if_exists [@bs.as "ax+"]
    ] [@bs.string]) ->
  unit = ""  [@@bs.module "fs"]


external readFileSync : 
  string ->
  ( 
    [
      `hex
    | `utf8
    | `ascii 
    | `latin1
    | `base64 
    | `ucs2
    | `base64
    | `binary 
    | `utf16le ][@bs.string]) ->
  string = "readFileSync" 
[@@bs.val] [@@bs.module "fs"]

external readFileAsUtf8Sync : 
  string -> (_[@bs.as "utf8"]) -> 
  string = "readFileSync"
[@@bs.val] [@@bs.module "fs"]  

external existsSync : string -> bool = ""
[@@bs.val] [@@bs.module "fs"]  

external writeFileSync :
  string ->
  string ->
  (
    [ `hex
    | `utf8
    | `ascii
    | `latin1
    | `base64
    | `ucs2
    | `base64
    | `binary
    | `utf16le ][@bs.string]) ->
  unit = "writeFileSync"
[@@bs.val] [@@bs.module "fs"]

external writeFileAsUtf8Sync :
  string ->
  string ->
  (_[@bs.as "utf8"]) ->
  unit = "writeFileSync"
[@@bs.val] [@@bs.module "fs"]
