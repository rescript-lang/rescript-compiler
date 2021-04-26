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


type t =
  < argv : string array;
    arch : string ;
    abort : unit -> unit [@bs.meth];
    chdir : string -> unit [@bs.meth];
    cwd : unit -> string [@bs.meth];
    disconnect : unit -> unit [@bs.meth];
    platform : string;
    env : string Js_dict.t; (* ocamldep sucks which can not map `Js.Dic.t` to `Js_dict.t`*)
  >
(* FIXME : use record *)

external process : t = "process" [@@bs.module]
external argv : string array = "argv" [@@bs.module "process"]
external exit : int -> 'a = "exit" [@@bs.module "process"]
external cwd : unit -> string = "cwd" [@@bs.module "process"]

(** The process.uptime() method returns the number of seconds
    the current Node.js process has been running.) *)
external uptime : t -> unit -> float = "uptime" [@@bs.send]

val putEnvVar : string -> string -> unit
val deleteEnvVar : string -> unit
