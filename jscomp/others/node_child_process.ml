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

(** Node Child Process API *)

type option

external option : ?cwd:string -> ?encoding:string -> unit -> option = "" [@@bs.obj]

(* TODO: when no option it would return buffer  *)
external execSync : string -> option -> string = "execSync" [@@bs.module "child_process"]

(*
  Note we have to make it abstract type, since if you declare it as
  `< pid : float > Js.t`, then you will create other external
  functions which will work with this type too, it is not what you want
*)
type spawnResult


external spawnSync : string -> spawnResult = "spawnSync" [@@bs.module "child_process"]

external readAs : spawnResult ->
  < pid : int ;
    status : int Js.null;
    signal : string Js.null ;
    stdout : Node.string_buffer Js.null ;
    stderr : Node.string_buffer Js.null >  =
  "%identity"
