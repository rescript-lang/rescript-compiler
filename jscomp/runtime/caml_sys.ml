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





external getEnv : 'a -> string -> string option = "" [@@bs.get_index] 
let caml_sys_getenv s =
  if Js.typeof [%raw{|process|}] = "undefined"
     || [%raw{|process.env|}] = Caml_undefined_extern.empty 
  then raise Not_found
  else  
    match getEnv [%raw{|process.env|}] s with 
    | None -> raise Not_found
    | Some x -> x 



(* TODO: improve [js_pass_scope] to avoid remove unused n here *)

external now : unit -> float = "" [@@bs.val "Date.now"]


(* let caml_initial_time = now ()  *. 0.001 *)

type process
external uptime : process -> unit -> float = "" [@@bs.send]
external exit : process -> int -> 'a =  ""  [@@bs.send]

let caml_sys_time () =
  if Js.typeof [%raw{|process|}] = "undefined" 
    || [%raw{|process.uptime|}] = Caml_undefined_extern.empty
  then -1.
  else uptime [%raw{|process|}] ()
  

external random : unit -> float = "Math.random" [@@bs.val]

let caml_sys_random_seed () : nativeint array = 
   [|
     Caml_nativeint.of_float 
     ((Caml_nativeint.to_float (Caml_nativeint.logxor (Caml_nativeint.of_float (now ()))
                             0xffffffffn)) *. random ()) |]

type spawnResult

external spawnSync : string -> spawnResult = "" [@@bs.module "child_process"]

external readAs : spawnResult -> 
  < 
    status : int Js.null;
  > Js.t = 
  "%identity"



let caml_sys_system_command _cmd = 127

let caml_sys_getcwd () = 
  if Js.typeof [%raw{|process|}] = "undefined" then "/"
  else [%raw{|process|}]##cwd ()  


(* Called by {!Sys} in the toplevel, should never fail*)
let caml_sys_get_argv () : string * string array = 
  if Js.typeof [%raw{|process|}] = "undefined" then "",[|""|] 
  else 
    let argv = [%raw{|process.argv|}] in 
    if Js.testAny argv then ("",[|""|])
    else Caml_array.unsafe_get argv 0, argv

(** {!Pervasives.sys_exit} *)
let caml_sys_exit :int -> 'a = fun exit_code -> 
  if Js.typeof [%raw{|process|}] <> "undefined"  then 
    exit [%raw{|process|}] exit_code 



let caml_sys_is_directory _s = 
  raise (Failure "caml_sys_is_directory not implemented")

(** Need polyfill to make cmdliner work 
    {!Sys.is_directory} or {!Sys.file_exists} {!Sys.command} 
*)
let caml_sys_file_exists _s = 
  raise ( Failure "caml_sys_file_exists not implemented")
