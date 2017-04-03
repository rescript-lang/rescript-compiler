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




(** *)

let caml_raise_not_found () = raise Not_found

[%%bs.raw{|
function $$caml_sys_getenv(n) {
    //nodejs env
    if (typeof process !== 'undefined'
        && process.env
        && process.env[n] != undefined){
        return process.env[n]
    }
    else{ 
     caml_raise_not_found()
    };
  }
|}]

(** TODO: rewrite in OCaml *)
external caml_sys_getenv : string -> string = "$$caml_sys_getenv"
  [@@bs.val ] (* [@@bs.local] *)
(* TODO: improve [js_pass_scope] to avoid remove unused n here *)


[%%bs.raw{|
function $$date(){
  return (+new Date())
};

|}]

external current_date : unit -> float = "$$date"
  [@@bs.val ] (* [@@bs.local] *)

let caml_initial_time = current_date ()  *. 0.001

let caml_sys_time () = (current_date () *. 0.001) -. caml_initial_time

external random : unit -> float = "Math.random" [@@bs.val]

let caml_sys_random_seed () : nativeint array = 
   [|
     Nativeint.of_float 
     ((Nativeint.to_float (Nativeint.logxor (Nativeint.of_float (current_date ()))
                             0xffffffffn)) *. random ()) |]

let caml_sys_system_command () = 127

let caml_sys_getcwd () = "/"

let caml_sys_is_directory _s = 
  raise @@ Failure "caml_sys_is_directory not implemented"

let caml_sys_file_exists _s = 
  raise @@ Failure "caml_sys_file_exists not implemented"
