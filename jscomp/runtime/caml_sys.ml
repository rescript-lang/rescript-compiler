(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)
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

external caml_sys_getenv : string -> string = "$$caml_sys_getenv"
  [@@bs.call ] [@@bs.local]
(* TODO: improve [js_pass_scope] to avoid remove unused n here *)

[%%bs.raw{|
function $$date(){
  return (+new Date())
};

|}]

external current_date : unit -> float = "$$date"
  [@@bs.call ] [@@bs.local]

let caml_initial_time = current_date ()  *. 0.001

let caml_sys_time () = (current_date () -. caml_initial_time ) *. 0.001

let caml_sys_random_seed () : nativeint array = 
   [|
     Nativeint.of_float 
     ((Nativeint.to_float (Nativeint.logxor (Nativeint.of_float (current_date ()))
                             0xffffffffn)) *. Js.Float.random ()) |]

let caml_sys_system_command () = 127

let caml_sys_getcwd () = "/"

let caml_sys_is_directory _s = 
  raise @@ Failure "caml_sys_is_directory not implemented"
