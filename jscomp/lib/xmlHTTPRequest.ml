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
open Js

type readyState = 
  | UNSENT 
  | OPENED 
  | HEADERS_RECEIVED 
  | LOADING
  | DONE

class type xmlHttpRequest = object 
  method onreadystatechange__w : (unit -> unit) -> unit 
  (* To make it type safe. we need mark 
     it expclitly [(unit ->unit) Js.callback].
     But this is only type safe in theory, since 
     pass functions to js side, there is no assumptions 
     that you can make about those js code.
  *)
  method readyState__r : readyState 
  method open__3 : string -> string -> boolean -> unit 
  method open_full__3 : 
    string -> string -> boolean -> unit
  method open_full__4 :
    string -> string -> boolean -> string  -> unit
  method open_full__5 :
    string -> string -> boolean -> string  -> string -> unit
  (* In the ppx 
     we can write 
     {[
       method open_full : 
         string -> string -> boolean -> string [@opt] -> string [@opt] -> unit 
     ]}
  *)
  method setRequestHeader : string -> string -> unit
  method overrideMimeType : string -> unit 
  method send : string opt -> unit 
  method abort__0 : unit
  method status__r : int
  method statusText__r : string 
  method getResponseHeader__1 : string -> string opt
  method getAllResponseHeaders__0 : string 
  method responseText__r : string 
  method responseType__r : string 
end
