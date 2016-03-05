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


external typeof : 'a -> string = "js_typeof"

external to_json_string : 'a -> string = "js_json_stringify"

external log : 'a -> unit = "js_dump"

external anything_to_string : 'a -> string = "js_anything_to_string"

external unsafe_js_expr : string -> 'a = "js_pure_expr"
external unsafe_js_stmt : string -> 'a = "js_pure_stmt"

type + 'a opt

external from_opt : 'a opt -> 'a option = "js_from_nullable"
external to_opt : 'a -> 'a opt  = "%identity"




external is_nil : 'a opt -> bool = "js_is_nil"

external nil : 'a opt = "null" [@@js.global]

(* Note [to_opt null] will be [null : 'a opt opt]*)


type + 'a def
external from_def : 'a def -> 'a option = "js_from_def"
external to_def : 'a -> 'a def = "%identity"
external is_undef : 'a def -> bool =  "js_is_undef"
external undef : 'a def = "undefined" [@@js.global]

type boolean 

(* let true_ : boolean = unsafe_js_expr "true" *)
external true_ : boolean = "true" [@@js.global]

external false_ : boolean = "false" [@@js.global]

external to_bool : boolean -> bool = "js_boolean_to_bool" 
