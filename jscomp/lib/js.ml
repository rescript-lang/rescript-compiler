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

(** This file will also be exported to external users 
    Attention: it should not have any code, all its code will be inlined so that 
    there will be never 
    {[ require('js')]}
*)
external typeof : 'a -> string = "js_typeof"

external to_json_string : 'a -> string = "js_json_stringify"

external log : 'a -> unit = "js_dump"

external anything_to_string : 'a -> string = "js_anything_to_string"
external anything_to_number : 'a -> float = "js_anything_to_number"



type + 'a opt

external from_opt : 'a opt -> 'a option = "js_from_nullable"
external to_opt : 'a -> 'a opt  = "%identity"




external is_nil : 'a opt -> bool = "js_is_nil"

external nil : 'a opt = "null" [@@js.val]

(* Note [to_opt null] will be [null : 'a opt opt]*)


type + 'a def
external from_def : 'a def -> 'a option = "js_from_def"
external to_def : 'a -> 'a def = "%identity"
external is_undef : 'a def -> bool =  "js_is_undef"
external undef : 'a def = "undefined" [@@js.val]

type boolean 

(* let true_ : boolean = unsafe_js_expr "true" *)
external true_ : boolean = "true" [@@js.val]

external false_ : boolean = "false" [@@js.val]

external to_bool : boolean -> bool = "js_boolean_to_bool" 
external to_number : 'a -> int = "js_anything_to_number" (* + conversion*)
external string_of_nativeint : nativeint -> string = "js_anything_to_string"
external string_of_char : char -> string = "js_string_of_char" 
(** TODO: check with {!String.of_char} 
    it's quite common that we have
    {[ Js.String.of_char x.[0] ]}
    It would be nice to generate code as below    
    {[ x[0]
    ]}
*)
module String = struct 
  external of_char : char -> string = "String.fromCharCode" 
      [@@js.call]
  external toUpperCase : string -> string = "toUpperCase" [@@js.send]
  external of_int : int -> base:int -> string = "toString" [@@js.send]
  external of_nativeint : nativeint -> base:int -> string = "toString" [@@js.send]
  external slice : string -> int -> int -> string = "slice" 
      [@@js.send]
  external slice_rest : string -> int -> string = "slice" 
      [@@js.send]
  external index_of : string -> string -> int = "indexOf"
      [@@js.send]
  external append : string -> string -> string = "js_string_append"
  external of_small_int_array : int array -> string = "js_string_of_small_int_array"
  external lastIndexOf : string -> string -> int = "lastIndexOf"
      [@@js.send]    
end

module Array = struct 
  external new_uninitialized : int -> 'a array = "js_create_array"
  external append : 'a array -> 'a array -> 'a array = "js_array_append"

end
module Bytes = struct 
  external to_int_array : bytes -> int array = "%identity"
  external of_int_array : int array -> bytes = "%identity"
  external new_uninitialized : int -> bytes = "js_create_array" 

end
module Float = struct 
  external nan : float = "NaN"
    [@@js.val ] 
  
  external to_fixed : float -> int -> string = "toFixed" 
    [@@js.send]

  external is_finite : float -> bool = "isFinite"
    [@@js.call ]

  external is_nan : float -> bool = "isNaN"
    [@@js.call ] 

  external exp : float -> float = "Math.exp"
    [@@js.call ]

  external log : float -> float = "Math.log"
    [@@js.call ]
  
  external to_exponential : float -> prec:int ->  string = "toExponential"
    [@@js.send]

  external log2 : float = "Math.LN2" [@@ js.val ]  
  external max : float -> float -> float = "Math.max" 
    [@@js.call]
  external random : unit -> float = "Math.random"
    [@@js.call ]

end

module Caml_obj = struct 
  external set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"
  external set_length : Obj.t -> int -> unit = "js_obj_set_length"
  external length : Obj.t -> int = "js_obj_length"
  external tag : Obj.t -> int = "caml_obj_tag"
  external set_tag : Obj.t -> int -> unit = "caml_obj_set_tag"
  external uninitialized_object : int -> int -> Obj.t = "js_uninitialized_object"
  external is_instance_array : Obj.t -> bool = "js_is_instance_array" (* use Array.isArray instead*)
end

module Caml_int64 = struct
  external discard_sign : int64 -> int64 = "js_int64_discard_sign"
  external div_mod : int64 -> int64 -> int64 * int64 = "js_int64_div_mod"
  external to_hex : int64 -> string = "js_int64_to_hex"    
end  
