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



type +'a t (** Js object type *)
type +'a fn (** Js uncurried function *)

module Unsafe = struct 
  external (!)  : 'a t -> 'a = "js_unsafe_downgrade"

  external mk0 : (unit -> 'a0) -> 'a0 fn = 
    "js_fn_mk_00" 

  external run0 : 'a0 fn  -> 'a0 = "js_fn_run_00"

  external mk1 : ('a0 -> 'a1) -> ('a0 * 'a1) fn  = 
    "js_fn_mk_01"

  external run1 : ('a0 * 'a1) fn -> 'a0 -> 'a1  = 
    "js_fn_run_01"

  external mk2 : ('a0 -> 'a1 -> 'a2 ) -> ('a0 *  'a1 * 'a2) fn = 
    "js_fn_mk_02"

  external run2 : ('a0 * 'a1 * 'a2 )fn -> 'a0 -> 'a1 -> 'a2  = 
    "js_fn_run_02"

  external mk3 : 
    ('a0 -> 'a1 -> 'a2 -> 'a3 ) -> ('a0 *  'a1 * 'a2 * 'a3)  fn = 
    "js_fn_mk_03"

  external run3 : 
    ('a0 * 'a1 * 'a2 * 'a3) fn -> 'a0 -> 'a1 -> 'a2 -> 'a3 = 
    "js_fn_run_03"

  external mk4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) 
    -> ('a0 *  'a1 * 'a2 * 'a3 * 'a4) fn = 
    "js_fn_mk_04"

  external run4 : ('a0 *  'a1 * 'a2 * 'a3 * 'a4) fn -> 
    'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 = 
    "js_fn_run_04"

  external mk5 : 
    ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) ->
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5) fn =
    "js_fn_mk_05"

  external run5 : 
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5) fn
    -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 = 
    "js_fn_run_05"

  external mk6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> 
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) fn =
    "js_fn_mk_06"
  external run6 : ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) fn
    -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 = 
    "js_fn_run_06"

  external mk7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> 
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 ) fn =
    "js_fn_mk_07"

  external run7 : ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 ) fn
    -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 = 
    "js_fn_run_07"

  external mk8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) ->
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 ) fn =
    "js_fn_mk_08"

  external run8 :   
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 ) fn -> 
    'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 
    = 
    "js_fn_run_08"

  external mk9 : 
    ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) ->
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 ) fn =
    "js_fn_mk_09"

  external run9 : 
    ('a0 *  'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 ) fn -> 
    'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 = 
    "js_fn_run_09"

end 

(** This file will also be exported to external users 
    Attention: it should not have any code, all its code will be inlined so that 
    there will be never 
    {[ require('js')]}
*)
external typeof : 'a -> string = "js_typeof"

external to_json_string : 'a -> string = "js_json_stringify"

external log : 'a -> unit = "js_dump"




type 'a set = 'a -> unit 
(* TODO: in theory: it should be {[ 'a -> 'a]} *) 

type any = Obj.t

external erase : 'a -> any = "%identity" 
external cast : any -> 'a = "%identity" 

(* Note [to_opt null] will be [null : 'a opt opt]*)

module Null = struct 
  type + 'a t 
  external to_opt : 'a t -> 'a option = "js_from_nullable"
  external return : 'a -> 'a t  = "%identity"
  external test : 'a t -> bool = "js_is_nil"
  external empty : 'a t = "null" [@@bs.val]
end

module Def = struct 
  type + 'a t 
  external to_opt : 'a t -> 'a option = "js_from_def"
  external return : 'a -> 'a t = "%identity"
  external test : 'a t -> bool =  "js_is_undef"
  external empty : 'a t = "undefined" [@@bs.val]
end

module Null_def = struct
  type + 'a t 
  external to_opt : 'a t -> 'a option = "js_from_nullable_def"
  external return : 'a -> 'a t = "%identity"
  external test : 'a t -> bool =  "js_is_nil_undef"
  external empty : 'a t = "undefined" [@@bs.val]
end

type boolean 


external true_ : boolean = "true" [@@bs.val]
external false_ : boolean = "false" [@@bs.val]

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
      [@@bs.call]
  external toUpperCase : string -> string = "toUpperCase" [@@bs.send]
  external of_int : int -> base:int -> string = "toString" [@@bs.send]
  external of_nativeint : nativeint -> base:int -> string = "toString" [@@bs.send]
  external slice : string -> int -> int -> string = "slice" 
      [@@bs.send]
  external slice_rest : string -> int -> string = "slice" 
      [@@bs.send]
  external index_of : string -> string -> int = "indexOf"
      [@@bs.send]
  external append : string -> string -> string = "js_string_append"
  external of_small_int_array : int array -> string = "js_string_of_small_int_array"
  external of_small_int32_array : int32 array -> string = "js_string_of_small_int_array"
  external lastIndexOf : string -> string -> int = "lastIndexOf"
      [@@bs.send]    
  external of_any : 'a -> string = "js_anything_to_string"


  (***********************)
  (* replaced primitives *)
  external length : string -> int = "%string_length"
  external get : string -> int -> char = "%string_safe_get"
  external create : int -> bytes = "caml_create_string"
  external unsafe_get : string -> int -> char = "%string_unsafe_get"
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
    = "caml_blit_string" "noalloc"
  external unsafe_fill : bytes -> int -> int -> char -> unit
    = "caml_fill_string" "noalloc"

end

module Array = struct 
  external new_uninitialized : int -> 'a array = "js_create_array"
  external append : 'a array -> 'a array -> 'a array = "js_array_append"
  external make : int -> 'a -> 'a array = "caml_make_vect"
end
module Bytes = struct 
  external to_int_array : bytes -> int array = "%identity"
  external of_int_array : int array -> bytes = "%identity"
  external new_uninitialized : int -> bytes = "js_create_array" 

  (***********************)
  (* replaced primitives *)
  (* Note that we explicitly define [unsafe_set] instead of 
     using {!Bytes.unsafe_set} since for some standard libraries, 
     it might point to ["%string_unsafe_set"]
  *)
  external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
  external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
  external length : bytes -> int = "%bytes_length"
end
module Float = struct 
  external nan : float = "NaN"
    [@@bs.val ] 
  
  external to_fixed : float -> int -> string = "toFixed" 
    [@@bs.send]

  external is_finite : float -> bool = "isFinite"
    [@@bs.call ]

  external is_nan : float -> bool = "isNaN"
    [@@bs.call ] 

  external exp : float -> float = "Math.exp"
    [@@bs.call ]

  external log : float -> float = "Math.log"
    [@@bs.call ]
  
  external to_exponential : float -> prec:int ->  string = "toExponential"
    [@@bs.send]

  external log2 : float = "Math.LN2" [@@ bs.val ]  
  external max : float -> float -> float = "Math.max" 
    [@@bs.call]
  external random : unit -> float = "Math.random"
    [@@bs.call ]
  external of_any : 'a -> float = "js_anything_to_number"
end

module Caml_obj = struct 
  external set_tag : any -> int -> unit = "caml_obj_set_tag"
  external set_length : any -> int -> unit = "js_obj_set_length"
  external length : any -> int = "js_obj_length"
  external tag : any -> int = "caml_obj_tag"
  external set_tag : any -> int -> unit = "caml_obj_set_tag"
  external uninitialized_object : int -> int -> any = "js_uninitialized_object"
  external is_instance_array : any -> bool = 
    "js_is_instance_array" (* use Array.isArray instead*)
  external size_of_any : any -> 'a Def.t =
    "length" [@@bs.get]
  external tag_of_any : any -> 'a Def.t =
    "tag" [@@bs.get]
  external magic : 'a -> 'b = "%identity"
end

module Caml_int64 = struct
  external discard_sign : int64 -> int64 = "js_int64_discard_sign"
  external div_mod : int64 -> int64 -> int64 * int64 = "js_int64_div_mod"
  external to_hex : int64 -> string = "js_int64_to_hex"    
end  
