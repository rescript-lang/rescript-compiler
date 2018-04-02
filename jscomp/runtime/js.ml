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

(** The Entry point to the JavaScript API

    This module will  be exported

    - It does not have any code, all its code will be inlined so that
       there will be never
       {[ require('js')]}

    - Its interface should be minimal

*)

(** {2 Internal types for FFI}

these types are not used by normal users
*)
module MapperRt = Js_mapperRt
module Internal = Js_internal

(* {2 Types for JS objects} *)

type +'a t
(** Js object type *)

type + 'a null
(** nullable, value of this type can be either [null] or ['a]
    this type is the same as [Js.Null.t] 
    See {!Js.Null}
 *)

type + 'a undefined
(** value of this type can be either [undefined] or ['a]
    this type is the same as {!Js.Undefined.t}  *)

type + 'a nullable    
type + 'a null_undefined = 'a nullable
(** value of this type can be [undefined], [null] or ['a]
    this type is the same as {!Js.Null_undefined.t}*)

external toOption : 'a nullable  -> 'a option = "#null_undefined_to_opt"
external undefinedToOption : 'a undefined -> 'a option = "#undefined_to_opt"
external nullToOption : 'a null -> 'a option = "#null_to_opt"
external test : 'a nullable -> bool = "#is_nil_undef"
external testAny : 'a -> bool = "#is_nil_undef"

type boolean = bool
(** The JS boolean type, can be [Js.true_] or [Js.false_] *)

(* I'd like to move this and the other types into a Js_core module that can be
   included back here, but the dependency hackery confuses me *)
type (+'a, +'e) promise
(** The promise type, defined here for interop 
    @deprecated Please use {!Js.Promise} instead
*)


(* tag::predefined_js_values[]*)
let true_ : boolean = true
let false_ : boolean = false
external null : 'a null = "#null"
(* The same as {!Js.Null.empty} will be compiled as [null]*)
external undefined : 'a undefined = "#undefined"
(* The same as  {!Js.Undefined.empty} will be compiled as [undefined]*)
(* end::predefined_js_values[]*)

(* tag::utility_functions[]*)
external to_bool : boolean -> bool = "%identity"
external typeof : 'a -> string = "#typeof"
(** [typeof x] will be compiled as [typeof x] in JS *)
external log : 'a -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log2 : 'a -> 'b -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log3 : 'a -> 'b -> 'c -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
external log4 : 'a -> 'b -> 'c -> 'd -> unit = "log" 
[@@bs.val] [@@bs.scope "console"]
(** A convenience function to log *)
external logMany : 'a array -> unit = "log"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]
(** A convenience function to log more than 4 arguments *)


external eqNull : 'a -> 'a null -> bool = "%bs_equal_null"
external eqUndefined : 'a -> 'a undefined -> bool = "%bs_equal_undefined"
external eqNullable : 'a -> 'a nullable -> bool = "%bs_equal_nullable" 

(** {4 operators }*)
external unsafe_lt : 'a -> 'a -> bool = "#unsafe_lt"
(**  [unsafe_lt a b] will be compiled using JS compare operator [a < b] *)
external unsafe_le : 'a -> 'a -> bool = "#unsafe_le"
(**  [unsafe_le a b] will be compiled using JS compare operator [a <= b] *)
external unsafe_gt : 'a -> 'a -> bool = "#unsafe_gt"
(**  [unsafe_gt a b] will be compiled using JS compare operator [a > b] *)
external unsafe_ge : 'a -> 'a -> bool = "#unsafe_ge"
(**  [unsafe_ge a b] will be compiled using JS compare operator [a >= b] *)
(* end::utility_functions[]*)

(* tag::nested_built_in_modules[] *)
(** {4 nested modules}*)

module Null = Js_null
module Undefined = Js_undefined
module Null_undefined = Js_null_undefined
module Nullable = Js_null_undefined
module Exn = Js_exn
(* end::nested_built_in_modules[] *)

(** {8 nested modules} *experimental* API, please refer to
  {! Js_dict}, {! Js_array}, {! Js_string} and {! Js_re} for more details *)

module Array = Js_array
module Boolean = Js_boolean
module Date = Js_date
module Dict = Js_dict
module Global = Js_global
module Json = Js_json
module Math = Js_math
module Obj  = Js_obj
module Re = Js_re
module String = Js_string
module Typed_array = Js_typed_array
module Types = Js_types
module Float = Js_float
module Int = Js_int
module Promise = Js_promise
module Option = Js_option
module Result = Js_result
module List = Js_list 
module Vector = Js_vector
module Console = Js_console
