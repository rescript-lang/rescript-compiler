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

[@@@bs.config {flags = [|"-unboxed-types";"-w" ;"-49"|]}]
(* DESIGN:
   - It does not have any code, all its code will be inlined so that
       there will never be
   {[ require('js')]}
   - Its interface should be minimal
*)

(** This library provides bindings and necessary support for JS FFI.
    It contains all bindings into [Js] namespace.

    @example {[
      [| 1;2;3;4|]
      |. Js.Array2.map (fun x -> x + 1 )
      |. Js.Array2.reduce (+) 0
      |. Js.log
    ]}
*)

(**/**)
(** Types for JS objects *)
type 'a t = < .. > as 'a
(**/**)

(* internal types for FFI, these types are not used by normal users 
    Absent cmi file when looking up module alias.
*)
module Fn = struct
  type 'a arity0 = {
    i0 : unit -> 'a [@internal]  
  }
  type 'a arity1 = {
    i1 : 'a [@internal]
  }
  type 'a arity2 = {
    i2 : 'a [@internal]
  }
  type 'a arity3 = {
    i3 : 'a [@internal]
  }
  type 'a arity4 = {
    i4 : 'a [@internal]
  }
  type 'a arity5 = {
    i5 : 'a [@internal]
  }
  type 'a arity6 = {
    i6 : 'a [@internal]
  }
  type 'a arity7 = {
    i7 : 'a [@internal]
  }
  type 'a arity8 = {
    i8 : 'a [@internal]
  }
  type 'a arity9 = {
    i9 : 'a [@internal]
  }
  type 'a arity10 = {
    i10 : 'a [@internal]
  }
  type 'a arity11 = {
    i11 : 'a [@internal]
  }
  type 'a arity12 = {
    i12 : 'a [@internal]
  }
  type 'a arity13 = {
    i13 : 'a [@internal]
  }
  type 'a arity14 = {
    i14 : 'a [@internal]
  }
  type 'a arity15 = {
    i15 : 'a [@internal]
  }
  type 'a arity16 = {
    i16 : 'a [@internal]
  }
  type 'a arity17 = {
    i17 : 'a [@internal]
  }
  type 'a arity18 = {
    i18 : 'a [@internal]
  }
  type 'a arity19 = {
    i19 : 'a [@internal]
  }
  type 'a arity20 = {
    i20 : 'a [@internal]
  }
  type 'a arity21 = {
    i21 : 'a [@internal]
  }
  type 'a arity22 = {
    i22 : 'a [@internal]
  }
end

(**/**)
module MapperRt = Js_mapperRt
module Internal = struct 
  open Fn    
  external opaqueFullApply : 'a -> 'a = "#full_apply"

  (* Use opaque instead of [._n] to prevent some optimizations happening *)
  external run : 'a arity0 -> 'a = "#run" 
  external opaque : 'a -> 'a = "%opaque"

end    
(**/**)


type + 'a null
(** nullable, value of this type can be either [null] or ['a]
    this type is the same as type [t] in {!Null}
*)

type + 'a undefined
(** value of this type can be either [undefined] or ['a]
    this type is the same as type [t] in {!Undefined}  *)

type + 'a nullable
(** value of this type can be [undefined], [null] or ['a]
    this type is the same as type [t] n {!Null_undefined} *)

type + 'a null_undefined = 'a nullable

external toOption : 'a nullable  -> 'a option = "#nullable_to_opt"
external undefinedToOption : 'a undefined -> 'a option = "#undefined_to_opt"
external nullToOption : 'a null -> 'a option = "#null_to_opt"

external isNullable : 'a nullable -> bool = "#is_nullable"

(** The same as {!test} except that it is more permissive on the types of input *)
external testAny : 'a -> bool = "#is_nullable"


type (+'a, +'e) promise
(** The promise type, defined here for interoperation across packages
    @deprecated please use {!Js.Promise}
*)

external null : 'a null = "#null"
(** The same as [empty] in {!Js.Null} will be compiled as [null]*)

external undefined : 'a undefined = "#undefined"
(** The same as  [empty] {!Js.Undefined} will be compiled as [undefined]*)



external typeof : 'a -> string = "#typeof"
(** [typeof x] will be compiled as [typeof x] in JS
    Please consider functions in {!Types} for a type safe way of reflection
*)

external log : 'a -> unit = "log"
[@@bs.val] [@@bs.scope "console"]
(** A convenience function to log everything *)

external log2 : 'a -> 'b -> unit = "log"
[@@bs.val] [@@bs.scope "console"]
external log3 : 'a -> 'b -> 'c -> unit = "log"
[@@bs.val] [@@bs.scope "console"]
external log4 : 'a -> 'b -> 'c -> 'd -> unit = "log"
[@@bs.val] [@@bs.scope "console"]

external logMany : 'a array -> unit = "log"
[@@bs.val] [@@bs.scope "console"] [@@bs.splice]
(** A convenience function to log more than 4 arguments *)

external eqNull : 'a -> 'a null -> bool = "%bs_equal_null"
external eqUndefined : 'a -> 'a undefined -> bool = "%bs_equal_undefined"
external eqNullable : 'a -> 'a nullable -> bool = "%bs_equal_nullable"

(** {4 operators }*)

external unsafe_lt : 'a -> 'a -> bool = "#unsafe_lt"
(** [unsafe_lt a b] will be compiled as [a < b].
    It is marked as unsafe, since it is impossible
    to give a proper semantics for comparision which applies to any type
*)


external unsafe_le : 'a -> 'a -> bool = "#unsafe_le"
(**  [unsafe_le a b] will be compiled as [a <= b].
     See also {!unsafe_lt}
*)


external unsafe_gt : 'a -> 'a -> bool = "#unsafe_gt"
(**  [unsafe_gt a b] will be compiled as [a > b].
     See also {!unsafe_lt}
*)

external unsafe_ge : 'a -> 'a -> bool = "#unsafe_ge"
(**  [unsafe_ge a b] will be compiled as [a >= b].
     See also {!unsafe_lt}
*)


(** {12 nested modules}*)

module Null = Js_null
(** Provide utilities around ['a null] *)

module Undefined = Js_undefined
(** Provide utilities around {!undefined} *)

module Nullable = Js_null_undefined
(** Provide utilities around {!null_undefined} *)

module Null_undefined = Js_null_undefined
(** @deprecated please use {!Js.Nullable} *)

module Exn = Js_exn
(** Provide utilities for dealing with Js exceptions *)

module Array = Js_array
(** Provide bindings to Js array*)

module Array2 = Js_array2
(** Provide bindings to Js array*)

module String = Js_string
(** Provide bindings to JS string *)

module String2 = Js_string2
(** Provide bindings to JS string *)

module Re = Js_re
(** Provide bindings to Js regex expression *)

module Promise = Js_promise
(** Provide bindings to JS promise *)

module Date = Js_date
(** Provide bindings for JS Date *)

module Dict = Js_dict
(** Provide utilities for JS dictionary object *)

module Global = Js_global
(** Provide bindings to JS global functions in global namespace*)

module Json = Js_json
(** Provide utilities for json *)

module Math = Js_math
(** Provide bindings for JS [Math] object *)

module Obj  = Js_obj
(** Provide utilities for {!Js.t} *)

module Typed_array = Js_typed_array
(** Provide bindings for JS typed array *)

module TypedArray2 = Js_typed_array2
(** Provide bindings for JS typed array *)

module Types = Js_types
(** Provide utilities for manipulating JS types  *)

module Float = Js_float
(** Provide utilities for JS float *)

module Int = Js_int
(** Provide utilities for int *)

module Option = Js_option
(** Provide utilities for option *)

module Result = Js_result
(** Define the interface for result *)

module List = Js_list
(** Provide utilities for list *)

module Vector = Js_vector

module Console = Js_console
