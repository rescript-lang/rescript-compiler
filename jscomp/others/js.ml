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

[@@@bs.config { flags = [| "-unboxed-types"; "-w"; "-49" |] }]
(* DESIGN:
   - It does not have any code, all its code will be inlined so that
       there will never be
   {[ require('js')]}
   - Its interface should be minimal
*)

(**
  The Js module mostly contains ReScript bindings to _standard JavaScript APIs_
  like [console.log](https://developer.mozilla.org/en-US/docs/Web/API/Console/log),
  or the JavaScript
  [String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String),
  [Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date), and
  [Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
  classes.

  It is meant as a zero-abstraction interop layer and directly exposes JavaScript functions as they are. If you can find your API in this module, prefer this over an equivalent Belt helper. For example, prefer [Js.Array2](js/array-2) over [Belt.Array](belt/array)

  ## Argument Order

  For historical reasons, some APIs in the Js namespace (e.g. [Js.String](js/string)) are
  using the data-last argument order whereas others (e.g. [Js.Date](js/date)) are using data-first.

  For more information about these argument orders and the trade-offs between them, see
  [this blog post](https://www.javierchavarri.com/data-first-and-data-last-a-comparison/).

  _Eventually, all modules in the Js namespace are going to be migrated to data-first though._

  In the meantime, there are several options for dealing with the data-last APIs:

  ```res example
  /* Js.String (data-last API used with pipe last operator) */
  Js.log("2019-11-10" |> Js.String.split("-"))
  Js.log("ReScript" |> Js.String.startsWith("Re"))

  /* Js.String (data-last API used with pipe first operator) */
  Js.log("2019-11-10"->Js.String.split("-", _))
  Js.log("ReScript"->Js.String.startsWith("Re", _))

  /* Js.String (data-last API used without any piping) */
  Js.log(Js.String.split("-", "2019-11-10"))
  Js.log(Js.String.startsWith("Re", "ReScript"))
  ```
  ## Js.Xxx2 Modules

  Prefer `Js.Array2` over `Js.Array`, `Js.String2` over `Js.String`, etc. The latters are old modules.
*)

type 'a t = < .. > as 'a
(** JS object type *)

module MapperRt = Js_mapperRt

module Internal = struct
  external opaqueFullApply : 'a -> 'a = "%uncurried_apply"

  (* Use opaque instead of [._n] to prevent some optimizations happening *)
  external run : (unit -> 'a [@bs]) -> 'a = "#run"
  external opaque : 'a -> 'a = "%opaque"
end

(**/**)

type +'a null
(**
  Nullable value of this type can be either null or 'a. This type is equivalent to Js.Null.t.
*)

type +'a undefined
(**
  A value of this type can be either undefined or 'a. This type is equivalent to Js.Undefined.t.
*)

type +'a nullable
(**
  A value of this type can be undefined, null or 'a. This type is equivalent to Js.Null_undefined.t.
*)

type +'a null_undefined = 'a nullable

external toOption : 'a nullable -> 'a option = "#nullable_to_opt"
external undefinedToOption : 'a undefined -> 'a option = "#undefined_to_opt"
external nullToOption : 'a null -> 'a option = "#null_to_opt"
external isNullable : 'a nullable -> bool = "#is_nullable"
external import : 'a -> 'a promise = "#import"

external testAny : 'a -> bool = "#is_nullable"
(** The same as {!test} except that it is more permissive on the types of input *)

type (+'a, +'e) promise
(**
  The promise type, defined here for interoperation across packages.
  @deprecated please use `Js.Promise`.
*)

external null : 'a null = "#null"
(**
  The same as empty in `Js.Null`. Compiles to `null`.
*)

external undefined : 'a undefined = "#undefined"
(**
  The same as empty `Js.Undefined`. Compiles to `undefined`.
*)

external typeof : 'a -> string = "#typeof"
(**
`typeof x` will be compiled as `typeof x` in JS. Please consider functions in
`Js.Types` for a type safe way of reflection.
*)

external log : 'a -> unit = "log"
  [@@val] [@@scope "console"]
(** Equivalent to console.log any value. *)

external log2 : 'a -> 'b -> unit = "log" [@@bs.val] [@@bs.scope "console"]
external log3 : 'a -> 'b -> 'c -> unit = "log" [@@bs.val] [@@bs.scope "console"]

external log4 : 'a -> 'b -> 'c -> 'd -> unit = "log"
  [@@bs.val] [@@bs.scope "console"]

external logMany : 'a array -> unit = "log"
  [@@bs.val] [@@bs.scope "console"] [@@bs.splice]
(** A convenience function to console.log more than 4 arguments *)

external eqNull : 'a -> 'a null -> bool = "%bs_equal_null"
external eqUndefined : 'a -> 'a undefined -> bool = "%bs_equal_undefined"
external eqNullable : 'a -> 'a nullable -> bool = "%bs_equal_nullable"

(** ## Operators *)

external unsafe_lt : 'a -> 'a -> bool = "#unsafe_lt"
(**
   `unsafe_lt(a, b)` will be compiled as `a < b`.
    It is marked as unsafe, since it is impossible
    to give a proper semantics for comparision which applies to any type
*)

external unsafe_le : 'a -> 'a -> bool = "#unsafe_le"
(**
   `unsafe_le(a, b) will be compiled as `a <= b`.
   See also `Js.unsafe_lt`.
*)

external unsafe_gt : 'a -> 'a -> bool = "#unsafe_gt"
(**
   `unsafe_gt(a, b)` will be compiled as `a > b`.
    See also `Js.unsafe_lt`.
*)

external unsafe_ge : 'a -> 'a -> bool = "#unsafe_ge"
(**
   `unsafe_ge(a, b)` will be compiled as `a >= b`.
   See also `Js.unsafe_lt`.
*)

(** ## Nested Modules *)

module Null = Js_null
(** Provide utilities for `Js.null<'a>` *)

module Undefined = Js_undefined
(** Provide utilities for `Js.undefined<'a>` *)

module Nullable = Js_null_undefined
(** Provide utilities for `Js.null_undefined` *)

module Null_undefined = Js_null_undefined
(** @deprecated please use `Js.Nullable` *)

module Exn = Js_exn
(** Provide utilities for dealing with Js exceptions *)

module Array = Js_array
(** Provide bindings to JS array*)

module Array2 = Js_array2
(** Provide bindings to JS array*)

module String = Js_string
(** Provide bindings to JS string *)

module String2 = Js_string2
(** Provide bindings to JS string *)

module Re = Js_re
(** Provide bindings to JS regex expression *)

module Promise = Js_promise
(** Provide bindings to JS Promise *)

module Promise2 = Js_promise2
(** Provide bindings to JS Promise *)

module Date = Js_date
(** Provide bindings for JS Date *)

module Dict = Js_dict
(** Provide utilities for JS dictionary object *)

module Global = Js_global
(** Provide bindings to JS global functions in global namespace*)

module Json = Js_json
(** Provide utilities for json *)

module Math = Js_math
(** Provide bindings for JS `Math` object *)

module Obj = Js_obj
(** Provide utilities for `Js.t` *)

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

module Bigint = Js_bigint
(** Provide utilities for bigint *)

module Option = Js_option
(** Provide utilities for option *)

module Result = Js_result
(** Define the interface for result *)

module List = Js_list
(** Provide utilities for list *)

module Vector = Js_vector
(** Provides bindings for JS Vector *)

module Console = Js_console
(** Provides bindings for console *)

module Set = Js_set
(** Provides bindings for ES6 Set *)

module WeakSet = Js_weakset
(** Provides bindings for ES6 WeakSet *)

module Map = Js_map
(** Provides bindings for ES6 Map *)

module WeakMap = Js_weakmap
(** Provides bindings for ES6 WeakMap *)
