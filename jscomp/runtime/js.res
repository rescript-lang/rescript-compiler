/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

@@config({flags: ["-unboxed-types", "-w", "-49"]})
@@ocaml.text(/* DESIGN:
   - It does not have any code, all its code will be inlined so that
       there will never be
   {[ require('js')]}
   - Its interface should be minimal
*/

" This library provides bindings and necessary support for JS FFI.
    It contains all bindings into [Js] namespace.

    @example {[
      [| 1;2;3;4|]
      |. Js.Array2.map (fun x -> x + 1 )
      |. Js.Array2.reduce (+) 0
      |. Js.log
    ]}
")

@@ocaml.text("/*")

@ocaml.doc(" Types for JS objects ")
type t<'a> = {..} as 'a

module MapperRt = Js_mapperRt
module Internal = {
  external opaqueFullApply: 'a => 'a = "%uncurried_apply"

  /* Use opaque instead of [._n] to prevent some optimizations happening */
  external run: ((. unit) => 'a) => 'a = "#run"
  external opaque: 'a => 'a = "%opaque"
}

@@ocaml.text("/*")

@ocaml.doc(" nullable, value of this type can be either [null] or ['a]
    this type is the same as type [t] in {!Null}
")
@unboxed
type null<+'a> = Value('a) | @as(null) Null

@ocaml.doc(" value of this type can be either [undefined] or ['a]
    this type is the same as type [t] in {!Undefined}  ")
type undefined<+'a>

@ocaml.doc(" value of this type can be [undefined], [null] or ['a]
    this type is the same as type [t] n {!Null_undefined} ")
@unboxed
type nullable<+'a> = Value('a) | @as(null) Null | @as(undefined) Undefined

type null_undefined<+'a> = nullable<'a>

external toOption: nullable<'a> => option<'a> = "#nullable_to_opt"
external undefinedToOption: undefined<'a> => option<'a> = "#undefined_to_opt"
external nullToOption: null<'a> => option<'a> = "#null_to_opt"

external isNullable: nullable<'a> => bool = "#is_nullable"

external import: 'a => promise<'a> = "#import"

@ocaml.doc(" The same as {!test} except that it is more permissive on the types of input ")
external testAny: 'a => bool = "#is_nullable"

@ocaml.doc(" The promise type, defined here for interoperation across packages
    @deprecated please use {!Js.Promise}
")
type promise<+'a, +'e>

@ocaml.doc(" The same as [empty] in {!Js.Null} will be compiled as [null]")
external null: null<'a> = "#null"

@ocaml.doc(" The same as  [empty] {!Js.Undefined} will be compiled as [undefined]")
external undefined: undefined<'a> = "#undefined"

@ocaml.doc(" [typeof x] will be compiled as [typeof x] in JS
    Please consider functions in {!Types} for a type safe way of reflection
")
external typeof: 'a => string = "#typeof"

@val @scope("console") @ocaml.doc(" A convenience function to log everything ")
external log: 'a => unit = "log"

@val @scope("console") external log2: ('a, 'b) => unit = "log"
@val @scope("console") external log3: ('a, 'b, 'c) => unit = "log"
@val @scope("console") external log4: ('a, 'b, 'c, 'd) => unit = "log"

@val @scope("console") @variadic @ocaml.doc(" A convenience function to log more than 4 arguments ")
external logMany: array<'a> => unit = "log"

external eqNull: ('a, null<'a>) => bool = "%bs_equal_null"
external eqUndefined: ('a, undefined<'a>) => bool = "%bs_equal_undefined"
external eqNullable: ('a, nullable<'a>) => bool = "%bs_equal_nullable"

@@ocaml.text(" {4 operators }")

@ocaml.doc(" [unsafe_lt a b] will be compiled as [a < b].
    It is marked as unsafe, since it is impossible
    to give a proper semantics for comparision which applies to any type
")
external unsafe_lt: ('a, 'a) => bool = "#unsafe_lt"

@ocaml.doc("  [unsafe_le a b] will be compiled as [a <= b].
     See also {!unsafe_lt}
")
external unsafe_le: ('a, 'a) => bool = "#unsafe_le"

@ocaml.doc("  [unsafe_gt a b] will be compiled as [a > b].
     See also {!unsafe_lt}
")
external unsafe_gt: ('a, 'a) => bool = "#unsafe_gt"

@ocaml.doc("  [unsafe_ge a b] will be compiled as [a >= b].
     See also {!unsafe_lt}
")
external unsafe_ge: ('a, 'a) => bool = "#unsafe_ge"

@@ocaml.text(" {12 nested modules}")

@ocaml.doc(" Provide utilities around ['a null] ")
module Null = Js_null

@ocaml.doc(" Provide utilities around {!undefined} ")
module Undefined = Js_undefined

@ocaml.doc(" Provide utilities around {!null_undefined} ")
module Nullable = Js_null_undefined

@ocaml.doc(" @deprecated please use {!Js.Nullable} ")
module Null_undefined = Js_null_undefined

@ocaml.doc(" Provide utilities for dealing with Js exceptions ")
module Exn = Js_exn

@ocaml.doc(" Provide bindings to Js array")
module Array = Js_array

@ocaml.doc(" Provide bindings to Js array")
module Array2 = Js_array2

@ocaml.doc(" Provide bindings to JS string ")
module String = Js_string

@ocaml.doc(" Provide bindings to JS string ")
module String2 = Js_string2

@ocaml.doc(" Provide bindings to Js regex expression ")
module Re = Js_re

@ocaml.doc(" Provide bindings to JS promise ")
module Promise = Js_promise

@ocaml.doc(" Provide bindings to JS promise ")
module Promise2 = Js_promise2

@ocaml.doc(" Provide bindings for JS Date ")
module Date = Js_date

@ocaml.doc(" Provide utilities for JS dictionary object ")
module Dict = Js_dict

@ocaml.doc(" Provide bindings to JS global functions in global namespace")
module Global = Js_global

@ocaml.doc(" Provide utilities for json ")
module Json = Js_json

@ocaml.doc(" Provide bindings for JS [Math] object ")
module Math = Js_math

@ocaml.doc(" Provide utilities for {!Js.t} ")
module Obj = Js_obj

@ocaml.doc(" Provide bindings for JS typed array ")
module Typed_array = Js_typed_array

@ocaml.doc(" Provide bindings for JS typed array ")
module TypedArray2 = Js_typed_array2

@ocaml.doc(" Provide utilities for manipulating JS types  ")
module Types = Js_types

@ocaml.doc(" Provide utilities for JS float ")
module Float = Js_float

@ocaml.doc(" Provide utilities for int ")
module Int = Js_int

@ocaml.doc(" Provide utilities for bigint ")
module BigInt = Js_bigint

@ocaml.doc(" Provide utilities for File ")
module File = Js_file

@ocaml.doc(" Provide utilities for Blob ")
module Blob = Js_blob

@ocaml.doc(" Provide utilities for option ")
module Option = Js_option

@ocaml.doc(" Define the interface for result ")
module Result = Js_result

@ocaml.doc(" Provide utilities for list ")
module List = Js_list

module Vector = Js_vector

module Console = Js_console

@ocaml.doc(" Provides bindings for ES6 Set ")
module Set = Js_set

@ocaml.doc(" Provides bindings for ES6 WeakSet ")
module WeakSet = Js_weakset

@ocaml.doc(" Provides bindings for ES6 Map ")
module Map = Js_map

@ocaml.doc(" Provides bindings for ES6 WeakMap ")
module WeakMap = Js_weakmap
