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

/* DESIGN:
   - It does not have any code, all its code will be inlined so that
       there will never be
   {[ require('js')]}
   - Its interface should be minimal
*/

/** 
 This library provides bindings and necessary support for JS FFI.
    It contains all bindings into [Js] namespace.

    @example {[
      [| 1;2;3;4|]
      |. Js.Array2.map (fun x -> x + 1 )
      |. Js.Array2.reduce (+) 0
      |. Js.log
    ]}
*/

type t<'a> = {..} as 'a
/** Types for JS objects */

module MapperRt = Js_mapperRt
module Internal = {
  external opaqueFullApply : 'a => 'a = "%uncurried_apply"

  /* Use opaque instead of [._n] to prevent some optimizations happening */
  external run : ((unit => 'a)) => 'a = "#run"
  external opaque : 'a => 'a = "%opaque"
}


/** nullable, value of this type can be either [null] or ['a]
    this type is the same as type [t] in {!Null}
*/
@unboxed
type null<+'a> = Value('a) | @as(null) Null

type undefined<+'a>
/** value of this type can be either [undefined] or ['a]
    this type is the same as type [t] in {!Undefined}  */

/** value of this type can be [undefined], [null] or ['a]
    this type is the same as type [t] n {!Null_undefined} */
@unboxed
type nullable<+'a>  = Value('a) | @as(null) Null| @as(undefined) Undefined 

type null_undefined<+'a>  = nullable<'a> 

external toOption: nullable<'a> => option<'a> = "#nullable_to_opt"
external undefinedToOption: undefined<'a> => option<'a> = "#undefined_to_opt"
external nullToOption: null<'a> => option<'a> = "#null_to_opt"

external isNullable: nullable<'a> => bool = "#is_nullable"

external import: 'a => promise<'a> = "#import"

external testAny: 'a => bool = "#is_nullable"
/** The same as {!test} except that it is more permissive on the types of input */

type promise<+'a, +'e>
/** The promise type, defined here for interoperation across packages
    @deprecated please use {!Js.Promise}
*/

external null: null<'a> = "#null"
/** The same as [empty] in {!Js.Null} will be compiled as [null]*/

external undefined: undefined<'a> = "#undefined"
/** The same as  [empty] {!Js.Undefined} will be compiled as [undefined]*/

external typeof: 'a => string = "#typeof"
/** [typeof x] will be compiled as [typeof x] in JS
    Please consider functions in {!Types} for a type safe way of reflection
*/

@val @scope("console")
external log: 'a => unit = "log"
/** A convenience function to log everything */

@val @scope("console")
external log2: ('a, 'b) => unit = "log"
@val @scope("console")
external log3: ('a, 'b, 'c) => unit = "log"
@val @scope("console")
external log4: ('a, 'b, 'c, 'd) => unit = "log"

@val @scope("console") @variadic
external logMany: array<'a> => unit = "log"
/** A convenience function to log more than 4 arguments */

external eqNull : ('a, null<'a>) => bool = "%bs_equal_null"
external eqUndefined : ('a, undefined<'a>) => bool = "%bs_equal_undefined"
external eqNullable : ('a, nullable<'a>) => bool = "%bs_equal_nullable"

/** {4 operators }*/

external unsafe_lt: ('a, 'a) => bool = "#unsafe_lt"
/** [unsafe_lt a b] will be compiled as [a < b].
    It is marked as unsafe, since it is impossible
    to give a proper semantics for comparision which applies to any type
*/

external unsafe_le: ('a, 'a) => bool = "#unsafe_le"
/**  [unsafe_le a b] will be compiled as [a <= b].
     See also {!unsafe_lt}
*/

external unsafe_gt: ('a, 'a) => bool = "#unsafe_gt"
/**  [unsafe_gt a b] will be compiled as [a > b].
     See also {!unsafe_lt}
*/

external unsafe_ge: ('a, 'a) => bool = "#unsafe_ge"
/**  [unsafe_ge a b] will be compiled as [a >= b].
     See also {!unsafe_lt}
*/

/** {12 nested modules}*/

module Null = Js_null
/** Provide utilities around ['a null] */

module Undefined = Js_undefined
/** Provide utilities around {!undefined} */

module Nullable = Js_null_undefined
/** Provide utilities around {!null_undefined} */

module Null_undefined = Js_null_undefined
/** @deprecated please use {!Js.Nullable} */

module Exn = Js_exn

module Array = Js_array

module Array2 = Js_array2

module String = Js_string

module String2 = Js_string2

module Re = Js_re

module Promise = Js_promise

module Promise2 = Js_promise2

module Date = Js_date

module Dict = Js_dict

module Global = Js_global

module Json = Js_json

module Math = Js_math

module Obj = Js_obj

module Typed_array = Js_typed_array

module TypedArray2 = Js_typed_array2

module Types = Js_types

module Float = Js_float

module Int = Js_int

module BigInt = Js_bigint

module File = Js_file

module Blob = Js_blob

module Option = Js_option

module Result = Js_result

module List = Js_list

module Vector = Js_vector

module Console = Js_console

module Set = Js_set

module WeakSet = Js_weakset

module Map = Js_map

module WeakMap = Js_weakmap
