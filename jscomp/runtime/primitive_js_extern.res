/**
  Nullable value of this type can be either null or 'a. This type is equivalent to Js.Null.t.
*/
@unboxed
type null<+'a> = Value('a) | @as(null) Null

/**
  A value of this type can be either undefined or 'a. This type is equivalent to Js.Undefined.t.
*/
type undefined<+'a>

@unboxed type nullable<+'a> = Value('a) | @as(null) Null | @as(undefined) Undefined

/***
  A value of this type can be undefined, null or 'a. This type is equivalent to Js.Null_undefined.t.
*/

type null_undefined<+'a> = nullable<'a>

/**
  The same as empty in `Js.Null`. Compiles to `null`.
*/
external null: null<'a> = "%null"

/**
  The same as empty `Js.Undefined`. Compiles to `undefined`.
*/
external undefined: undefined<'a> = "%undefined"

external isNullable: nullable<'a> => bool = "%is_nullable"

/** The same as {!test} except that it is more permissive on the types of input */
external testAny: 'a => bool = "%is_nullable"

/**
`typeof x` will be compiled as `typeof x` in JS. Please consider functions in
`Js.Types` for a type safe way of reflection.
*/
external typeof: 'a => string = "%typeof"

external eqNull: ('a, null<'a>) => bool = "%equal_null"
external eqUndefined: ('a, undefined<'a>) => bool = "%equal_undefined"
external eqNullable: ('a, nullable<'a>) => bool = "%equal_nullable"

/* ## Operators */

/**
   `unsafe_lt(a, b)` will be compiled as `a < b`.
    It is marked as unsafe, since it is impossible
    to give a proper semantics for comparision which applies to any type
*/
external unsafe_lt: ('a, 'a) => bool = "%unsafe_lt"

/**
   `unsafe_le(a, b)` will be compiled as `a <= b`.
   See also `Js.unsafe_lt`.
*/
external unsafe_le: ('a, 'a) => bool = "%unsafe_le"

/**
   `unsafe_gt(a, b)` will be compiled as `a > b`.
    See also `Js.unsafe_lt`.
*/
external unsafe_gt: ('a, 'a) => bool = "%unsafe_gt"

/**
   `unsafe_ge(a, b)` will be compiled as `a >= b`.
   See also `Js.unsafe_lt`.
*/
external unsafe_ge: ('a, 'a) => bool = "%unsafe_ge"
