@unboxed
type t<+'a> =
  | Value('a)
  | @as(null) Null
  | @as(undefined) Undefined
