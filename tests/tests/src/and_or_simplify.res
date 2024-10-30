@unboxed
type t = | @as(null) Null | @as(undefined) Undefined | B(bool) //| S(string)

let check_null_typeof = x =>
  switch x {
  | B(_) if x == Null => 3
  | _ => 4
  }

let check_undefined_typeof = x =>
  switch x {
  | B(_) if x == Undefined => 3
  | _ => 4
  }
