module type Printable = {
  type t
  let print: (Format.formatter, t) => unit
}

module type Comparable = {
  type t
  let compare: (t, t) => int
}
