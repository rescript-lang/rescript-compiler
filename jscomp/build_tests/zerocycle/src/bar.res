// one-file false positive

module Nested = {
  module Bar = {
    type t = private int
  }
}

open Nested

module Bar = {
  open Bar
  let t : t = Obj.magic(42)
}