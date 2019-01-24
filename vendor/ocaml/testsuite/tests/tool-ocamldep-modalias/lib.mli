module Packed : sig
  module A = LibA
  module B = LibB
  module C = LibC
end
include (module type of struct include Packed end)

val imp : int -> int
