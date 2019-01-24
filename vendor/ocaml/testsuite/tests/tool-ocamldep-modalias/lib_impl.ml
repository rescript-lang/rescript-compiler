module Packed = struct
  module A = LibA
  module B = LibB
  module C = LibC
end
include Packed

let imp x = x+1
