module type S = [%copy_typedef]

module type T = sig
  type t

  module type M = [%copy_typedef]
end

module M = struct
  type t = [%copy_typedef]
end

type t = [%copy_typedef]

let _x = M.A
let _y : t = [1; 2]


type _loc = [%copy_typedef "../../parsing/location.mli" t]
