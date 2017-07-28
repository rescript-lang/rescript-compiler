module type S = sig
  include Set.S
  module E : sig val x : int end
end

module Make(O : Set.OrderedType) : S with type elt = O.t =
  struct
    include Set.Make(O)
    module E = struct let x = 1 end
  end

module rec A : Set.OrderedType = struct
 type t = int
  let compare = Pervasives.compare
end
and B : S = struct
 module C = Make(A)
 include C
end
