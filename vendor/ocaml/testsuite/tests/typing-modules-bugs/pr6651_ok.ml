module type S = sig
  module type T
  module X : T
end

module F (X : S) = X.X

module M = struct
  module type T = sig type t end
  module X = struct type t = int end
end

type t = F(M).t
