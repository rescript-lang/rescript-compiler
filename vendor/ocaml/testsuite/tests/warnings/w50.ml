module A : sig end = struct
  module L = List

  module X1 = struct end

  module Y1 = X1
end
