module F (_ : sig end) = struct module type S end
module M = struct end
module N = M
module G (X : F(N).S) : F(M).S = X
