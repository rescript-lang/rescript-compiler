module A = struct module type S module S = struct end end
module F (_ : sig end) = struct module type S module S = A.S end
module M = struct end
module N = M
module G (X : F(N).S) : A.S = X
