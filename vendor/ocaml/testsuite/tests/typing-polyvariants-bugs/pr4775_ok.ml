module type Poly = sig
  type 'a t = 'a constraint 'a = [> ]
end

module Combine (A : Poly) (B : Poly) = struct
  type ('a, 'b) t = 'a A.t constraint 'a = 'b B.t
end

module C = Combine
  (struct type 'a t = 'a constraint 'a = [> ] end)
  (struct type 'a t = 'a constraint 'a = [> ] end)
