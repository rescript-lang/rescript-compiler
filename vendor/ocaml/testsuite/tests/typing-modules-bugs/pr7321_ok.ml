module type S = sig type 'a t end
module type Sp = sig type 'a t = private 'a array end

module Id (S : S) = S

module M : Sp = struct
  include Id (struct type 'a t = 'a array end)
end
