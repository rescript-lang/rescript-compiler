module type Comparable = sig
  val id: int
end

module Fold_ordered(P: sig module Id:Comparable end): sig
  val foo: int
end



module M : sig end
module O : sig end
