(* Bad - PR 4261 *)

module PR_4261 = struct
  module type S =
  sig
    type t
  end

  module type T =
  sig
    module D : S
    type t = D.t
  end

  module rec U : T with module D = U' = U
  and U' : S with type t = U'.t = U
end;;
