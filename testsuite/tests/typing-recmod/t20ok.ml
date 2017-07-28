(* PR 4557 *)
module PR_4557 = struct
  module F ( X : Set.OrderedType ) = struct
    module rec Mod : sig
      module XSet :
        sig
          type elt = X.t
          type t = Set.Make( X ).t
        end
      module XMap :
        sig
          type key = X.t
          type 'a t = 'a Map.Make(X).t
        end
      type elt = X.t
      type t = XSet.t XMap.t
      val compare: t -> t -> int
    end
       =
    struct
      module XSet = Set.Make( X )
      module XMap = Map.Make( X )

      type elt = X.t
      type t = XSet.t XMap.t
      let compare = (fun x y -> 0)
    end
    and ModSet : Set.S with type elt = Mod.t = Set.Make( Mod )
  end
end
