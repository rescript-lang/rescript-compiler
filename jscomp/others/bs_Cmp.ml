
type 'a compare = ('a -> 'a -> int [@bs])
type 'a cmp = 'a compare
external getCmp : 'a cmp -> 'a cmp = "%identity"

module type S = sig
  type id
  type t
  val cmp : t cmp
end
type ('key, 'id) t = (module S with type t = 'key and type id = 'id)

module Make (M : sig
   type t
    val cmp : t -> t -> int [@bs]
  end) =
struct
  type id
  type t = M.t
  let cmp = M.cmp
end
