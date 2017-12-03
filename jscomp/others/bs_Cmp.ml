
type 'a compare = ('a -> 'a -> int [@bs])
type ('a,'id) cmp = 'a compare

external getCmp : ('a,'id) cmp -> 'a compare = "%identity"

module type S = sig
  type id
  type t
  val cmp : (t,id) cmp
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
