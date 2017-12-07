
type 'a compare = ('a -> 'a -> int)
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

let make 
  (type key) 
  (cmp : key -> key -> int [@bs])   
  =
  let module M = struct 
    type t = key
    let cmp = cmp
  end in  
  let module N = Make(M) in 
  (module N : S with type t = key)

