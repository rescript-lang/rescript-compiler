

type ('a,'id) hash = ('a -> int [@bs])
type ('a, 'id) eq = ('a -> 'a -> bool [@bs])
external getHash : ('a,'id) hash -> ('a -> int [@bs]) = "%identity"
external getEq : ('a, 'id) eq -> ('a -> 'a -> bool [@bs]) = "%identity"

module type S = sig
  type id
  type t
  val hash : (t,id) hash
  val eq : (t,id) eq 
end

type ('key, 'id) t = (module S with type t = 'key and type id = 'id)

module Make (M : sig
   type t
   val hash : t -> int  [@bs]
   val eq : t -> t -> bool [@bs]
  end) =
struct
  type id
  type t = M.t
  let hash = M.hash
  let eq = M.eq
end

let make 
  (type key) 
  ~eq:(eq : key -> key -> bool [@bs])
  ~hash:(hash: key -> int [@bs])
  =
  let module M = struct 
    type t = key
    let hash = hash
    let eq = eq
  end in  
  let module N = Make(M) in 
  (module N : S with type t = key)

