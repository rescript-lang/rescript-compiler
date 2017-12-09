
type ('a, 'id) hash
type ('a, 'id) eq
    
external getHash : ('a,'id) hash -> ('a -> int [@bs]) = "%identity"
external getEq : ('a, 'id) eq -> ('a -> 'a -> bool [@bs]) = "%identity"

module type S = sig
  type id
  type t
  val hash : (t,id) hash
  val eq : (t,id) eq 
end

type ('key, 'id) t = (module S with type t = 'key and type id = 'id)

val make:
  eq:('a -> 'a -> bool [@bs]) ->
  hash:('a -> int [@bs]) ->
  (module S with type t = 'a)
    
