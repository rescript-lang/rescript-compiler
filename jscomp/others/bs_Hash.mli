
type ('a, 'id) hash
type ('a, 'id) eq
    
external getHashInternal : ('a,'id) hash -> ('a -> int [@bs]) = "%identity"
external getEqInternal : ('a, 'id) eq -> ('a -> 'a -> bool [@bs]) = "%identity"

module type T = sig
  type id
  type t
  val hash : (t,id) hash
  val eq : (t,id) eq 
end

type ('key, 'id) t = (module T with type t = 'key and type id = 'id)

val make:
  eq:('a -> 'a -> bool [@bs]) ->
  hash:('a -> int [@bs]) ->
  (module T with type t = 'a)
    
