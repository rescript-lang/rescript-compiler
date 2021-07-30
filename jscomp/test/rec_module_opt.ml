
[@@@bs.config {
  flags = [|
  (* "-drawlambda"; *)
  (* "-dlambda";  *)
  (* "-dtypedtree"; *)
  (* "-bs-diagnose" *)

  "-bs-no-cross-module-opt"
  |]
}]

module rec A : sig
  type t = Leaf of string | Node of ASet.t
  val compare: t -> t -> int
end = struct
  type t = Leaf of string | Node of ASet.t
  let compare t1 t2 =
    match (t1, t2) with
    | (Leaf s1, Leaf s2) -> Pervasives.compare s1 s2
    | (Leaf _, Node _) -> 1
    | (Node _, Leaf _) -> -1
    | (Node n1, Node n2) -> ASet.compare n1 n2
  let hello x = x  
end
and ASet
  : Set.S with type elt = A.t
  = Set.Make(A)

module rec X : sig end = X  


module rec X0 : sig 
  type t   
end   = struct 
  type t  
end   
and Y0 : sig 
  type t   
end   = struct 
  type t   
end    
module type S = sig 
  val f : int -> int    
end   

module rec X1 : S = struct 
  let f x = x + 1
end  
and Y1 : S = struct 
  let f x = x + 2  
end  
