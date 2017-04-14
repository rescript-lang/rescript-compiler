


module rec CRS : sig type t end =  CRS
and Layer : sig type t end = Layer

and Point : sig
  type t
  val add : t -> t -> t 
end = struct
  type t
  external add : t -> t -> t = "" [@@bs.send]
end
