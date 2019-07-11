module type G = sig
  type t

  module V : sig
    (** Vertices are {!COMPARABLE}. *)

    type t

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    type label

    val create : label -> t
    val label : t -> label
  end

  val succ : t -> V.t -> V.t list
end

module Make (G : G) : sig
  val min_cutset : G.t -> G.V.t -> G.V.t list
end
