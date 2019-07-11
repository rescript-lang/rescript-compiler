module N : sig
  val add : int -> int -> int
end

module type S0 = sig
  val f1 : unit -> unit
  val f2 : unit -> unit -> unit
  val f3 : unit -> unit -> unit -> unit
end

module N0 : S0

module N1 : sig
  val f2 : unit -> unit -> unit
  val f3 : unit -> unit -> unit -> unit
end
