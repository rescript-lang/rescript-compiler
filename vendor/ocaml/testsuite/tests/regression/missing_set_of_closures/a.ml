module type Ret = sig
  val g : int -> int -> int
end

module F() : Ret = struct
  let n = Sys.opaque_identity 42
  let rec f = ((fun x -> x + n) [@inline never])
  and g = ((fun x -> f) [@inline])
end [@@inline never]
