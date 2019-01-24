module type S = sig
  val foo : int -> int
end

module O (X : S) = struct
  let cow x = X.foo x
  let sheep x = 1 + cow x
end [@@inline always]

module F (X : S) (Y : S) = struct
  let cow x = Y.foo (X.foo x)
  let sheep x = 1 + cow x
end [@@inline always]

module type S1 = sig
  val bar : int -> int
  val foo : int -> int
end

module type T = sig
  val sheep : int -> int
end

module F1 (X : S) (Y : S) : T = struct
  let cow x = Y.foo (X.foo x)
  let sheep x = 1 + cow x
end [@@inline always]

module F2 : S1 -> S1 -> T = functor (X : S) -> functor (Y : S) -> struct
  let cow x = Y.foo (X.foo x)
  let sheep x = 1 + cow x
end [@@inline always]

module M : sig
  module F (X : S1) (Y : S1) : T
end = struct
  module F (X : S) (Y : S) = struct
    let cow x = Y.foo (X.foo x)
    let sheep x = 1 + cow x
  end [@@inline always]
end
