module type Base = sig
  (*
    Base module that defines a monad.
    Separate from S since all other functions can be derived from these ones.
  *)
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type Infix = sig
  (*
    Common operators to operate with monadic values
  *)
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  include Infix

  module Monad_infix : Infix with type 'a t := 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val ignore : 'a t -> unit t
end

(**
  Monad construction functor.

  Used to create monad instances for different types, automatically defining
  operators and functions over them.
*)
module Make (M : Base) : S with type 'a t := 'a M.t = struct
  let bind = M.bind
  let return = M.return

  module Monad_infix = struct
    let (>>=) = bind
    let (>>|) t f = t >>= fun a -> return (f a)
  end
  include Monad_infix

  let join t = t >>= fun t' -> t'
  let map t ~f = t >>| f
  let ignore t = map t ~f:(fun _ -> ())
end


(**
   Two parameter monad below!

   This particular signatures are prepared for the Result type, and as such will
   preserve the second type, allowing you to only change the first one.
*)

module type Base2 = sig
  type ('a, 'd) t
  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val return : 'a -> ('a, _) t
end

module type Infix2 = sig
  type ('a, 'd) t
  val (>>=) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val (>>|) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t
end

module type S2 = sig
  include Infix2
  module Monad_infix : Infix2 with type ('a, 'd) t := ('a, 'd) t
  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t
  val return : 'a -> ('a, _) t
  val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t
  val join : (('a, 'd) t, 'd) t -> ('a, 'd) t
  val ignore : (_, 'd) t -> (unit, 'd) t
end

(*
  This module is used to type check that the single and multiparameter monad
  instances do not deviate from each other, but rather S2 _refines_ S.

  A similar one would be required if we included S3 or monads parametrised over
  more than 2 types.
*)
module Check_S2_refines_S (X : S) : (S2 with type ('a, 'd) t = 'a X.t) =
struct
  type ('a, 'd) t = 'a X.t
  include struct
    open X
    let (>>=)      = (>>=)
    let (>>|)      = (>>|)
    let bind       = bind
    let return     = return
    let map        = map
    let join       = join
    let ignore     = ignore
  end
  module Monad_infix = struct
    open X.Monad_infix
    let (>>=) = (>>=)
    let (>>|) = (>>|)
  end
end

(**
  Monad construction functor for monads parametrised over two types.

  Used to create monad instances for different types, automatically defining
  operators and functions over them.
*)
module Make2 (M : Base2) : S2 with type ('a, 'd) t := ('a, 'd) M.t = struct
  let bind = M.bind
  let return = M.return

  module Monad_infix = struct
    let (>>=) = bind
    let (>>|) t f = t >>= fun a -> return (f a)
  end
  include Monad_infix

  let join t = t >>= fun t' -> t'
  let map t ~f = t >>| f
  let ignore t = map t ~f:(fun _ -> ())
end
