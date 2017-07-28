type 'a termpc =
    [`And of 'a * 'a
    |`Or of 'a * 'a
    |`Not of 'a
    |`Atom of string
    ]

type 'a termk =
    [`Dia of 'a
    |`Box of 'a
    |'a termpc
    ]

module type T = sig
  type term
  val map : (term -> term) -> term -> term
  val nnf : term -> term
  val nnf_not : term -> term
end

module Fpc(X : T with type term = private [> 'a termpc] as 'a) =
  struct
    type term = X.term termpc
    let nnf = function
      |`Not(`Atom _) as x -> x
      |`Not x     -> X.nnf_not x
      | x         -> X.map X.nnf x
    let map f : term -> X.term = function
      |`Not x    -> `Not (f x)
      |`And(x,y) -> `And (f x, f y)
      |`Or (x,y) -> `Or  (f x, f y)
      |`Atom _ as x -> x
    let nnf_not : term -> _ = function
      |`Not x    -> X.nnf x
      |`And(x,y) -> `Or  (X.nnf_not x, X.nnf_not y)
      |`Or (x,y) -> `And (X.nnf_not x, X.nnf_not y)
      |`Atom _ as x -> `Not x
  end

module Fk(X : T with type term = private [> 'a termk] as 'a) =
  struct
    type term = X.term termk
    module Pc = Fpc(X)
    let map f : term -> _ = function
      |`Dia x -> `Dia (f x)
      |`Box x -> `Box (f x)
      |#termpc as x -> Pc.map f x
    let nnf = Pc.nnf
    let nnf_not : term -> _ = function
      |`Dia x -> `Box (X.nnf_not x)
      |`Box x -> `Dia (X.nnf_not x)
      |#termpc as x -> Pc.nnf_not x
  end
