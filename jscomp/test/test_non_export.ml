include (
  struct
    module V = String
    module U = V

    let u = U.make
    let v = (module U : Set.OrderedType)

    let pack h =
      let module U = (val h : Set.OrderedType) in
      let module V = Set.Make (U) in
      (module V : Set.S)

    let g = pack v
    let gg = pack (module String)

    module N = Set.Make (U)
    module NN = Set.Make (String)
  end :
    sig end )
