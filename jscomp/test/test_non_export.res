include (
  {
    module V = String
    module U = V

    let u = U.make
    let v = module(U: Set.OrderedType)

    let pack = h => {
      module U = unpack(h: Set.OrderedType)
      module V = Set.Make(U)
      module(V: Set.S)
    }

    let g = pack(v)
    let gg = pack(module(String))
    module N = Set.Make(U)
    module NN = Set.Make(String)
  }: {}
)
