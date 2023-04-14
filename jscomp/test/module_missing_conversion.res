module type S = module type of String

module XX = {
  include Array
  let f = x => x
}
let u = [module(String: S)]

let hh = {
  let module(String: S) = u[0]
  String.length("x")
}

let ghh: Hashtbl.t<int, int> = MoreLabels.Hashtbl.create(30)
