module type S = module type of String

module XX = struct
  include Array

  let f x = x
end

let u = [|(module String : S)|]

let hh =
  let (module String : S) = u.(0) in
  String.length "x"

let ghh : (int, int) Hashtbl.t = MoreLabels.Hashtbl.create 30
