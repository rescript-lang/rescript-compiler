let f = "ghsogh".[3]
let hh = try "ghsogh".[-3] with Invalid_argument e -> Js.log e ; 'a'
