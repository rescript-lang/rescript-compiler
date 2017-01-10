
type u 

external a : u = "react" [@@bs.module]

external b : unit -> int = "bool" [@@bs.module "react"]

let v = a
let h = b ()

type bla


external foo : bla = "foo.react" [@@bs.module]

external bar : unit -> bla  = "bar" [@@bs.val] [@@bs.module "foo.react"]

let c = foo 

let d = bar ()
