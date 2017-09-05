type bla


external foo : bla = "foo.react" [@@bs.module]

external bar : unit -> bla  = "bar" [@@bs.val] [@@bs.module "foo.react"]

let c = foo 

let d = bar ()