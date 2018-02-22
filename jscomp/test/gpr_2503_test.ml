

(* TODO:*)


external make: ?foo:([`a|`b] [@bs.string]) -> unit -> _ = "" [@@bs.obj]

let makeWrapper ?foo () = Js.log (make ?foo ())

external make2: foo:([`a|`b] [@bs.string]) -> unit -> _ = "" [@@bs.obj]

let makeWrapper2 foo () = Js.log (make2 foo ())

let _ = 
  makeWrapper2 `a () 
  