

(* TODO:*)


external make: ?foo:([`a|`b] [@bs.string]) -> unit -> _ = "" [@@bs.obj]

let makeWrapper ?foo () = Js.log (make ?foo ())
let _ = 
  makeWrapper ~foo:`a () 
  