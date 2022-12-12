/*** Provides functionality for dealing with the `'a Js.undefined` type */

type t<+'a> = Js.undefined<'a>
external to_opt: t<'a> => option<'a> = "#undefined_to_opt"
external toOption: t<'a> => option<'a> = "#undefined_to_opt"
external return: 'a => t<'a> = "%identity"

external empty: t<'a> = "#undefined"
let test: t<'a> => bool = x => x == empty
let testAny: 'a => bool = x => Obj.magic(x) == empty
external getUnsafe: t<'a> => 'a = "%identity"

let getExn = f =>
  switch toOption(f) {
  | None => Js_exn.raiseError("Js.Undefined.getExn")
  | Some(x) => x
  }

let bind = (x, f) =>
  switch to_opt(x) {
  | None => empty
  | Some(x) => return(f(. x))
  }

let iter = (x, f) =>
  switch to_opt(x) {
  | None => ()
  | Some(x) => f(. x)
  }

let fromOption = x =>
  switch x {
  | None => empty
  | Some(x) => return(x)
  }

let from_opt = fromOption
