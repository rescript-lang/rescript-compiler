/*** Contains functionality for dealing with values that can be both `null` and `undefined` */

type t<+'a> = Js.nullable<'a>
external toOption: t<'a> => option<'a> = "#nullable_to_opt"
external to_opt: t<'a> => option<'a> = "#nullable_to_opt"
external return: 'a => t<'a> = "%identity"
external isNullable: t<'a> => bool = "#is_nullable"
external null: t<'a> = "#null"
external undefined: t<'a> = "#undefined"

let bind = (x, f) =>
  switch to_opt(x) {
  | None => (Obj.magic((x: t<'a>)): t<'b>)
  | Some(x) => return(f(. x))
  }

let iter = (x, f) =>
  switch to_opt(x) {
  | None => ()
  | Some(x) => f(. x)
  }

let fromOption = x =>
  switch x {
  | None => undefined
  | Some(x) => return(x)
  }

let from_opt = fromOption
