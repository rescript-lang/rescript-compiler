let optionMap = (x, f) =>
  switch x {
  | None => None
  | Some(v) => Some(f(v))
  }

module Parens = {
  @react.component
  let make = (~id=?) => {
    <div id=?{id->optionMap(x => x)} />
  }
}