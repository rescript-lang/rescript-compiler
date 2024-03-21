@@uncurried

type dateFormatOptions = {someOption?: bool}

@module("SomeModule")
external formatDate: (Js.Date.t, ~options: dateFormatOptions=?, ~done: bool=?) => string =
  "formatDate"

let x = formatDate(Js.Date.make())
let x = formatDate(Js.Date.make(), ~options={someOption: true})
let x = formatDate(Js.Date.make(), ~done=true)
