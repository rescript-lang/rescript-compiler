module StandardNotation = {
  external raise: (. exn) => 'a = "%raise"
  let dd = () => raise(. Not_found)

  @val external sum: (. float, float) => float = "sum"
  let h = sum(. 1.0, 2.0)

  module M: {
    let sum: (. float, float) => float
  } = {
    external sum: (. float, float) => float = "sum"
  }
  let hh = M.sum(. 1.0, 2.0)

  external mod_float : (. float, float) => float = "?fmod_float"
  let mf = mod_float(. 3., 4.)
}

@@uncurried

external raise: exn => 'a = "%raise"
let dd = (. ()) => raise(Not_found)

@val external sum: (float, float) => float = "sum"
let h = sum(1.0, 2.0)

module M: {
  let sum: (float, float) => float
} = {
  external sum: (float, float) => float = "sum"
}
let hh = M.sum(1.0, 2.0)

external mod_float : (float, float) => float = "?fmod_float"
let mf = mod_float(3., 4.)
