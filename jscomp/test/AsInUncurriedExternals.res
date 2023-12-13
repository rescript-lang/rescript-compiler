@@uncurried

@obj
external makeOptions: (
  ~objectMode: @as(json`false`) _,
  ~name: string,
  unit,
) => int = ""

let mo = makeOptions

let options = mo(~name="foo", ())
