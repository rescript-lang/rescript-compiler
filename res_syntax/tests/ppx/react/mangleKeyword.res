@@jsxConfig({version: 3})

module C0 = {
  @react.component
  let make = (~_open) => React.string(_open)
}
module C1 = {
  @react.component
  external make: (~_open: string) => React.element = "default"
}

let c0 = <C0 _open="x" />
let c1 = <C1 _open="x" />

@@jsxConfig({version: 4, mode: "classic"})

module C0 = {
  @react.component
  let make =
    (@as("open") ~_open, @as("type") ~_type: string) => React.string(_open)
}
module C1 = {
  @react.component
  external make: (@as("open") ~_open: string, @as("type") ~_type: string) => React.element =
    "default"
}

let c0 = <C0 _open="x" _type="t" />
let c1 = <C1 _open="x" _type="t" />

@@jsxConfig({version: 4, mode: "automatic"})

module C0 = {
  @react.component
  let make =
    (@as("open") ~_open, @as("type") ~_type: string) => React.string(_open)
}
module C1 = {
  @react.component
  external make: (@as("open") ~_open: string, @as("type") ~_type: string) => React.element =
    "default"
}

let c0 = <C0 _open="x" _type="t" />
let c1 = <C1 _open="x" _type="t" />
