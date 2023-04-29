@@jsxConfig({version: 3})

module C30 = {
  @react.component
  let make = (~_open) => React.string(_open)
}
module C31 = {
  @react.component
  external make: (~_open: string) => React.element = "default"
}

let c30 = <C30 _open="x" />
let c31 = <C31 _open="x" />

@@jsxConfig({version: 4, mode: "classic"})

module C4C0 = {
  @react.component
  let make =
    (@as("open") ~_open, @as("type") ~_type: string) => React.string(_open)
}
module C4C1 = {
  @react.component
  external make: (@as("open") ~_open: string, @as("type") ~_type: string) => React.element =
    "default"
}

let c4c0 = <C4C0 _open="x" _type="t" />
let c4c1 = <C4C1 _open="x" _type="t" />

@@jsxConfig({version: 4, mode: "automatic"})

module C4A0 = {
  @react.component
  let make =
    (@as("open") ~_open, @as("type") ~_type: string) => React.string(_open)
}
module C4A1 = {
  @react.component
  external make: (@as("open") ~_open: string, @as("type") ~_type: string) => React.element =
    "default"
}

let c4a0 = <C4A0 _open="x" _type="t" />
let c4a1 = <C4A1 _open="x" _type="t" />
