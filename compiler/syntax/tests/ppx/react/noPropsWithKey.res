@@jsxConfig({version: 4, mode: "classic"})

module V4CA = {
  @react.component
  let make = () => <div />
}

module V4CB = {
  @module("c") @react.component
  external make: unit => React.element = "component"
}

module V4C = {
  @react.component
  let make = () => <><V4CA key="k" /> <V4CB key="k" /></>
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4CA = {
  @react.component
  let make = () => <div />
}

module V4CB = {
  @module("c") @react.component
  external make: unit => React.element = "component"
}

module V4C = {
  @react.component
  let make = () => <><V4CA key="k" /> <V4CB key="k" /></>
}