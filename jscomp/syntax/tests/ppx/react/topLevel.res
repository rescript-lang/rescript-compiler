@@jsxConfig({version: 3})

module V3 = {
  @react.component
  let make = (~a, ~b, _) => {
    Js.log("This function should be named 'TopLevel.react'")
    <div />
  }
}

@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @react.component
  let make = (~a, ~b, _) => {
    Js.log("This function should be named 'TopLevel.react'")
    <div />
  }
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4A = {
  @react.component
  let make = (~a, ~b, _) => {
    Js.log("This function should be named 'TopLevel.react'")
    <div />
  }
}
