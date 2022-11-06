@@jsxConfig({version: 3})

module V3 = {
  @module("c") @react.component
    external make: (
      ~x: t<'a>,
      ~children: React.element,
    ) => React.element = "component"
}

@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @module("c") @react.component
    external make: (
      ~x: t<'a>,
      ~children: React.element,
    ) => React.element = "component"
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4C = {
  @module("c") @react.component
    external make: (
      ~x: t<'a>,
      ~children: React.element,
    ) => React.element = "component"
}
