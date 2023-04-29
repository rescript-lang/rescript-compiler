@@jsxConfig({version: 3})

module V3 = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}

@@jsxConfig({version: 4, mode: "classic"})

module V4C = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}

@@jsxConfig({version: 4, mode: "automatic"})

module V4C = {
  @module("componentForwardRef") @react.component
    external make: (
      ~x: string,
      ~ref: ReactDOM.Ref.currentDomRef=?,
    ) => React.element = "component"
}
