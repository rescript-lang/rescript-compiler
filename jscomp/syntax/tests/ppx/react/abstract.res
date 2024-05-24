module SomeComp2 = {
  type props = {x: int}
  @module("SomeModule")
  external make: props => React.element = "SomeComp2"
}

let _ = <SomeComp2 x=42 />

@@jsxConfig({version: 3})

@react.component
let rec make = (~foo, ()) => React.createElement(make, makeProps(~foo, ()))
