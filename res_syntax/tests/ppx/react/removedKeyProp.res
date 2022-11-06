@@jsxConfig({version: 4, mode: "classic"})

module Foo = {
  @react.component
  let make = (~x, ~y) => React.string(x++y)
}

module HasChildren = {
  @react.component
  let make = (~children) => <> children </>
}

@react.component
let make = () => <>
  <Foo key="k" x="x" y="y" />
  <Foo x="x" y="y" />
  <HasChildren key="k">
    <Foo x="x" y="y" />
  </HasChildren>
  <HasChildren>
    <Foo x="x" y="y" />
  </HasChildren>
</>
