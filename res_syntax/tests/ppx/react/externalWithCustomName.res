@@jsxConfig({version: 3})

module Foo = {
  @react.component @module("Foo")
  external component: (~a: int, ~b: string, _) => React.element = "component"
}

let t = <Foo.component a=1 b={"1"} />

@@jsxConfig({version: 4, mode: "classic"})

module Foo = {
  @react.component @module("Foo")
  external component: (~a: int, ~b: string, _) => React.element = "component"
}

let t = <Foo.component a=1 b={"1"} />

@@jsxConfig({version: 4, mode: "automatic"})

module Foo = {
  @react.component @module("Foo")
  external component: (~a: int, ~b: string, _) => React.element = "component"
}

let t = <Foo.component a=1 b={"1"} />
