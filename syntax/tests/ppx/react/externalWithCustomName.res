module Foo = {
  @react.component @bs.module("Foo")
  external component: (~a: int, ~b: string, _) => React.element = "component"
}

let t = <Foo.component a=1 b={"1"} />
