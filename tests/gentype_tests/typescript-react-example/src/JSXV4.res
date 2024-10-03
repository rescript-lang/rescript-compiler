@@jsxConfig({version: 4})

@genType
module CompV4 = {
  @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@@jsxConfig({version: 3})

@genType
module CompV3 = {
  @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@genType
type person = {
  name: string,
  age: int,
}

@genType type renderMe<'a> = React.component<{"randomString": string, "poly": 'a}>

@@jsxConfig({version: 4})

@genType.import("./hookExample") @react.component
external make: (
  ~actions: React.element=?,
  ~person: person,
  ~children: React.element,
  ~renderMe: renderMe<'a>,
) => React.element = "make"
