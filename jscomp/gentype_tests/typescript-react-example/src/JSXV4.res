@@jsxConfig({version: 4})

@genType @react.component
let make = (~x, ~y) => React.string(x ++ y)

module CompV4 = {
  @genType @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}

@@jsxConfig({version: 3})

module CompV3 = {
  @genType @react.component
  let make = (~x, ~y) => React.string(x ++ y)
}
