@@jsxConfig({version: 4, mode: "automatic"})

module C0 = {
  @react.component
  let make = (~priority as _, ~text="Test") => React.string(text)
}

module C1 = {
  @react.component
  let make = (~priority as p, ~text="Test") => React.string(p ++ text)
}
