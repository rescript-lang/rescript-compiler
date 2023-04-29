module C0 = {
  @react.component
  let make = (~a=2, ~b=a*2) => React.int(a + b)
}

module C1 = {
  @react.component
  let make = (~a=2, ~b) => React.int(a + b)
}
