module C0 = {
  @react.component
  let make = (~a=2, ~b=a*2) => <div />
}

module C1 = {
  @react.component
  let make = (~a=2, ~b) => <div />
}
