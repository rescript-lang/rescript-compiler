@@jsxConfig({version: 4})

module C0 = {
  let a = 1
  @react.component
  let make = (~a=2, ~b=a * 2) => {
    let _ = a + b
    React.null
  }
}

module C1 = {
  @react.component
  let make = (~foo as bar="") => {
    let _ = bar
    React.null
  }
}
