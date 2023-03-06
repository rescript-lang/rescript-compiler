@@bs.config({
  flags: ["-bs-jsx", "4"],
})

module C0 = {
  @react.component
  let make = (~a=2, ~b=a * 2) => {
    React.int(a + b)
  }
}

module C1 = {
  @react.component
  let make = (~foo as bar="") => {
    React.string(bar)
  }
}

module C2 = {
  @react.component
  let make = (~foo as bar="", ~a=bar, ~b) => {
    React.string(bar ++ a ++ b)
  }
}
