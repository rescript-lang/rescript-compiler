@@bs.config({
  flags: ["-bs-jsx", "4"],
})

module C0 = {
  let a = 1
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
