module Bar = {
  @react.component
  let make = (~a, ~b, _) => {
    Js.log(
      "This function should be named `InnerModule.react$Bar`",
    )
    <div />
  }
  @react.component
  let component = (~a, ~b, _) => {
    Js.log(
      "This function should be named `InnerModule.react$Bar$component`",
    )
    <div />
  }
}
