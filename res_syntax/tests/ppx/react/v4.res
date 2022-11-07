// Component with type constraint
@react.component
let make = (~x: string, ~y: string) => React.string(x ++ y)

module AnotherName = {
  // Component with another name than "make"
  @react.component
  let anotherName = (~x) => React.string(x)
}
