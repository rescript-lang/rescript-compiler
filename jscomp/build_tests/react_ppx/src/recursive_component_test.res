@@warning("-39")
// https://github.com/rescript-lang/rescript-compiler/issues/4511
/*
[@bs.config {
  flags : [|"-dsource"|]
}];
*/
@react.component
let rec make = (~foo, ()) => React.createElement(make, makeProps(~foo, ()))

@@jsxConfig({version:4})

module Rec = {
  @react.component
  let rec make = (~b) => {
    mm({b:b})
  }
  and mm = (x) => make({b: ! x.b})
}
