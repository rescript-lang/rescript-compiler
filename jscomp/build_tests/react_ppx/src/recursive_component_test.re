[@react.component]
let rec make = (~foo, ()) =>
  React.createElement(make, makeProps(~foo, ()));
