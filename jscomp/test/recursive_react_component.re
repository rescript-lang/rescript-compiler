
[@bs.config {
  flags : [|"-bs-jsx","3", "-dsource",
     // "-w","A", 
     // "-warn-error", "a"
    |]
}];

[@react.component]
let rec make = (~foo, ()) =>
  React.createElement(make, makeProps(~foo, ()));