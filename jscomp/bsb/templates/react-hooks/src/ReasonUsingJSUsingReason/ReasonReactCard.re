// In this Interop example folder, we have:
// - A ReasonReact component, ReasonReactCard.re (this file)
// - Used by a ReactJS component, ReactJSCard.js
// - ReactJSCard.js can be used by ReasonReact, through bindings in ReasonUsingJSUsingReason.re
// - ReasonUsingJSUsingReason.re is used by Index.re

[@react.component]
let make = (~style) => {
  <div style> {React.string("This is a ReasonReact card")} </div>;
};
