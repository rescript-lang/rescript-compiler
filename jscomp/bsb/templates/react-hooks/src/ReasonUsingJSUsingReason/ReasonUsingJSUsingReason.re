// In this Interop example folder, we have:
// - A ReasonReact component, ReasonReactCard.re
// - Used by a ReactJS component, ReactJSCard.js
// - ReactJSCard.js can be used by ReasonReact, through bindings in ReasonUsingJSUsingReason.re (this file)
// - ReasonUsingJSUsingReason.re is used by Index.re

// All you need to do to use a ReactJS component in ReasonReact, is to write the lines below!
// reasonml.github.io/reason-react/docs/en/components#import-from-js
[@react.component] [@bs.module]
external make: unit => React.element = "./ReactJSCard";
