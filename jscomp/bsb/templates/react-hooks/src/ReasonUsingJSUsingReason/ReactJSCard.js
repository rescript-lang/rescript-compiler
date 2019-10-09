// In this Interop example folder, we have:
// - A ReasonReact component, ReasonReactCard.re
// - Used by a ReactJS component, ReactJSCard.js (this file)
// - ReactJSCard.js can be used by ReasonReact, through bindings in ReasonUsingJSUsingReason.re
// - ReasonUsingJSUsingReason.re is used by Index.re

var ReactDOM = require('react-dom');
var React = require('react');

var ReasonReactCard = require('./ReasonReactCard.bs').make;

var ReactJSComponent = function() {
  let backgroundColor = "rgba(0, 0, 0, 0.05)";
  let padding = "12px";

  // We're not using JSX here, to avoid folks needing to install the related
  // React toolchains just for this example.
  // <div style={...}>
  //   <div style={...}>This is a ReactJS card</div>
  //   <ReasonReactCard style={...} />
  // </div>
  return React.createElement(
    "div",
    {style: {backgroundColor, padding, borderRadius: "8px"}},
    React.createElement("div", {style: {marginBottom: "8px"}}, "This is a ReactJS card"),
    React.createElement(ReasonReactCard, {style: {backgroundColor, padding, borderRadius: "4px"}}),
  )
};
ReactJSComponent.displayName = "MyBanner";

module.exports = ReactJSComponent;
