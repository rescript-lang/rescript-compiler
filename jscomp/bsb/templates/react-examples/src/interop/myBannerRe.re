/* ReactJS used by ReasonReact */

/* This component wraps a ReactJS one, so that ReasonReact components can consume it */

/* Typing the myBanner.js component's output as a `reactClass`. */
/* Note that this file's JS output is located at reason-react-example/lib/js/src/interop/myBannerRe.js; we're specifying the relative path to myBanner.js in the string below */
[@bs.module] external myBanner : ReasonReact.reactClass = "../../../../src/interop/myBanner";

/* This is like declaring a normal ReasonReact component's `make` function, except the body is a the interop hook wrapJsForReason */
let make = (~show, ~message, children) =>
  ReasonReact.wrapJsForReason(
    ~reactClass = myBanner,
    ~props = {
      "show": Js.Boolean.to_js_boolean(show), /* ^ don't forget to convert an OCaml bool into a JS boolean! */
      "message": message /* OCaml string maps to JS string, no conversion needed here */
    },
    children);
