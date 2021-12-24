/* Mixin */
module Impl = (T: {type t;}) => {
  [@bs.send.pipe : T.t] external addSelectionChangeEventListener : ([@bs.as "selectionchange"] _, Dom.focusEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addSelectionChangeEventListenerWithOptions : ([@bs.as "selectionchange"] _, Dom.focusEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addSelectionChangeEventListenerUseCapture : ([@bs.as "selectionchange"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeSelectionChangeEventListener : ([@bs.as "selectionchange"] _, Dom.focusEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeSelectionChangeEventListenerWithOptions : ([@bs.as "selectionchange"] _, Dom.focusEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeSelectionChangeEventListenerUseCapture : ([@bs.as "selectionchange"] _, Dom.focusEvent =>unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";
};
