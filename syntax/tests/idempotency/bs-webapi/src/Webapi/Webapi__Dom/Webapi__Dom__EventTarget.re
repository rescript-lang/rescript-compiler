module Impl = (T: {type t;}) => {
  external asEventTarget : T.t => Dom.eventTarget = "%identity";

  [@bs.send.pipe : T.t] external addEventListener : (string, Dom.event => unit) => unit = "";
  [@bs.send.pipe : T.t] external addEventListenerWithOptions : (string, Dom.event => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addEventListenerUseCapture : (string, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeEventListener : (string, Dom.event => unit) => unit = "";
  [@bs.send.pipe : T.t] external removeEventListenerWithOptions : (string, Dom.event => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeEventListenerUseCapture : (string, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external dispatchEvent : Dom.event_like('a) => bool = "";

  /**
   *  non-standard event-specific functions
   */

   /* UI */

  [@bs.send.pipe : T.t] external addLoadEventListener : ([@bs.as "load"] _, Dom.event => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addLoadEventListenerWithOptions : ([@bs.as "load"] _, Dom.event => unit,{. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addLoadEventListenerUseCapture : ([@bs.as "load"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeLoadEventListener : ([@bs.as "load"] _, Dom.event => unit) => unit= "removeEventListener";
  [@bs.send.pipe : T.t] external removeLoadEventListenerWithOptions : ([@bs.as "load"] _, Dom.event => unit, {. "capture": bool, "passive": bool})=> unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeLoadEventListenerUseCapture : ([@bs.as "load"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addUnloadEventListener : ([@bs.as "unload"] _, Dom.event => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addUnloadEventListenerWithOptions : ([@bs.as "unload"] _, Dom.event=> unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addUnloadEventListenerUseCapture : ([@bs.as "unload"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeUnloadEventListener : ([@bs.as "unload"] _, Dom.event => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeUnloadEventListenerWithOptions : ([@bs.as "unload"] _, Dom.event => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeUnloadEventListenerUseCapture : ([@bs.as "unload"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addAbortEventListener : ([@bs.as "abort"] _, Dom.event => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addAbortEventListenerWithOptions : ([@bs.as "abort"] _, Dom.event => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addAbortEventListenerUseCapture : ([@bs.as "abort"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeAbortEventListener : ([@bs.as "abort"] _, Dom.event => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeAbortEventListenerWithOptions : ([@bs.as "abort"] _, Dom.event => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeAbortEventListenerUseCapture : ([@bs.as "abort"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addErrorEventListener : ([@bs.as "error"] _, Dom.event => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addErrorEventListenerWithOptions : ([@bs.as "error"] _, Dom.event => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addErrorEventListenerUseCapture : ([@bs.as "error"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeErrorEventListener : ([@bs.as "error"] _, Dom.event => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeErrorEventListenerWithOptions : ([@bs.as "error"] _, Dom.event => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeErrorEventListenerUseCapture : ([@bs.as "error"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addSelectEventListener : ([@bs.as "select"] _, Dom.event => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addSelectEventListenerWithOptions : ([@bs.as "select"] _, Dom.event => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addSelectEventListenerUseCapture : ([@bs.as "select"] _, Dom.event => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeSelectEventListener : ([@bs.as "select"] _, Dom.event => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeSelectEventListenerWithOptions : ([@bs.as "select"] _, Dom.event => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeSelectEventListenerUseCapture : ([@bs.as "select"] _, Dom.event =>unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Focus */

  [@bs.send.pipe : T.t] external addBlurEventListener : ([@bs.as "blur"] _, Dom.focusEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addBlurEventListenerWithOptions : ([@bs.as "blur"] _, Dom.focusEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addBlurEventListenerUseCapture : ([@bs.as "blur"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeBlurEventListener : ([@bs.as "blur"] _, Dom.focusEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeBlurEventListenerWithOptions : ([@bs.as "blur"] _, Dom.focusEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeBlurEventListenerUseCapture : ([@bs.as "blur"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addFocusEventListener : ([@bs.as "focus"] _, Dom.focusEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addFocusEventListenerWithOptions : ([@bs.as "focus"] _, Dom.focusEvent => unit, {. "capture": bool, "once": bool,"passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addFocusEventListenerUseCapture : ([@bs.as "focus"] _, Dom.focusEvent =>unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeFocusEventListener : ([@bs.as "focus"] _, Dom.focusEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeFocusEventListenerWithOptions : ([@bs.as "focus"] _, Dom.focusEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeFocusEventListenerUseCapture : ([@bs.as "focus"] _, Dom.focusEvent=> unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addFocusInEventListener : ([@bs.as "focusin"] _, Dom.focusEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addFocusInEventListenerWithOptions : ([@bs.as "focusin"] _, Dom.focusEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addFocusInEventListenerUseCapture : ([@bs.as "focusin"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeFocusInEventListener : ([@bs.as "focusin"] _, Dom.focusEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeFocusInEventListenerWithOptions : ([@bs.as "focusin"] _, Dom.focusEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeFocusInEventListenerUseCapture : ([@bs.as "focusin"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addFocusOutEventListener : ([@bs.as "focusout"] _, Dom.focusEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addFocusOutEventListenerWithOptions : ([@bs.as "focusout"] _, Dom.focusEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addFocusOutEventListenerUseCapture : ([@bs.as "focusout"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeFocusOutEventListener : ([@bs.as "focusout"] _, Dom.focusEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeFocusOutEventListenerWithOptions : ([@bs.as "focusout"] _, Dom.focusEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeFocusOutEventListenerUseCapture : ([@bs.as "focusout"] _, Dom.focusEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Mouse */

  [@bs.send.pipe : T.t] external addClickEventListener : ([@bs.as "click"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addClickEventListenerWithOptions : ([@bs.as "click"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addClickEventListenerUseCapture : ([@bs.as "click"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeClickEventListener : ([@bs.as "click"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeClickEventListenerWithOptions : ([@bs.as "click"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeClickEventListenerUseCapture : ([@bs.as "click"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDblClickEventListener : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDblClickEventListenerWithOptions : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDblClickEventListenerUseCapture : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDblClickEventListener : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDblClickEventListenerWithOptions : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDblClickEventListenerUseCapture : ([@bs.as "dblclick"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseDownEventListener : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseDownEventListenerWithOptions : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseDownEventListenerUseCapture : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseDownEventListener : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseDownEventListenerWithOptions : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseDownEventListenerUseCapture : ([@bs.as "mousedown"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseEnterEventListener : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseEnterEventListenerWithOptions : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseEnterEventListenerUseCapture : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseEnterEventListener : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseEnterEventListenerWithOptions : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseEnterEventListenerUseCapture : ([@bs.as "mouseenter"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseMoveEventListener : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseMoveEventListenerWithOptions : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseMoveEventListenerUseCapture : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseMoveEventListener : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseMoveEventListenerWithOptions : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseMoveEventListenerUseCapture : ([@bs.as "mousemove"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseOutEventListener : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseOutEventListenerWithOptions : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseOutEventListenerUseCapture : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseOutEventListener : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseOutEventListenerWithOptions : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseOutEventListenerUseCapture : ([@bs.as "mouseout"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseOverEventListener : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseOverEventListenerWithOptions : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseOverEventListenerUseCapture : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseOverEventListener : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseOverEventListenerWithOptions : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseOverEventListenerUseCapture : ([@bs.as "mouseover"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addMouseUpEventListener : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addMouseUpEventListenerWithOptions : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addMouseUpEventListenerUseCapture : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeMouseUpEventListener : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeMouseUpEventListenerWithOptions : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeMouseUpEventListenerUseCapture : ([@bs.as "mouseup"] _, Dom.mouseEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Wheel */

  [@bs.send.pipe : T.t] external addWheelEventListener : ([@bs.as "wheel"] _, Dom.wheelEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addWheelEventListenerWithOptions : ([@bs.as "wheel"] _, Dom.wheelEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addWheelEventListenerUseCapture : ([@bs.as "wheel"] _, Dom.wheelEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeWheelEventListener : ([@bs.as "wheel"] _, Dom.wheelEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeWheelEventListenerWithOptions : ([@bs.as "wheel"] _, Dom.wheelEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeWheelEventListenerUseCapture : ([@bs.as "wheel"] _, Dom.wheelEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Input */

  [@bs.send.pipe : T.t] external addBeforeInputEventListener : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addBeforeInputEventListenerWithOptions : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addBeforeInputEventListenerUseCapture : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeBeforeInputEventListener : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeBeforeInputEventListenerWithOptions : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeBeforeInputEventListenerUseCapture : ([@bs.as "beforeinput"] _, Dom.inputEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addInputEventListener : ([@bs.as "input"] _, Dom.inputEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addInputEventListenerWithOptions : ([@bs.as "input"] _, Dom.inputEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addInputEventListenerUseCapture : ([@bs.as "input"] _, Dom.inputEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeInputEventListener : ([@bs.as "input"] _, Dom.inputEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeInputEventListenerWithOptions : ([@bs.as "input"] _, Dom.inputEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeInputEventListenerUseCapture : ([@bs.as "input"] _, Dom.inputEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Keyboard */

  [@bs.send.pipe : T.t] external addKeyDownEventListener : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addKeyDownEventListenerWithOptions : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addKeyDownEventListenerUseCapture : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeKeyDownEventListener : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeKeyDownEventListenerWithOptions : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeKeyDownEventListenerUseCapture : ([@bs.as "keydown"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addKeyUpEventListener : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addKeyUpEventListenerWithOptions : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addKeyUpEventListenerUseCapture : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeKeyUpEventListener : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeKeyUpEventListenerWithOptions : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeKeyUpEventListenerUseCapture : ([@bs.as "keyup"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";
  
  [@bs.send.pipe : T.t] external addKeyPressEventListener : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addKeyPressEventListenerWithOptions : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addKeyPressEventListenerUseCapture : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeKeyPressEventListener : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeKeyPressEventListenerWithOptions : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeKeyPressEventListenerUseCapture : ([@bs.as "keypress"] _, Dom.keyboardEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Composition */

  [@bs.send.pipe : T.t] external addCompositionStartEventListener : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addCompositionStartEventListenerWithOptions : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addCompositionStartEventListenerUseCapture : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeCompositionStartEventListener : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeCompositionStartEventListenerWithOptions : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeCompositionStartEventListenerUseCapture : ([@bs.as "compositionstart"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addCompositionUpdateEventListener : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addCompositionUpdateEventListenerWithOptions : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addCompositionUpdateEventListenerUseCapture : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeCompositionUpdateEventListener : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeCompositionUpdateEventListenerWithOptions : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeCompositionUpdateEventListenerUseCapture : ([@bs.as "compositionupdate"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addCompositionEndEventListener : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addCompositionEndEventListenerWithOptions : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addCompositionEndEventListenerUseCapture : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeCompositionEndEventListener : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeCompositionEndEventListenerWithOptions : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeCompositionEndEventListenerUseCapture : ([@bs.as "compositionend"] _, Dom.compositionEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Drag */

  [@bs.send.pipe : T.t] external addDragEventListener : ([@bs.as "drag"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragEventListenerWithOptions : ([@bs.as "drag"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragEventListenerUseCapture : ([@bs.as "drag"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragEventListener : ([@bs.as "drag"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragEventListenerWithOptions : ([@bs.as "drag"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragEventListenerUseCapture : ([@bs.as "drag"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragEndEventListener : ([@bs.as "dragend"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragEndEventListenerWithOptions : ([@bs.as "dragend"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragEndEventListenerUseCapture : ([@bs.as "dragend"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragEndEventListener : ([@bs.as "dragend"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragEndEventListenerWithOptions : ([@bs.as "dragend"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragEndEventListenerUseCapture : ([@bs.as "dragend"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragEnterEventListener : ([@bs.as "dragenter"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragEnterEventListenerWithOptions : ([@bs.as "dragenter"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragEnterEventListenerUseCapture : ([@bs.as "dragenter"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragEnterEventListener : ([@bs.as "dragenter"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragEnterEventListenerWithOptions : ([@bs.as "dragenter"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragEnterEventListenerUseCapture : ([@bs.as "dragenter"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragExitEventListener : ([@bs.as "dragexit"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragExitEventListenerWithOptions : ([@bs.as "dragexit"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragExitEventListenerUseCapture : ([@bs.as "dragexit"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragExitEventListener : ([@bs.as "dragexit"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragExitEventListenerWithOptions : ([@bs.as "dragexit"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragExitEventListenerUseCapture : ([@bs.as "dragexit"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragLeaveEventListener : ([@bs.as "dragleave"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragLeaveEventListenerWithOptions : ([@bs.as "dragleave"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragLeaveEventListenerUseCapture : ([@bs.as "dragleave"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragLeaveEventListener : ([@bs.as "dragleave"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragLeaveEventListenerWithOptions : ([@bs.as "dragleave"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragLeaveEventListenerUseCapture : ([@bs.as "dragleave"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragOverEventListener : ([@bs.as "dragover"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragOverEventListenerWithOptions : ([@bs.as "dragover"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragOverEventListenerUseCapture : ([@bs.as "dragover"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragOverEventListener : ([@bs.as "dragover"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragOverEventListenerWithOptions : ([@bs.as "dragover"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragOverEventListenerUseCapture : ([@bs.as "dragover"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDragStartEventListener : ([@bs.as "dragstart"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDragStartEventListenerWithOptions : ([@bs.as "dragstart"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDragStartEventListenerUseCapture : ([@bs.as "dragstart"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDragStartEventListener : ([@bs.as "dragstart"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDragStartEventListenerWithOptions : ([@bs.as "dragstart"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDragStartEventListenerUseCapture : ([@bs.as "dragstart"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addDropEventListener : ([@bs.as "drop"] _, Dom.dragEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addDropEventListenerWithOptions : ([@bs.as "drop"] _, Dom.dragEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addDropEventListenerUseCapture : ([@bs.as "drop"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeDropEventListener : ([@bs.as "drop"] _, Dom.dragEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeDropEventListenerWithOptions : ([@bs.as "drop"] _, Dom.dragEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeDropEventListenerUseCapture : ([@bs.as "drop"] _, Dom.dragEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Touch */

  [@bs.send.pipe : T.t] external addTouchCancelEventListener : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addTouchCancelEventListenerWithOptions : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addTouchCancelEventListenerUseCapture : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeTouchCancelEventListener : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeTouchCancelEventListenerWithOptions : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeTouchCancelEventListenerUseCapture : ([@bs.as "touchcancel"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addTouchEndEventListener : ([@bs.as "touchend"] _, Dom.touchEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addTouchEndEventListenerWithOptions : ([@bs.as "touchend"] _, Dom.touchEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addTouchEndEventListenerUseCapture : ([@bs.as "touchend"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeTouchEndEventListener : ([@bs.as "touchend"] _, Dom.touchEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeTouchEndEventListenerWithOptions : ([@bs.as "touchend"] _, Dom.touchEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeTouchEndEventListenerUseCapture : ([@bs.as "touchend"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addTouchMoveEventListener : ([@bs.as "touchmove"] _, Dom.touchEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addTouchMoveEventListenerWithOptions : ([@bs.as "touchmove"] _, Dom.touchEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addTouchMoveEventListenerUseCapture : ([@bs.as "touchmove"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeTouchMoveEventListener : ([@bs.as "touchmove"] _, Dom.touchEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeTouchMoveEventListenerWithOptions : ([@bs.as "touchmove"] _, Dom.touchEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeTouchMoveEventListenerUseCapture : ([@bs.as "touchmove"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addTouchStartEventListener : ([@bs.as "touchstart"] _, Dom.touchEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addTouchStartEventListenerWithOptions : ([@bs.as "touchstart"] _, Dom.touchEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addTouchStartEventListenerUseCapture : ([@bs.as "touchstart"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeTouchStartEventListener : ([@bs.as "touchstart"] _, Dom.touchEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeTouchStartEventListenerWithOptions : ([@bs.as "touchstart"] _, Dom.touchEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeTouchStartEventListenerUseCapture : ([@bs.as "touchstart"] _, Dom.touchEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  /* Animation */

  [@bs.send.pipe : T.t] external addAnimationCancelEventListener : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addAnimationCancelEventListenerWithOptions : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addAnimationCancelEventListenerUseCapture : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeAnimationCancelEventListener : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeAnimationCancelEventListenerWithOptions : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeAnimationCancelEventListenerUseCapture : ([@bs.as "animationcancel"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addAnimationEndEventListener : ([@bs.as "animationend"] _, Dom.animationEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addAnimationEndEventListenerWithOptions : ([@bs.as "animationend"] _, Dom.animationEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addAnimationEndEventListenerUseCapture : ([@bs.as "animationend"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeAnimationEndEventListener : ([@bs.as "animationend"] _, Dom.animationEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeAnimationEndEventListenerWithOptions : ([@bs.as "animationend"] _, Dom.animationEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeAnimationEndEventListenerUseCapture : ([@bs.as "animationend"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addAnimationIterationEventListener : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addAnimationIterationEventListenerWithOptions : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addAnimationIterationEventListenerUseCapture : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeAnimationIterationEventListener : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeAnimationIterationEventListenerWithOptions : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeAnimationIterationEventListenerUseCapture : ([@bs.as "animationiteration"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";

  [@bs.send.pipe : T.t] external addAnimationStartEventListener : ([@bs.as "animationstart"] _, Dom.animationEvent => unit) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external addAnimationStartEventListenerWithOptions : ([@bs.as "animationstart"] _, Dom.animationEvent => unit, {. "capture": bool, "once": bool, "passive": bool}) => unit = "addEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external addAnimationStartEventListenerUseCapture : ([@bs.as "animationstart"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "addEventListener";
  [@bs.send.pipe : T.t] external removeAnimationStartEventListener : ([@bs.as "animationstart"] _, Dom.animationEvent => unit) => unit = "removeEventListener";
  [@bs.send.pipe : T.t] external removeAnimationStartEventListenerWithOptions : ([@bs.as "animationstart"] _, Dom.animationEvent => unit, {. "capture": bool, "passive": bool}) => unit = "removeEventListener"; /* not widely supported */
  [@bs.send.pipe : T.t] external removeAnimationStartEventListenerUseCapture : ([@bs.as "animationstart"] _, Dom.animationEvent => unit, [@bs.as {json|true|json}] _) => unit = "removeEventListener";
};

include Impl({ type nonrec t = Dom.eventTarget; });

external unsafeAsDocument : Dom.eventTarget => Dom.document = "%identity";
external unsafeAsElement : Dom.eventTarget => Dom.element = "%identity";
external unsafeAsWindow : Dom.eventTarget => Dom.window = "%identity";
