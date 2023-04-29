module Impl = (
  T: {
    type t
  },
) => {
  external asEventTarget: T.t => Dom.eventTarget = "%identity"

  @bs.send.pipe(: T.t) external addEventListener: (string, Dom.event => unit) => unit = ""
  @bs.send.pipe(: T.t)
  external addEventListenerWithOptions: (
    string,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addEventListenerUseCapture: (string, Dom.event => unit, @as(json`true`) _) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t) external removeEventListener: (string, Dom.event => unit) => unit = ""
  @bs.send.pipe(: T.t)
  external removeEventListenerWithOptions: (
    string,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeEventListenerUseCapture: (string, Dom.event => unit, @as(json`true`) _) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t) external dispatchEvent: Dom.event_like<'a> => bool = ""

  @ocaml.doc("
   *  non-standard event-specific functions
   ")
  @bs.send.pipe(: T.t)
  external /* UI */

  addLoadEventListener: (@as("load") _, Dom.event => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addLoadEventListenerWithOptions: (
    @as("load") _,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addLoadEventListenerUseCapture: (
    @as("load") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeLoadEventListener: (@as("load") _, Dom.event => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeLoadEventListenerWithOptions: (
    @as("load") _,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeLoadEventListenerUseCapture: (
    @as("load") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addUnloadEventListener: (@as("unload") _, Dom.event => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addUnloadEventListenerWithOptions: (
    @as("unload") _,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addUnloadEventListenerUseCapture: (
    @as("unload") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeUnloadEventListener: (@as("unload") _, Dom.event => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeUnloadEventListenerWithOptions: (
    @as("unload") _,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeUnloadEventListenerUseCapture: (
    @as("unload") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addAbortEventListener: (@as("abort") _, Dom.event => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addAbortEventListenerWithOptions: (
    @as("abort") _,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addAbortEventListenerUseCapture: (
    @as("abort") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeAbortEventListener: (@as("abort") _, Dom.event => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeAbortEventListenerWithOptions: (
    @as("abort") _,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeAbortEventListenerUseCapture: (
    @as("abort") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addErrorEventListener: (@as("error") _, Dom.event => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addErrorEventListenerWithOptions: (
    @as("error") _,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addErrorEventListenerUseCapture: (
    @as("error") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeErrorEventListener: (@as("error") _, Dom.event => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeErrorEventListenerWithOptions: (
    @as("error") _,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeErrorEventListenerUseCapture: (
    @as("error") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addSelectEventListener: (@as("select") _, Dom.event => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addSelectEventListenerWithOptions: (
    @as("select") _,
    Dom.event => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addSelectEventListenerUseCapture: (
    @as("select") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeSelectEventListener: (@as("select") _, Dom.event => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeSelectEventListenerWithOptions: (
    @as("select") _,
    Dom.event => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeSelectEventListenerUseCapture: (
    @as("select") _,
    Dom.event => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Focus */

  @bs.send.pipe(: T.t)
  external addBlurEventListener: (@as("blur") _, Dom.focusEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addBlurEventListenerWithOptions: (
    @as("blur") _,
    Dom.focusEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addBlurEventListenerUseCapture: (
    @as("blur") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeBlurEventListener: (@as("blur") _, Dom.focusEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeBlurEventListenerWithOptions: (
    @as("blur") _,
    Dom.focusEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeBlurEventListenerUseCapture: (
    @as("blur") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addFocusEventListener: (@as("focus") _, Dom.focusEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addFocusEventListenerWithOptions: (
    @as("focus") _,
    Dom.focusEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addFocusEventListenerUseCapture: (
    @as("focus") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusEventListener: (@as("focus") _, Dom.focusEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusEventListenerWithOptions: (
    @as("focus") _,
    Dom.focusEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeFocusEventListenerUseCapture: (
    @as("focus") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addFocusInEventListener: (@as("focusin") _, Dom.focusEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addFocusInEventListenerWithOptions: (
    @as("focusin") _,
    Dom.focusEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addFocusInEventListenerUseCapture: (
    @as("focusin") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusInEventListener: (@as("focusin") _, Dom.focusEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusInEventListenerWithOptions: (
    @as("focusin") _,
    Dom.focusEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeFocusInEventListenerUseCapture: (
    @as("focusin") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addFocusOutEventListener: (@as("focusout") _, Dom.focusEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addFocusOutEventListenerWithOptions: (
    @as("focusout") _,
    Dom.focusEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addFocusOutEventListenerUseCapture: (
    @as("focusout") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusOutEventListener: (@as("focusout") _, Dom.focusEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeFocusOutEventListenerWithOptions: (
    @as("focusout") _,
    Dom.focusEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeFocusOutEventListenerUseCapture: (
    @as("focusout") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Mouse */

  @bs.send.pipe(: T.t)
  external addClickEventListener: (@as("click") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addClickEventListenerWithOptions: (
    @as("click") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addClickEventListenerUseCapture: (
    @as("click") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeClickEventListener: (@as("click") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeClickEventListenerWithOptions: (
    @as("click") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeClickEventListenerUseCapture: (
    @as("click") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDblClickEventListener: (@as("dblclick") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDblClickEventListenerWithOptions: (
    @as("dblclick") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDblClickEventListenerUseCapture: (
    @as("dblclick") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDblClickEventListener: (@as("dblclick") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDblClickEventListenerWithOptions: (
    @as("dblclick") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDblClickEventListenerUseCapture: (
    @as("dblclick") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseDownEventListener: (@as("mousedown") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseDownEventListenerWithOptions: (
    @as("mousedown") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseDownEventListenerUseCapture: (
    @as("mousedown") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseDownEventListener: (@as("mousedown") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseDownEventListenerWithOptions: (
    @as("mousedown") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseDownEventListenerUseCapture: (
    @as("mousedown") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseEnterEventListener: (@as("mouseenter") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseEnterEventListenerWithOptions: (
    @as("mouseenter") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseEnterEventListenerUseCapture: (
    @as("mouseenter") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseEnterEventListener: (@as("mouseenter") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseEnterEventListenerWithOptions: (
    @as("mouseenter") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseEnterEventListenerUseCapture: (
    @as("mouseenter") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseMoveEventListener: (@as("mousemove") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseMoveEventListenerWithOptions: (
    @as("mousemove") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseMoveEventListenerUseCapture: (
    @as("mousemove") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseMoveEventListener: (@as("mousemove") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseMoveEventListenerWithOptions: (
    @as("mousemove") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseMoveEventListenerUseCapture: (
    @as("mousemove") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseOutEventListener: (@as("mouseout") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseOutEventListenerWithOptions: (
    @as("mouseout") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseOutEventListenerUseCapture: (
    @as("mouseout") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseOutEventListener: (@as("mouseout") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseOutEventListenerWithOptions: (
    @as("mouseout") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseOutEventListenerUseCapture: (
    @as("mouseout") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseOverEventListener: (@as("mouseover") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseOverEventListenerWithOptions: (
    @as("mouseover") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseOverEventListenerUseCapture: (
    @as("mouseover") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseOverEventListener: (@as("mouseover") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseOverEventListenerWithOptions: (
    @as("mouseover") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseOverEventListenerUseCapture: (
    @as("mouseover") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addMouseUpEventListener: (@as("mouseup") _, Dom.mouseEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addMouseUpEventListenerWithOptions: (
    @as("mouseup") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addMouseUpEventListenerUseCapture: (
    @as("mouseup") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseUpEventListener: (@as("mouseup") _, Dom.mouseEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeMouseUpEventListenerWithOptions: (
    @as("mouseup") _,
    Dom.mouseEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeMouseUpEventListenerUseCapture: (
    @as("mouseup") _,
    Dom.mouseEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Wheel */

  @bs.send.pipe(: T.t)
  external addWheelEventListener: (@as("wheel") _, Dom.wheelEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addWheelEventListenerWithOptions: (
    @as("wheel") _,
    Dom.wheelEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addWheelEventListenerUseCapture: (
    @as("wheel") _,
    Dom.wheelEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeWheelEventListener: (@as("wheel") _, Dom.wheelEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeWheelEventListenerWithOptions: (
    @as("wheel") _,
    Dom.wheelEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeWheelEventListenerUseCapture: (
    @as("wheel") _,
    Dom.wheelEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Input */

  @bs.send.pipe(: T.t)
  external addBeforeInputEventListener: (@as("beforeinput") _, Dom.inputEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addBeforeInputEventListenerWithOptions: (
    @as("beforeinput") _,
    Dom.inputEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addBeforeInputEventListenerUseCapture: (
    @as("beforeinput") _,
    Dom.inputEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeBeforeInputEventListener: (@as("beforeinput") _, Dom.inputEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeBeforeInputEventListenerWithOptions: (
    @as("beforeinput") _,
    Dom.inputEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeBeforeInputEventListenerUseCapture: (
    @as("beforeinput") _,
    Dom.inputEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addInputEventListener: (@as("input") _, Dom.inputEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addInputEventListenerWithOptions: (
    @as("input") _,
    Dom.inputEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addInputEventListenerUseCapture: (
    @as("input") _,
    Dom.inputEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeInputEventListener: (@as("input") _, Dom.inputEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeInputEventListenerWithOptions: (
    @as("input") _,
    Dom.inputEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeInputEventListenerUseCapture: (
    @as("input") _,
    Dom.inputEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Keyboard */

  @bs.send.pipe(: T.t)
  external addKeyDownEventListener: (@as("keydown") _, Dom.keyboardEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addKeyDownEventListenerWithOptions: (
    @as("keydown") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addKeyDownEventListenerUseCapture: (
    @as("keydown") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyDownEventListener: (@as("keydown") _, Dom.keyboardEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyDownEventListenerWithOptions: (
    @as("keydown") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeKeyDownEventListenerUseCapture: (
    @as("keydown") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addKeyUpEventListener: (@as("keyup") _, Dom.keyboardEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addKeyUpEventListenerWithOptions: (
    @as("keyup") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addKeyUpEventListenerUseCapture: (
    @as("keyup") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyUpEventListener: (@as("keyup") _, Dom.keyboardEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyUpEventListenerWithOptions: (
    @as("keyup") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeKeyUpEventListenerUseCapture: (
    @as("keyup") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addKeyPressEventListener: (@as("keypress") _, Dom.keyboardEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addKeyPressEventListenerWithOptions: (
    @as("keypress") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addKeyPressEventListenerUseCapture: (
    @as("keypress") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyPressEventListener: (@as("keypress") _, Dom.keyboardEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeKeyPressEventListenerWithOptions: (
    @as("keypress") _,
    Dom.keyboardEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeKeyPressEventListenerUseCapture: (
    @as("keypress") _,
    Dom.keyboardEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Composition */

  @bs.send.pipe(: T.t)
  external addCompositionStartEventListener: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addCompositionStartEventListenerWithOptions: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addCompositionStartEventListenerUseCapture: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionStartEventListener: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionStartEventListenerWithOptions: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeCompositionStartEventListenerUseCapture: (
    @as("compositionstart") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addCompositionUpdateEventListener: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addCompositionUpdateEventListenerWithOptions: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addCompositionUpdateEventListenerUseCapture: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionUpdateEventListener: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionUpdateEventListenerWithOptions: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeCompositionUpdateEventListenerUseCapture: (
    @as("compositionupdate") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addCompositionEndEventListener: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addCompositionEndEventListenerWithOptions: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addCompositionEndEventListenerUseCapture: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionEndEventListener: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeCompositionEndEventListenerWithOptions: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeCompositionEndEventListenerUseCapture: (
    @as("compositionend") _,
    Dom.compositionEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Drag */

  @bs.send.pipe(: T.t)
  external addDragEventListener: (@as("drag") _, Dom.dragEvent => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragEventListenerWithOptions: (
    @as("drag") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragEventListenerUseCapture: (
    @as("drag") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEventListener: (@as("drag") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEventListenerWithOptions: (
    @as("drag") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragEventListenerUseCapture: (
    @as("drag") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragEndEventListener: (@as("dragend") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragEndEventListenerWithOptions: (
    @as("dragend") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragEndEventListenerUseCapture: (
    @as("dragend") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEndEventListener: (@as("dragend") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEndEventListenerWithOptions: (
    @as("dragend") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragEndEventListenerUseCapture: (
    @as("dragend") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragEnterEventListener: (@as("dragenter") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragEnterEventListenerWithOptions: (
    @as("dragenter") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragEnterEventListenerUseCapture: (
    @as("dragenter") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEnterEventListener: (@as("dragenter") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragEnterEventListenerWithOptions: (
    @as("dragenter") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragEnterEventListenerUseCapture: (
    @as("dragenter") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragExitEventListener: (@as("dragexit") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragExitEventListenerWithOptions: (
    @as("dragexit") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragExitEventListenerUseCapture: (
    @as("dragexit") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragExitEventListener: (@as("dragexit") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragExitEventListenerWithOptions: (
    @as("dragexit") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragExitEventListenerUseCapture: (
    @as("dragexit") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragLeaveEventListener: (@as("dragleave") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragLeaveEventListenerWithOptions: (
    @as("dragleave") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragLeaveEventListenerUseCapture: (
    @as("dragleave") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragLeaveEventListener: (@as("dragleave") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragLeaveEventListenerWithOptions: (
    @as("dragleave") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragLeaveEventListenerUseCapture: (
    @as("dragleave") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragOverEventListener: (@as("dragover") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragOverEventListenerWithOptions: (
    @as("dragover") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragOverEventListenerUseCapture: (
    @as("dragover") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragOverEventListener: (@as("dragover") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragOverEventListenerWithOptions: (
    @as("dragover") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragOverEventListenerUseCapture: (
    @as("dragover") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDragStartEventListener: (@as("dragstart") _, Dom.dragEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addDragStartEventListenerWithOptions: (
    @as("dragstart") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDragStartEventListenerUseCapture: (
    @as("dragstart") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDragStartEventListener: (@as("dragstart") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDragStartEventListenerWithOptions: (
    @as("dragstart") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDragStartEventListenerUseCapture: (
    @as("dragstart") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addDropEventListener: (@as("drop") _, Dom.dragEvent => unit) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addDropEventListenerWithOptions: (
    @as("drop") _,
    Dom.dragEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addDropEventListenerUseCapture: (
    @as("drop") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeDropEventListener: (@as("drop") _, Dom.dragEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeDropEventListenerWithOptions: (
    @as("drop") _,
    Dom.dragEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeDropEventListenerUseCapture: (
    @as("drop") _,
    Dom.dragEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Touch */

  @bs.send.pipe(: T.t)
  external addTouchCancelEventListener: (@as("touchcancel") _, Dom.touchEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addTouchCancelEventListenerWithOptions: (
    @as("touchcancel") _,
    Dom.touchEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addTouchCancelEventListenerUseCapture: (
    @as("touchcancel") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchCancelEventListener: (@as("touchcancel") _, Dom.touchEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchCancelEventListenerWithOptions: (
    @as("touchcancel") _,
    Dom.touchEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeTouchCancelEventListenerUseCapture: (
    @as("touchcancel") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addTouchEndEventListener: (@as("touchend") _, Dom.touchEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addTouchEndEventListenerWithOptions: (
    @as("touchend") _,
    Dom.touchEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addTouchEndEventListenerUseCapture: (
    @as("touchend") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchEndEventListener: (@as("touchend") _, Dom.touchEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchEndEventListenerWithOptions: (
    @as("touchend") _,
    Dom.touchEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeTouchEndEventListenerUseCapture: (
    @as("touchend") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addTouchMoveEventListener: (@as("touchmove") _, Dom.touchEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addTouchMoveEventListenerWithOptions: (
    @as("touchmove") _,
    Dom.touchEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addTouchMoveEventListenerUseCapture: (
    @as("touchmove") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchMoveEventListener: (@as("touchmove") _, Dom.touchEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchMoveEventListenerWithOptions: (
    @as("touchmove") _,
    Dom.touchEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeTouchMoveEventListenerUseCapture: (
    @as("touchmove") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addTouchStartEventListener: (@as("touchstart") _, Dom.touchEvent => unit) => unit =
    "addEventListener"
  @bs.send.pipe(: T.t)
  external addTouchStartEventListenerWithOptions: (
    @as("touchstart") _,
    Dom.touchEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addTouchStartEventListenerUseCapture: (
    @as("touchstart") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchStartEventListener: (@as("touchstart") _, Dom.touchEvent => unit) => unit =
    "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeTouchStartEventListenerWithOptions: (
    @as("touchstart") _,
    Dom.touchEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeTouchStartEventListenerUseCapture: (
    @as("touchstart") _,
    Dom.touchEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  /* Animation */

  @bs.send.pipe(: T.t)
  external addAnimationCancelEventListener: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addAnimationCancelEventListenerWithOptions: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addAnimationCancelEventListenerUseCapture: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationCancelEventListener: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationCancelEventListenerWithOptions: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeAnimationCancelEventListenerUseCapture: (
    @as("animationcancel") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addAnimationEndEventListener: (
    @as("animationend") _,
    Dom.animationEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addAnimationEndEventListenerWithOptions: (
    @as("animationend") _,
    Dom.animationEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addAnimationEndEventListenerUseCapture: (
    @as("animationend") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationEndEventListener: (
    @as("animationend") _,
    Dom.animationEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationEndEventListenerWithOptions: (
    @as("animationend") _,
    Dom.animationEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeAnimationEndEventListenerUseCapture: (
    @as("animationend") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addAnimationIterationEventListener: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addAnimationIterationEventListenerWithOptions: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addAnimationIterationEventListenerUseCapture: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationIterationEventListener: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationIterationEventListenerWithOptions: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeAnimationIterationEventListenerUseCapture: (
    @as("animationiteration") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"

  @bs.send.pipe(: T.t)
  external addAnimationStartEventListener: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addAnimationStartEventListenerWithOptions: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addAnimationStartEventListenerUseCapture: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationStartEventListener: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeAnimationStartEventListenerWithOptions: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeAnimationStartEventListenerUseCapture: (
    @as("animationstart") _,
    Dom.animationEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"
}

include Impl({
  type t = Dom.eventTarget
})

external unsafeAsDocument: Dom.eventTarget => Dom.document = "%identity"
external unsafeAsElement: Dom.eventTarget => Dom.element = "%identity"
external unsafeAsWindow: Dom.eventTarget => Dom.window = "%identity"
