/* Mixin */
module Impl = (
  T: {
    type t
  },
) => {
  @bs.send.pipe(: T.t)
  external addSelectionChangeEventListener: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external addSelectionChangeEventListenerWithOptions: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
    {"capture": bool, "once": bool, "passive": bool},
  ) => unit = "addEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external addSelectionChangeEventListenerUseCapture: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "addEventListener"
  @bs.send.pipe(: T.t)
  external removeSelectionChangeEventListener: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
  ) => unit = "removeEventListener"
  @bs.send.pipe(: T.t)
  external removeSelectionChangeEventListenerWithOptions: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
    {"capture": bool, "passive": bool},
  ) => unit = "removeEventListener" /* not widely supported */
  @bs.send.pipe(: T.t)
  external removeSelectionChangeEventListenerUseCapture: (
    @as("selectionchange") _,
    Dom.focusEvent => unit,
    @as(json`true`) _,
  ) => unit = "removeEventListener"
}
