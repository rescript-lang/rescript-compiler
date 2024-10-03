@genType.import("./shims/ReactEvent.shim")
type inputFocusEvent = ReactEvent.Focus.t

@genType.import("./MyInput") @react.component
external make: (~onFocus: inputFocusEvent => unit=?) => React.element = "default"
