@@jsxConfig({version: 3})

module Select = {
  module type T = {
    type key
    type t
  }

  @react.component
  let make = (
    type a key,
    ~model as module(T: T with type t = a and type key = key),
    ~selected: option<key>,
    ~onChange: option<key> => unit,
    ~items: array<a>,
  ) => {
    let _ = (model, selected, onChange, items)
    <div />
  }
}

@@jsxConfig({version: 4, mode: "classic"})

module Select = {
  module type T = {
    type key
    type t
  }

  @react.component
  let make = (
    type a key,
    ~model as module(T: T with type t = a and type key = key),
    ~selected: option<key>,
    ~onChange: option<key> => unit,
    ~items: array<a>,
  ) => {
    let _ = (model, selected, onChange, items)
    <div />
  }
}

module External = {
  module type T = {
    type key
    type t
  }

  @react.component @module("c")
  external make: (
    ~model: module(T with type t = 'a and type key = 'key),
    ~selected: option<'key>,
    ~onChange: option<'key> => unit,
    ~items: array<'a>,
  ) => React.element = "default"
}
