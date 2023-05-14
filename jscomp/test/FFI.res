@@uncurried

let canUseCanvas: unit => bool = %ffi(`
  function canUseCanvas() {
    return !!document.createElement('canvas').getContext;
  }
`)
