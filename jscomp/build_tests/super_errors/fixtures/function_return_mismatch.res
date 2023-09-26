type cleanup = unit => unit

let fnExpectingCleanup = (cb: unit => cleanup) => {
  let cleanup = cb()
  cleanup()
}

let x = fnExpectingCleanup(() => {
  123
})
