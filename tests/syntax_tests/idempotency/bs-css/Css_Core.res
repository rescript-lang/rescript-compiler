module type CssImplementationIntf = {
  let mergeStyles: (. array<string>) => string
  let injectRule: (. Js.Json.t) => unit
  let injectRaw: (. string) => unit
  let make: (. Js.Json.t) => string
  let makeKeyFrames: (. Js.Dict.t<Js.Json.t>) => string
}
