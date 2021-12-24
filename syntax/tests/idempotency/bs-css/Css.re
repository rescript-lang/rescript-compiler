include Css_Legacy_Core;
include Css_Colors;

include Css_Legacy_Core.Make({
  exception NotImplemented;

  let make = (. _) => raise(NotImplemented);
  let mergeStyles = (. _) => raise(NotImplemented);
  let injectRule = (. _) => ();
  let injectRaw = (. _) => ();
  let makeKeyFrames = (. _) => raise(NotImplemented);
});

external unsafeJsonToStyles: Js.Json.t => ReactDOMRe.Style.t = "%identity";

let style = rules => rules->toJson->unsafeJsonToStyles;