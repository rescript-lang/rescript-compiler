type imageElement;

type canvasRenderingContext2D;

type canvasElement;

[@bs.val] external document : Dom.document = "";

[@bs.val] external window : Dom.window = "";

/* external createImg: (_ [@bs.as "img"]) -> document -> imageElement = "createElement" [@@bs.send] */
[@bs.send]
external createImg : (Dom.document, [@bs.as "img"] _) => imageElement =
  "createElement";

[@bs.val] external requestAnimationFrame : (float => unit) => unit = "";

[@bs.return null_to_opt] [@bs.send]
external getElementById : (Dom.document, string) => option(Dom.element) = "";

[@bs.send]
external addEventListener :
  (Dom.document, string, Dom.event_like('a) => bool, bool) => unit =
  "";

[@bs.send]
external addEventListenerImg :
  (imageElement, string, Dom.event_like('a) => bool, bool) => unit =
  "addEventListener";

/* unsafe casts */
external imageElementToJsObj : imageElement => Js.t({..}) = "%identity";

external canvasRenderingContext2DToJsObj :
  canvasRenderingContext2D => Js.t({..}) =
  "%identity";

external canvasElementToJsObj : canvasElement => Js.t({..}) = "%identity";

external keyboardEventToJsObj : Dom.keyboardEvent => Js.t({..}) = "%identity";

external elementToCanvasElement : Dom.element => canvasElement = "%identity";

external windowToJsObj : Dom.window => Js.t({..}) = "%identity";
