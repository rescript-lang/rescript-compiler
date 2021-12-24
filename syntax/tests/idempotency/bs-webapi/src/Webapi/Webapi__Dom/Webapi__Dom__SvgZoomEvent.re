type t = Dom.svgZoomEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "SVGZoomEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "SVGZoomEvent";

[@bs.get] external zoomRectScreen : t => Dom.svgRect = "";
[@bs.get] external previousScale : t => float = "";
[@bs.get] external previousTranslate : t => Dom.svgPoint = "";
[@bs.get] external newScale : t => float = "";
[@bs.get] external newTranslate : t => Dom.svgPoint = "";
