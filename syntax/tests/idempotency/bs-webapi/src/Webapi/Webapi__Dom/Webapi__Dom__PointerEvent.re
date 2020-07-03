type t = Dom.pointerEvent;
type pointerId = Dom.eventPointerId;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });
include Webapi__Dom__MouseEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "PointerEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "PointerEvent";

[@bs.get] external pointerId : t => pointerId = "";
[@bs.get] external width : t => int = "";
[@bs.get] external height : t => int = "";
[@bs.get] external pressure : t => float = "";
[@bs.get] external tiltX : t => int = "";
[@bs.get] external tiltY : t => int = "";
[@bs.get] external pointerType : t => string /* pointerType enum */ = "";
let pointerType: t => Webapi__Dom__Types.pointerType =
  (self) => Webapi__Dom__Types.decodePointerType(pointerType(self));
[@bs.get] external isPrimary : t => bool = "";
