type t = Dom.focusEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "FocusEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "FocusEvent";

[@bs.get] [@bs.return nullable] external relatedTarget : t => option(Dom.eventTarget) = "";
