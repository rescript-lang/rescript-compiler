type t = Dom.relatedEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "RelatedEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "RelatedEvent";

[@bs.get] [@bs.return nullable] external relatedTarget : t => option(Dom.eventTarget) = "";
