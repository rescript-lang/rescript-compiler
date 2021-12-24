type t = Dom.pageTransitionEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "PageTransitionEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "PageTransitionEvent";

[@bs.get] external persisted : t => bool = "";
