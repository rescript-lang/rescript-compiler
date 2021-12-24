type t = Dom.customEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "CustomEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "CustomEvent";
