type t = Dom.beforeUnloadEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "BeforeUnloadEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "BeforeUnloadEvent";

[@bs.get] external returnValue : t => string = "";
