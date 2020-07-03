type t = Dom.compositionEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "CompositionEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "CompositionEvent";

[@bs.get] external data : t => string = "";
