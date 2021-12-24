type t = Dom.inputEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "InputEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "InputEvent";

[@bs.get] external data : t => string = "";
[@bs.get] external isComposing : t => bool = "";
