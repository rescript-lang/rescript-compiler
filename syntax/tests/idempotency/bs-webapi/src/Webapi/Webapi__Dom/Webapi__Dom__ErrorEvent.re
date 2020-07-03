type t = Dom.errorEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "ErrorEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "ErrorEvent";

[@bs.get] external message : t => string = "";
[@bs.get] external filename : t => string = "";
[@bs.get] external lineno : t => int = "";
[@bs.get] external colno : t => int = "";
[@bs.get] external error : t => Js.t({..}) = "";
