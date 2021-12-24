type t = Dom.webGlContextEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "WebGLContextEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "WebGLContextEvent";

[@bs.get] external statusMessage : t => string = "";
