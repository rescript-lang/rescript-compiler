module Impl = (T: {type t;}) => {
  [@bs.get] external detail : T.t => int = "";
  [@bs.get] external view : T.t => Dom.window = ""; /* technically returns a `WindowProxy` */
};

type t = Dom.uiEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "UIEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "UIEvent";
