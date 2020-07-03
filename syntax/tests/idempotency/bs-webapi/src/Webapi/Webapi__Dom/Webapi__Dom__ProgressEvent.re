type t = Dom.progressEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "ProgressEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "ProgressEvent";

[@bs.get] external lengthComputable : t => bool = "";
[@bs.get] external loaded : t => int = "";
[@bs.get] external total : t => int = "";
