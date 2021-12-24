type t = Dom.dragEvent;

include Webapi__Dom__Event.Impl({ type nonrec t = t; });
include Webapi__Dom__UiEvent.Impl({ type nonrec t = t; });
include Webapi__Dom__MouseEvent.Impl({ type nonrec t = t; });

[@bs.new] external make : string => t = "DragEvent";
[@bs.new] external makeWithOptions : (string, Js.t({..})) => t = "DragEvent";

[@bs.get] external dataTransfer : t => Dom.dataTransfer = "";
