type t;

[@bs.get] external contentRect: t => Dom.domRect = "";
[@bs.get] external target: t => Dom.element = "";
