type t = Dom.domRect;

[@bs.new] external make : (~x: float, ~y: float, ~width: float, ~height: float) => t = "DOMRect"; /* experimental */

[@bs.get] external top : t => float = "";
[@bs.get] external bottom : t => float = "";
[@bs.get] external left : t => float = "";
[@bs.get] external right : t => float = "";
[@bs.get] external height : t => float = "";
[@bs.get] external width : t => float = "";
[@bs.get] external x : t => float = "";
[@bs.get] external y : t => float = "";
