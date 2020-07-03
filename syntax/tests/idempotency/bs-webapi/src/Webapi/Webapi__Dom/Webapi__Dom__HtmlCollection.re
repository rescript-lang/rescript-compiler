type t = Dom.htmlCollection;

[@bs.val] [@bs.scope ("Array", "prototype", "slice")] external toArray : t => array(Dom.element) = "call";

[@bs.get] external length : t => int = "";
[@bs.send.pipe : t] [@bs.return nullable] external item : int => option(Dom.element) = "";
[@bs.send.pipe : t] [@bs.return nullable] external namedItem : string => option(Dom.element) = "";
