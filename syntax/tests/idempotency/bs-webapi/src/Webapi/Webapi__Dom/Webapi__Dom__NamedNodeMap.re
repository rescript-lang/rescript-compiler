type t = Dom.namedNodeMap;

[@bs.get] external length : t => int = "";

[@bs.send.pipe : t] [@bs.return nullable] external item : int => option(Dom.attr) = "";
[@bs.send.pipe : t] [@bs.return nullable] external getNamedItem : string => option(Dom.attr) = "";
[@bs.send.pipe : t] [@bs.return nullable] external getNamedItemNS : (string, string) => option(Dom.attr) = "";
[@bs.send.pipe : t] external setNamedItem : Dom.attr => unit = "";
[@bs.send.pipe : t] external setNamedItemNS : Dom.attr => unit = "";
[@bs.send.pipe : t] external removeNamedItem : string => Dom.attr = "";
[@bs.send.pipe : t] external removeNamedItemNS : (string, string) => Dom.attr = "";

[@bs.val] [@bs.scope ("Array", "prototype", "slice")] external toArray : t => array(Dom.attr) = "call";
