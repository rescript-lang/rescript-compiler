type t = Dom.nodeIterator;

[@bs.get] external root : t => Dom.node = "";
[@bs.get] external referenceNode : t => Dom.node = "";
[@bs.get] external pointerBeforeReferenceNode : t => bool = "";
[@bs.get] external whatToShow : t => Webapi__Dom__Types.WhatToShow.t = "";
[@bs.get] [@bs.return nullable] external filter : t => option(Dom.nodeFilter) = "";

[@bs.send.pipe : t] [@bs.return nullable] external nextNode : option(Dom.node) = "";
[@bs.send.pipe : t] [@bs.return nullable] external previousNode : option(Dom.node) = "";
[@bs.send.pipe : t] external detach : unit = "";
