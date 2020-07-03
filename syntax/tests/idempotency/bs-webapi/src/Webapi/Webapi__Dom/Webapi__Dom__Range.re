type t = Dom.range;

[@bs.new] external make : unit => t = "Range"; /* experimental */

[@bs.get] external collapsed : t => bool = "";
[@bs.get] external commonAncestorContainer : t => Dom.node = "";
[@bs.get] external endContainer : t => Dom.node = "";
[@bs.get] external endOffset : t => int = "";
[@bs.get] external startContainer : t => Dom.node = "";
[@bs.get] external startOffset : t => int = "";

[@bs.send.pipe : t] external setStart : (Dom.node_like('a), int) => unit = "";
[@bs.send.pipe : t] external setEnd : (Dom.node_like('a), int) => unit = "";
[@bs.send.pipe : t] external setStartBefore : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external setStartAfter : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external setEndBefore : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external setEndAfter : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external selectNode : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external selectNodeContents : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external collapse : unit = "";
[@bs.send.pipe : t] external collapseToStart : ([@bs.as {json|true|json}] _) => unit = "collapse";
[@bs.send.pipe : t] external cloneContents : Dom.documentFragment = "";
[@bs.send.pipe : t] external deleteContents : unit = "";
[@bs.send.pipe : t] external extractContents : Dom.documentFragment = "";
[@bs.send.pipe : t] external insertNode : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external surroundContents : Dom.node_like('a) => unit = "";
[@bs.send.pipe : t] external compareBoundaryPoints : (int /* compareHow enum */, t) => int /* compareResult enum */ = "";
let compareBoundaryPoint: (Webapi__Dom__Types.compareHow, t, t) => Webapi__Dom__Types.compareResult =
  (how, range, self) =>
    Webapi__Dom__Types.decodeCompareResult(
      compareBoundaryPoints(Webapi__Dom__Types.encodeCompareHow(how), range, self)
    );
[@bs.send.pipe : t] external cloneRange : t = "";
[@bs.send.pipe : t] external detach : unit = "";
[@bs.send.pipe : t] external toString : string = "";
[@bs.send.pipe : t] external comparePoint : (Dom.node_like('a), int) => int /* compareRsult enum */ = "";
let comparePoint: (Dom.node_like('a), int, t) => Webapi__Dom__Types.compareResult =
  (node, offset, self) => Webapi__Dom__Types.decodeCompareResult(comparePoint(node, offset, self));
[@bs.send.pipe : t] external createContextualFragment : string => Dom.documentFragment = ""; /* experimental, but widely supported */
[@bs.send.pipe : t] external getBoundingClientRect : Dom.domRect = ""; /* experimental, but widely supported */
[@bs.send.pipe : t] external getClientRects : array(Dom.domRect) = ""; /* experimental, but widely supported */
[@bs.send.pipe : t] external intersectsNode : Dom.node_like('a) => bool = "";
[@bs.send.pipe : t] external isPointInRange : (Dom.node_like('a), int) => bool = "";
