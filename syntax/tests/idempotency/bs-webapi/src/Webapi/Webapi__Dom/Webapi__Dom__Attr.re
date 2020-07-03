type t = Dom.attr;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });

[@bs.get] external namespaceURI : t => string = "";
[@bs.get] external prefix : t => string = "";
[@bs.get] external localName : t => string = "";
[@bs.get] external name : t => string = "";
[@bs.get] external value : t => string = "";
[@bs.get] [@bs.return nullable] external ownerElement : t => option(Dom.element) = "";
[@bs.get] external specified : t => bool = ""; /* useless; always returns true (exact wording from spec) */
