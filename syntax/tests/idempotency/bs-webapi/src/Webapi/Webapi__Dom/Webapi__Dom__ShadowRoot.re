type t = Dom.shadowRoot;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__NonElementParentNode.Impl({ type nonrec t = t; });
include Webapi__Dom__DocumentOrShadowRoot.Impl({ type nonrec t = t; });
include Webapi__Dom__ParentNode.Impl({ type nonrec t = t; });

[@bs.get] external shadowRootMode : t => string = "";
let shadowRootMode: t => Webapi__Dom__Types.shadowRootMode =
  (self) => Webapi__Dom__Types.decodeShadowRootMode(shadowRootMode(self));
[@bs.get] external host : t => Dom.element = "";
