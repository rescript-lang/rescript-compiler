type t = Dom.documentType;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__ChildNode.Impl({ type nonrec t = t; });

[@bs.get] external name : t => string = "";
[@bs.get] external publicId : t => string = "";
[@bs.get] external systemId : t => string = "";
