type t = Dom.processingInstruction;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__NonDocumentTypeChildNode.Impl({ type nonrec t = t; });
include Webapi__Dom__ChildNode.Impl({ type nonrec t = t; });

[@bs.get] external target : t => string = "";
