type t = Dom.documentFragment;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__NonElementParentNode.Impl({ type nonrec t = t; });
include Webapi__Dom__ParentNode.Impl({ type nonrec t = t; });
