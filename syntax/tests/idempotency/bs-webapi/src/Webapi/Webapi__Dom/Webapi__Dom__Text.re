let ofNode = (node: Dom.node) : option('a) =>
  Webapi__Dom__Node.nodeType(node) == Text ? Some(Obj.magic(node)) : None;

module Impl = (T: {type t;}) => {
  let ofNode: Dom.node => option(T.t) = ofNode;

  [@bs.send.pipe : T.t] external splitText : (~offset: int, unit) => Dom.text = "";
  [@bs.get] external wholeText : T.t => string = "";
};

type t = Dom.text;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__CharacterData.Impl({ type nonrec t = t; });
include Webapi__Dom__NonDocumentTypeChildNode.Impl({ type nonrec t = t; });
include Webapi__Dom__ChildNode.Impl({ type nonrec t = t; });
include Webapi__Dom__Slotable.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });