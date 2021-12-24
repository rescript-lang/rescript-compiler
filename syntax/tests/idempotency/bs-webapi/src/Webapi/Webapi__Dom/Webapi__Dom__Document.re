module Impl = (T: {type t;}) => {
  external asDocument : T.t => Dom.document = "%identity";

  let asHtmlDocument: T.t => Js.null(Dom.htmlDocument) = [%bs.raw
    {|
    function (document) {
      return document.doctype.name === "html" ?  document : null;
    }
  |}
  ];
  [@deprecated "Will fail if no doctype is defined, consider using unsafeAsHtmlDocument instead"]
  let asHtmlDocument: T.t => option(Dom.htmlDocument) =
    (self) => Js.Null.toOption(asHtmlDocument(self));

  external unsafeAsHtmlDocument : T.t => Dom.htmlDocument = "%identity";

  let ofNode = (node: Dom.node) : option(T.t) =>
  Webapi__Dom__Node.nodeType(node) == Document ? Some(Obj.magic(node)) : None;

  [@bs.get] external characterSet : T.t => string = "";
  [@bs.get] external compatMode : T.t => string /* compatMode enum */ = ""; /* experimental */
  let compatMode: T.t => Webapi__Dom__Types.compatMode =
    (self) => Webapi__Dom__Types.decodeCompatMode(compatMode(self));
  [@bs.get] external doctype : T.t => Dom.documentType = "";
  [@bs.get] external documentElement : T.t => Dom.element = "";
  [@bs.get] external documentURI : T.t => string = "";
  [@bs.get] external hidden : T.t => bool = "";
  [@bs.get] external implementation : T.t => Dom.domImplementation = "";
  [@bs.get] external lastStyleSheetSet : T.t => string = "";
  [@bs.get] [@bs.return nullable] external pointerLockElement : T.t => option(Dom.element) = ""; /* experimental */

  [@bs.get] external preferredStyleSheetSet : T.t => string = "";
  [@bs.get] [@bs.return nullable] external scrollingElement : T.t => option(Dom.element) = "";
  [@bs.get] external selectedStyleSheetSet : T.t => string = "";
  [@bs.set] external setSelectedStyleSheetSet : (T.t, string) => unit = "selectedStyleSheetSet";
  [@bs.get] external styleSheets : T.t => array(Dom.cssStyleSheet) = ""; /* return StyleSheetList, not array */
  [@bs.get] external styleSheetSets : T.t => array(string) = "";
  [@bs.get] external visibilityState : T.t => string /* visibilityState enum */ = "";
  let visibilityState: T.t => Webapi__Dom__Types.visibilityState =
    (self) => Webapi__Dom__Types.decodeVisibilityState(visibilityState(self));

  [@bs.send.pipe : T.t] external adoptNode : Dom.element_like('a) => Dom.element_like('a) = "";
  [@bs.send.pipe : T.t] external createAttribute : string => Dom.attr = "";
  [@bs.send.pipe : T.t] external createAttributeNS : (string, string) => Dom.attr = "";
  [@bs.send.pipe : T.t] external createComment : string => Dom.comment = "";
  [@bs.send.pipe : T.t] external createDocumentFragment : Dom.documentFragment = "";
  [@bs.send.pipe : T.t] external createElement : string => Dom.element = "";
  [@bs.send.pipe : T.t] external createElementWithOptions : (string, Js.t({..})) => Dom.element = "createElement"; /* not widely supported */
  [@bs.send.pipe : T.t] external createElementNS : (string, string) => Dom.element = "";
  [@bs.send.pipe : T.t] external createElementNSWithOptions : (string, string, Js.t({..})) => Dom.element = "createElementNS"; /* not widely supported */
  [@bs.send.pipe : T.t] external createEvent : string /* large enum */ => Dom.event = ""; /* discouraged (but not deprecated) in favor of Event constructors */
  [@bs.send.pipe : T.t] external createNodeIterator : Dom.node_like('a) => Dom.nodeIterator = "";
  [@bs.send.pipe : T.t] external createNodeIteratorWithWhatToShow : (Dom.node_like('a), Webapi__Dom__Types.WhatToShow.t) => Dom.nodeIterator = "createNodeIterator";
  [@bs.send.pipe : T.t] external createNodeIteratorWithWhatToShowFilter : (Dom.node_like('a), Webapi__Dom__Types.WhatToShow.t, Dom.nodeFilter) => Dom.nodeIterator = "createNodeIterator"; /* createProcessingInstruction */
  [@bs.send.pipe : T.t] external createRange : Dom.range = "";
  [@bs.send.pipe : T.t] external createTextNode : string => Dom.text = "";
  [@bs.send.pipe : T.t] external createTreeWalker : Dom.element_like('a) => Dom.treeWalker = "";
  [@bs.send.pipe : T.t] external createTreeWalkerWithWhatToShow : (Dom.element_like('a), Webapi__Dom__Types.WhatToShow.t) => Dom.treeWalker = "createTreeWalker";
  [@bs.send.pipe : T.t] external createTreeWalkerWithWhatToShowFilter : (Dom.element_like('a), Webapi__Dom__Types.WhatToShow.t, Dom.nodeFilter) => Dom.treeWalker = "createTreeWalker";
  [@bs.send.pipe : T.t] external elementFromPoint : (int, int) => Dom.element = ""; /* experimental, but widely supported */
  [@bs.send.pipe : T.t] external elementsFromPoint : (int, int) => array(Dom.element) = ""; /* experimental */
  [@bs.send.pipe : T.t] external enableStyleSheetsForSet : string => unit = "";
  [@bs.send.pipe : T.t] external exitPointerLock : unit = ""; /* experimental */
  [@bs.send.pipe : T.t] external getAnimations : array(Dom.animation) = ""; /* experimental */
  [@bs.send.pipe : T.t] external getElementsByClassName : string => Dom.htmlCollection = "";
  [@bs.send.pipe : T.t] external getElementsByTagName : string => Dom.htmlCollection = "";
  [@bs.send.pipe : T.t] external getElementsByTagNameNS : (string, string) => Dom.htmlCollection = "";
  [@bs.send.pipe : T.t] external importNode : Dom.element_like('a) => Dom.element_like('a) = "";
  [@bs.send.pipe : T.t] external importNodeDeep : (Dom.element_like('a), [@bs.as {json|true|json}] _) => Dom.element_like('a) = "importNode";
  [@bs.send.pipe : T.t] external registerElement : (string, unit) => Dom.element = ""; /* experimental and deprecated in favor of customElements.define() */
  [@bs.send.pipe : T.t] external registerElementWithOptions : (string, Js.t({..}), unit) => Dom.element = "registerElement"; /* experimental and deprecated in favor of customElements.define() */

  /** XPath stuff */
  /* createExpression */
  /* createNSResolver */
  /* evaluate */
  
  /* GlobalEventHandlers interface */
};

type t = Dom.document;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__NonElementParentNode.Impl({ type nonrec t = t; });
include Webapi__Dom__DocumentOrShadowRoot.Impl({ type nonrec t = t; });
include Webapi__Dom__ParentNode.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });
