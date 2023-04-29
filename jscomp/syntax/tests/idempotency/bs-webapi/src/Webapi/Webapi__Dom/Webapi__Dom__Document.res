module Impl = (
  T: {
    type t
  },
) => {
  external asDocument: T.t => Dom.document = "%identity"

  let asHtmlDocument: T.t => Js.null<Dom.htmlDocument> = %raw(`
    function (document) {
      return document.doctype.name === "html" ?  document : null;
    }
  `)
  @deprecated("Will fail if no doctype is defined, consider using unsafeAsHtmlDocument instead")
  let asHtmlDocument: T.t => option<Dom.htmlDocument> = self =>
    Js.Null.toOption(asHtmlDocument(self))

  external unsafeAsHtmlDocument: T.t => Dom.htmlDocument = "%identity"

  let ofNode = (node: Dom.node): option<T.t> =>
    Webapi__Dom__Node.nodeType(node) == Document ? Some(Obj.magic(node)) : None

  @get external characterSet: T.t => string = ""
  @get external compatMode: T.t => string /* compatMode enum */ = "" /* experimental */
  let compatMode: T.t => Webapi__Dom__Types.compatMode = self =>
    Webapi__Dom__Types.decodeCompatMode(compatMode(self))
  @get external doctype: T.t => Dom.documentType = ""
  @get external documentElement: T.t => Dom.element = ""
  @get external documentURI: T.t => string = ""
  @get external hidden: T.t => bool = ""
  @get external implementation: T.t => Dom.domImplementation = ""
  @get external lastStyleSheetSet: T.t => string = ""
  @get @return(nullable)
  external pointerLockElement: T.t => option<Dom.element> = "" /* experimental */

  @get external preferredStyleSheetSet: T.t => string = ""
  @get @return(nullable) external scrollingElement: T.t => option<Dom.element> = ""
  @get external selectedStyleSheetSet: T.t => string = ""
  @set external setSelectedStyleSheetSet: (T.t, string) => unit = "selectedStyleSheetSet"
  @get
  external styleSheets: T.t => array<Dom.cssStyleSheet> = "" /* return StyleSheetList, not array */
  @get external styleSheetSets: T.t => array<string> = ""
  @get external visibilityState: T.t => string /* visibilityState enum */ = ""
  let visibilityState: T.t => Webapi__Dom__Types.visibilityState = self =>
    Webapi__Dom__Types.decodeVisibilityState(visibilityState(self))

  @bs.send.pipe(: T.t) external adoptNode: Dom.element_like<'a> => Dom.element_like<'a> = ""
  @bs.send.pipe(: T.t) external createAttribute: string => Dom.attr = ""
  @bs.send.pipe(: T.t) external createAttributeNS: (string, string) => Dom.attr = ""
  @bs.send.pipe(: T.t) external createComment: string => Dom.comment = ""
  @bs.send.pipe(: T.t) external createDocumentFragment: Dom.documentFragment = ""
  @bs.send.pipe(: T.t) external createElement: string => Dom.element = ""
  @bs.send.pipe(: T.t)
  external createElementWithOptions: (string, {..}) => Dom.element =
    "createElement" /* not widely supported */
  @bs.send.pipe(: T.t) external createElementNS: (string, string) => Dom.element = ""
  @bs.send.pipe(: T.t)
  external createElementNSWithOptions: (string, string, {..}) => Dom.element =
    "createElementNS" /* not widely supported */
  @bs.send.pipe(: T.t)
  external createEvent: string /* large enum */ => Dom.event =
    "" /* discouraged (but not deprecated) in favor of Event constructors */
  @bs.send.pipe(: T.t) external createNodeIterator: Dom.node_like<'a> => Dom.nodeIterator = ""
  @bs.send.pipe(: T.t)
  external createNodeIteratorWithWhatToShow: (
    Dom.node_like<'a>,
    Webapi__Dom__Types.WhatToShow.t,
  ) => Dom.nodeIterator = "createNodeIterator"
  @bs.send.pipe(: T.t)
  external createNodeIteratorWithWhatToShowFilter: (
    Dom.node_like<'a>,
    Webapi__Dom__Types.WhatToShow.t,
    Dom.nodeFilter,
  ) => Dom.nodeIterator = "createNodeIterator" /* createProcessingInstruction */
  @bs.send.pipe(: T.t) external createRange: Dom.range = ""
  @bs.send.pipe(: T.t) external createTextNode: string => Dom.text = ""
  @bs.send.pipe(: T.t) external createTreeWalker: Dom.element_like<'a> => Dom.treeWalker = ""
  @bs.send.pipe(: T.t)
  external createTreeWalkerWithWhatToShow: (
    Dom.element_like<'a>,
    Webapi__Dom__Types.WhatToShow.t,
  ) => Dom.treeWalker = "createTreeWalker"
  @bs.send.pipe(: T.t)
  external createTreeWalkerWithWhatToShowFilter: (
    Dom.element_like<'a>,
    Webapi__Dom__Types.WhatToShow.t,
    Dom.nodeFilter,
  ) => Dom.treeWalker = "createTreeWalker"
  @bs.send.pipe(: T.t)
  external elementFromPoint: (int, int) => Dom.element = "" /* experimental, but widely supported */
  @bs.send.pipe(: T.t)
  external elementsFromPoint: (int, int) => array<Dom.element> = "" /* experimental */
  @bs.send.pipe(: T.t) external enableStyleSheetsForSet: string => unit = ""
  @bs.send.pipe(: T.t) external exitPointerLock: unit = "" /* experimental */
  @bs.send.pipe(: T.t) external getAnimations: array<Dom.animation> = "" /* experimental */
  @bs.send.pipe(: T.t) external getElementsByClassName: string => Dom.htmlCollection = ""
  @bs.send.pipe(: T.t) external getElementsByTagName: string => Dom.htmlCollection = ""
  @bs.send.pipe(: T.t) external getElementsByTagNameNS: (string, string) => Dom.htmlCollection = ""
  @bs.send.pipe(: T.t) external importNode: Dom.element_like<'a> => Dom.element_like<'a> = ""
  @bs.send.pipe(: T.t)
  external importNodeDeep: (Dom.element_like<'a>, @as(json`true`) _) => Dom.element_like<'a> =
    "importNode"
  @bs.send.pipe(: T.t)
  external registerElement: (string, unit) => Dom.element =
    "" /* experimental and deprecated in favor of customElements.define() */
  @bs.send.pipe(: T.t)
  external registerElementWithOptions: (string, {..}, unit) => Dom.element =
    "registerElement" /* experimental and deprecated in favor of customElements.define() */

  @@ocaml.doc(
    " XPath stuff "
    /* createExpression */
    /* createNSResolver */
    /* evaluate */

    /* GlobalEventHandlers interface */
  )
}

type t = Dom.document

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__NonElementParentNode.Impl({
  type t = t
})
include Webapi__Dom__DocumentOrShadowRoot.Impl({
  type t = t
})
include Webapi__Dom__ParentNode.Impl({
  type t = t
})
include Impl({
  type t = t
})
