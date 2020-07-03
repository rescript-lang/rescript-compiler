module Impl = (T: {type t;}) => {
  type t_htmlElement = T.t;

  let ofElement: Dom.element => Js.null(t_htmlElement) = [%bs.raw
    {|
    function (element) {
      // BEWARE: Assumes "contentEditable" uniquely identifies an HTMLELement
      return element.contentEditable !== undefined ?  element : null;
    }
  |}
  ];
  [@deprecated "Consider using Element.asHtmlElement or Element.unsafeAsHtmlElement instead"]
  let ofElement: Dom.element => option(t_htmlElement) = (self) => Js.Null.toOption(ofElement(self));

  [@bs.get] external accessKey : t_htmlElement => string = "";
  [@bs.set] external setAccessKey : (t_htmlElement, string) => unit = "accessKey";
  [@bs.get] external accessKeyLabel : t_htmlElement => string = "";
  [@bs.get] external contentEditable : t_htmlElement => string /* enum */ = "";
  let contentEditable: t_htmlElement => Webapi__Dom__Types.contentEditable =
    (self) => Webapi__Dom__Types.decodeContentEditable(contentEditable(self));
  [@bs.set] external setContentEditable : (t_htmlElement, string /* enum */) => unit = "contentEditable";
  let setContentEditable: (t_htmlElement, Webapi__Dom__Types.contentEditable) => unit =
    (self, value) => setContentEditable(self, Webapi__Dom__Types.encodeContentEditable(value));
  [@bs.get] external isContentEditable : t_htmlElement => bool = "";
  [@bs.get] external contextMenu : t_htmlElement => Dom.htmlElement = ""; /* returns HTMLMenuElement */
  [@bs.set] external setContextMenu : (t_htmlElement, Dom.htmlElement) => unit = "contextMenu"; /* accepts and returns HTMLMenuElement */
  [@bs.get] external dataset : t_htmlElement => Dom.domStringMap = "";
  [@bs.get] external dir : t_htmlElement => string /* enum */ = "";
  let dir: t_htmlElement => Webapi__Dom__Types.dir = (self) => Webapi__Dom__Types.decodeDir(dir(self));
  [@bs.set] external setDir : (t_htmlElement, string /* enum */) => unit = "dir";
  let setDir: (t_htmlElement, Webapi__Dom__Types.dir) => unit =
    (self, value) => setDir(self, Webapi__Dom__Types.encodeDir(value));
  [@bs.get] external draggable : t_htmlElement => bool = "";
  [@bs.set] external setDraggable : (t_htmlElement, bool) => unit = "draggable";
  /*let setDraggable : t_htmlElement => bool => unit = fun self value => setDraggable self (Js.Boolean.to_js_boolean value);*/ /* temproarily removed to reduce codegen size */
  [@bs.get] external dropzone : t_htmlElement => Dom.domSettableTokenList = "";
  [@bs.get] external hidden : t_htmlElement => bool = "";
  [@bs.set] external setHidden : (t_htmlElement, bool) => unit = "hidden";
  /*let setHidden : t_htmlElement => bool => unit = fun self value => setHidden self (Js.Boolean.to_js_boolean value);*/ /* temproarily removed to reduce codegen size */
  [@bs.get] external itemScope : t_htmlElement => bool = ""; /* experimental */
  [@bs.set] external setItemScope : (t_htmlElement, bool) => unit = "itemScope";
  /*let setItemScope : t_htmlElement => bool => unit = fun self value => setItemScope self (Js.Boolean.to_js_boolean value);*//* experimental */ /* temproarily removed to reduce codegen size */
  [@bs.get] external itemType : t_htmlElement => Dom.domSettableTokenList = ""; /* experimental */
  [@bs.get] external itemId : t_htmlElement => string = ""; /* experimental */
  [@bs.set] external setItemId : (t_htmlElement, string) => unit = "itemId"; /* experimental */
  [@bs.get] external itemRef : t_htmlElement => Dom.domSettableTokenList = ""; /* experimental */
  [@bs.get] external itemProp : t_htmlElement => Dom.domSettableTokenList = ""; /* experimental */
  [@bs.get] external itemValue : t_htmlElement => Js.t({..}) = ""; /* experimental */
  [@bs.set] external setItemValue : (t_htmlElement, Js.t({..})) => unit = "itemValue"; /* experimental */
  [@bs.get] external lang : t_htmlElement => string = "";
  [@bs.set] external setLang : (t_htmlElement, string) => unit = "lang";
  [@bs.get] external offsetHeight : t_htmlElement => int = ""; /* experimental */
  [@bs.get] external offsetLeft : t_htmlElement => int = ""; /* experimental */
  [@bs.get] [@bs.return nullable] external offsetParent : t_htmlElement => option(Dom.element) = ""; /* experimental */
  [@bs.get] external offsetTop : t_htmlElement => int = ""; /* experimental, but widely supported */
  [@bs.get] external offsetWidth : t_htmlElement => int = ""; /* experimental */
  /*external properties : r => HTMLPropertiesCollection.t = "properties" [@@bs.get]; /* experimental */*/
  [@bs.get] external spellcheck : t_htmlElement => bool = "";
  [@bs.set] external setSpellcheck : (t_htmlElement, bool) => unit = "spellcheck" ;
  /*let setSpellcheck : t_htmlElement => bool => unit = fun self value => setSpellcheck self (Js.Boolean.to_js_boolean value);*/ /* temproarily removed to reduce codegen size */
  [@bs.get] external style : t_htmlElement => Dom.cssStyleDeclaration = "";
  [@bs.set] external setStyle : (t_htmlElement, Dom.cssStyleDeclaration) => unit = "style";
  [@bs.get] external tabIndex : t_htmlElement => int = "";
  [@bs.set] external setTabIndex : (t_htmlElement, int) => unit = "tabIndex";
  [@bs.get] external title : t_htmlElement => string = "";
  [@bs.set] external setTitle : (t_htmlElement, string) => unit = "title";
  [@bs.get] external translate : t_htmlElement => bool = ""; /* experimental */
  [@bs.set] external setTranslate : (t_htmlElement, bool) => unit = "translate"; /* experimental */
  /*let setTranslate : t_htmlElement => bool => unit = fun self value => setTranslate self (Js.Boolean.to_js_boolean value);*/ /* temproarily removed to reduce codegen size */

  [@bs.send.pipe : t_htmlElement] external blur : unit = "";
  [@bs.send.pipe : t_htmlElement] external click : unit = "";
  [@bs.send.pipe : t_htmlElement] external focus : unit = "";
  [@bs.send.pipe : t_htmlElement] external focusPreventScroll : ([@bs.as {json|{ "preventScroll": true }|json}] _) => unit = "focus";
  [@bs.send.pipe : t_htmlElement] external forceSpellCheck : unit = ""; /* experimental */


  /* TODO: element-spcific, should be pulled out */
  [@bs.get] external value : t_htmlElement => string = ""; /* HTMLInputElement */
  [@bs.get] external checked : t_htmlElement => bool = ""; /* HTMLInputElement */
  [@bs.get] external type_ : t_htmlElement => string = "type"; /* HTMLStyleElement */
  [@bs.set] external setType : (t_htmlElement, string) => unit = "type"; /* HTMLStyleElement */
  [@bs.get] external rel : t_htmlElement => string = ""; /* HTMLLinkElement */
  [@bs.set] external setRel : (t_htmlElement, string) => unit = "rel"; /* HTMLLinkElement */
  [@bs.get] external href : t_htmlElement => string = ""; /* HTMLLinkElement, HTMLAnchorElement */
  [@bs.set] external setHref : (t_htmlElement, string) => unit = "href"; /* HTMLLinkElement, HTMLAnchorElement */
};

/* TODO
module Tree (T: { type t; }) => {
  include ElementRe.Tree { type t = Type };
  include Impl { type t = Type };
};

include Tree { type t = Dom.htmlElement };
*/

type t = Dom.htmlElement;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__GlobalEventHandlers.Impl({ type nonrec t = t; });
include Webapi__Dom__Element.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });
