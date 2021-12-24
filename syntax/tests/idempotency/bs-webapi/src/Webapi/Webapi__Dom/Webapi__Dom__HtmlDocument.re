module Impl = (T: {type t;}) => {
  type t_htmlDocument = T.t;

  [@bs.get] [@bs.return nullable] external activeElement : t_htmlDocument => option(Dom.element) = "";
  [@bs.get] [@bs.return nullable] external body : t_htmlDocument => option(Dom.element) = ""; /* returns option HTMLBodyElement */
  [@bs.set] external setBody : (t_htmlDocument, Dom.element) => unit = "body"; /* accepth HTMLBodyElement */
  [@bs.get] external cookie : t_htmlDocument => string = "";
  [@bs.set] external setCookie : (t_htmlDocument, string) => unit = "cookie";
  [@bs.get] [@bs.return nullable] external defaultView : t_htmlDocument => option(Dom.window) = "";
  [@bs.get] external designMode : t_htmlDocument => string /* designMode enum */ = "";
  let designMode: t_htmlDocument => Webapi__Dom__Types.designMode =
    (self) => Webapi__Dom__Types.decodeDesignMode(designMode(self));
  [@bs.set] external setDesignMode : (t_htmlDocument, string /* designMode enum */) => unit = "designMode";
  let setDesignMode: (t_htmlDocument, Webapi__Dom__Types.designMode) => unit =
    (self, value) => setDesignMode(self, Webapi__Dom__Types.encodeDesignMode(value));
  [@bs.get] external dir : t_htmlDocument => string /* dir enum */ = "";
  let dir: t_htmlDocument => Webapi__Dom__Types.dir = (self) => Webapi__Dom__Types.decodeDir(dir(self));
  [@bs.set] external setDir : (t_htmlDocument, string /* dir enum */) => unit = "dir";
  let setDir: (t_htmlDocument, Webapi__Dom__Types.dir) => unit =
    (self, value) => setDir(self, Webapi__Dom__Types.encodeDir(value));
  [@bs.get] [@bs.return nullable] external domain : t_htmlDocument => option(string) = "";
  [@bs.set] external setDomain : (t_htmlDocument, string) => unit = "domain";
  [@bs.get] external embeds : t_htmlDocument => Dom.nodeList = "";
  [@bs.get] external forms : t_htmlDocument => Dom.htmlCollection = "";
  [@bs.get] external head : t_htmlDocument => Dom.element = ""; /* returns HTMLHeadElement */
  [@bs.get] external images : t_htmlDocument => Dom.htmlCollection = "";
  [@bs.get] external lastModified : t_htmlDocument => string = "";
  [@bs.get] external links : t_htmlDocument => Dom.nodeList = "";
  [@bs.get] external location : t_htmlDocument => Dom.location = "";
  [@bs.set] external setLocation : (t_htmlDocument, string) => unit = "location";
  [@bs.get] external plugins : t_htmlDocument => Dom.htmlCollection = "";
  [@bs.get] external readyState : t_htmlDocument => string /* enum */ = "";
  let readyState: t_htmlDocument => Webapi__Dom__Types.readyState =
    (self) => Webapi__Dom__Types.decodeReadyState(readyState(self));
  [@bs.get] external referrer : t_htmlDocument => string = "";
  [@bs.get] external scripts : t_htmlDocument => Dom.htmlCollection = "";
  [@bs.get] external title : t_htmlDocument => string = "";
  [@bs.set] external setTitle : (t_htmlDocument, string) => unit = "title";
  [@bs.get] external url : t_htmlDocument => string = "URL";

  [@bs.send.pipe : t_htmlDocument] external close : unit = "";
  [@bs.send.pipe : t_htmlDocument] external execCommand : (string, bool, Js.null(string)) => bool = "";
  let execCommand: (string, bool, option(string), t_htmlDocument) => bool =
    (command, show, value, self) =>
      execCommand(command, show, Js.Null.fromOption(value), self);
  [@bs.send.pipe : t_htmlDocument] external getElementsByName : string => Dom.nodeList = "";
  [@bs.send.pipe : t_htmlDocument] external getSelection : Dom.selection = "";
  [@bs.send.pipe : t_htmlDocument] external hasFocus : bool = "";
  [@bs.send.pipe : t_htmlDocument] external open_ : unit = "open";
  [@bs.send.pipe : t_htmlDocument] external queryCommandEnabled : string => bool = "";
  [@bs.send.pipe : t_htmlDocument] external queryCommandIndeterm : string => bool = "";
  [@bs.send.pipe : t_htmlDocument] external queryCommandSupported : string => bool = "";
  [@bs.send.pipe : t_htmlDocument] external queryCommandValue : string => string = "";
  [@bs.send.pipe : t_htmlDocument] external write : string => unit = "";
  [@bs.send.pipe : t_htmlDocument] external writeln : string => unit = "";
};

type t = Dom.htmlDocument;

include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__GlobalEventHandlers.Impl({ type nonrec t = t; });
include Webapi__Dom__Document.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });
