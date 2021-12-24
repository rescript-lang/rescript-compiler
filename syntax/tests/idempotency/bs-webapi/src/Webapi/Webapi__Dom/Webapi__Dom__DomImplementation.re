type t = Dom.domImplementation;

[@bs.send.pipe : t] external createDocumentType : (~qualifiedName: string, ~publicId: string, ~systemId: string) => Dom.documentType = "";
[@bs.send.pipe : t] external createDocument : (Js.null(string), string, Js.null(Dom.documentType)) => Dom.xmlDocument = "";
let createDocument = (~namespace: option(string)=?, ~qualifiedName: string, ~docType: option(Dom.documentType)=?) =>
  createDocument(Js.Null.fromOption(namespace), qualifiedName, Js.Null.fromOption(docType));
[@bs.send.pipe : t] external createHTMLDocument : Dom.htmlDocument = "";
[@bs.send.pipe : t] external createHTMLDocumentWithTitle : string => Dom.htmlDocument = "createHTMLDocument";
[@bs.send.pipe : t] external hasFeature : bool = ""; /* useless; always returns true (this is exact wording from the actual spec) */
