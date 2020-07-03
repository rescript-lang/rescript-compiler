/*
 * Spec: https://html.spec.whatwg.org/multipage/forms.html#the-form-element
 * MDN: https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement
 */

module Impl = (T: {type t;}) => {
  type t_htmlFormElement = T.t;

  /* TODO: elements: HTMLFormControlsCollection */
  [@bs.get] external length : t_htmlFormElement => int = "";
  [@bs.get] external name : t_htmlFormElement => string = "";
  [@bs.set] external setName : (t_htmlFormElement, string) => unit = "name";
  [@bs.get] external method : t_htmlFormElement => string = "";
  [@bs.set] external setMethod : (t_htmlFormElement, string) => unit = "method";
  [@bs.get] external target : t_htmlFormElement => string = "";
  [@bs.set] external setTarget : (t_htmlFormElement, string) => unit = "target";
  [@bs.get] external action : t_htmlFormElement => string = "";
  [@bs.set] external setAction : (t_htmlFormElement, string) => unit = "action";
  [@bs.get] external acceptCharset : t_htmlFormElement => string = "";
  [@bs.set] external setAcceptCharset : (t_htmlFormElement, string) => unit = "acceptCharset";
  [@bs.get] external autocomplete : t_htmlFormElement => string = "";
  [@bs.set] external setAutocomplete : (t_htmlFormElement, string) => unit = "autocomplete";
  [@bs.get] external noValidate : t_htmlFormElement => bool = "";
  [@bs.set] external setNoValidate : (t_htmlFormElement, bool) => unit = "noValidate";
  [@bs.get] external enctype : t_htmlFormElement => string = "";
  [@bs.set] external setEnctype : (t_htmlFormElement, string) => unit = "enctype";
  [@bs.get] external encoding : t_htmlFormElement => string = "";
  [@bs.set] external setEncoding : (t_htmlFormElement, string) => unit = "encoding";

  [@bs.send.pipe: t_htmlFormElement] external submit : unit = "";
  [@bs.send.pipe: t_htmlFormElement] external reset : unit = "";
  [@bs.send.pipe: t_htmlFormElement] external checkValidity : bool = "";
  [@bs.send.pipe: t_htmlFormElement] external reportValidity : bool = "";
};

type t = Dom.htmlFormElement;

include Webapi__Dom__EventTarget.Impl({ type nonrec t = t; });
include Webapi__Dom__Node.Impl({ type nonrec t = t; });
include Webapi__Dom__Element.Impl({ type nonrec t = t; });
include Webapi__Dom__HtmlElement.Impl({ type nonrec t = t; });
include Impl({ type nonrec t = t; });
