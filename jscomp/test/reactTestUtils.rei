let act: (unit => unit) => unit;

let actAsync: (unit => Js.Promise.t('a)) => Js.Promise.t(unit);

[@bs.module "react-dom/test-utils"]
external isElement: 'element => bool = "isElement";

[@bs.module "react-dom/test-utils"]
external isElementOfType: ('element, React.component('props)) => bool =
  "isElement";

[@bs.module "react-dom/test-utils"]
external isDOMComponent: 'element => bool = "isDOMComponent";

[@bs.module "react-dom/test-utils"]
external isCompositeComponent: 'element => bool = "isCompositeComponent";

[@bs.module "react-dom/test-utils"]
external isCompositeComponentWithType:
  ('element, React.component('props)) => bool =
  "isCompositeComponentWithType";

module Simulate: {
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external click: Dom.element => unit = "click";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external clickWithEvent: (Dom.element, 'event) => unit = "click";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external change: Dom.element => unit = "change";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external blur: Dom.element => unit = "blur";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external changeWithEvent: (Dom.element, 'event) => unit = "change";
  let changeWithValue: (Dom.element, string) => unit;
  let changeWithChecked: (Dom.element, bool) => unit;
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external canPlay: Dom.element => unit = "canPlay";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external timeUpdate: Dom.element => unit = "timeUpdate";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external ended: Dom.element => unit = "ended";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external focus: Dom.element => unit = "focus";
};

module DOM: {
  [@bs.return nullable] [@bs.get]
  external value: Dom.element => option(string) = "value";

  let findBySelector: (Dom.element, string) => option(Dom.element);
  let findByAllSelector: (Dom.element, string) => array(Dom.element);
  let findBySelectorAndTextContent:
    (Dom.element, string, string) => option(Dom.element);
  let findBySelectorAndPartialTextContent:
    (Dom.element, string, string) => option(Dom.element);
};

let prepareContainer: (ref(option(Dom.element)), unit) => unit;
let cleanupContainer: (ref(option(Dom.element)), unit) => unit;
let getContainer: ref(option(Dom.element)) => Dom.element;
