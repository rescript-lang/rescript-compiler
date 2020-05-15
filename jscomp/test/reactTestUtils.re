type undefined = Js.undefined(unit);

let undefined: undefined = Js.Undefined.empty;

[@bs.module "react-dom/test-utils"]
external reactAct: ((. unit) => undefined) => unit = "act";

let act: (unit => unit) => unit =
  func => {
    let reactFunc =
      (.) => {
        func();
        undefined;
      };
    reactAct(reactFunc);
  };

[@bs.module "react-dom/test-utils"]
external reactActAsync: ((. unit) => Js.Promise.t('a)) => Js.Promise.t(unit) =
  "act";

let actAsync = func => {
  let reactFunc =
    (.) => {
      func();
    };
  reactActAsync(reactFunc);
};

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

module Simulate = {
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
  let changeWithValue = (element, value) => {
    let event = {
      "target": {
        "value": value,
      },
    };
    changeWithEvent(element, event);
  };
  let changeWithChecked = (element, value) => {
    let event = {
      "target": {
        "checked": value,
      },
    };
    changeWithEvent(element, event);
  };
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external canPlay: Dom.element => unit = "canPlay";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external timeUpdate: Dom.element => unit = "timeUpdate";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external ended: Dom.element => unit = "ended";
  [@bs.module "react-dom/test-utils"] [@bs.scope "Simulate"]
  external focus: Dom.element => unit = "focus";
};

[@bs.val] external document: Dom.document = "document";

[@bs.send]
external querySelector: (Dom.element, string) => option(Dom.element) =
  "querySelector";

[@bs.send]
external querySelectorAll:
  (Dom.element, string) => Js.Array.array_like(Dom.element) =
  "querySelectorAll";

[@bs.get] external textContent: Dom.element => string = "textContent";
[@bs.get] external body: Dom.document => option(Dom.element) = "body";
[@bs.send]
external createElement: (Dom.document, string) => Dom.element =
  "createElement";
[@bs.send] external remove: Dom.element => unit = "remove";
[@bs.send]
external appendChild: (Dom.element, Dom.element) => Dom.element =
  "appendChild";

let querySelectorAll = (element, string) => {
  Js.Array.from(querySelectorAll(element, string));
};

module DOM = {
  open Belt;

  [@bs.return nullable] [@bs.get]
  external value: Dom.element => option(string) = "value";

  let findBySelector = (element, selector) =>
    querySelector(element, selector);

  let findByAllSelector = (element, selector) =>
    querySelectorAll(element, selector);

  let findBySelectorAndTextContent = (element, selector, content) =>
    querySelectorAll(element, selector)
    ->Array.getBy(node => node->textContent === content);

  let findBySelectorAndPartialTextContent = (element, selector, content) =>
    querySelectorAll(element, selector)
    ->Array.getBy(node => node->textContent->Js.String2.includes(content));
};

let prepareContainer = (container: ref(option(Dom.element)), ()) => {
  Belt.(
    {
      let containerElement = document->createElement("div");
      let _ =
        document
        ->body
        ->Option.map(body => body->appendChild(containerElement));
      container := Some(containerElement);
    }
  );
};

let cleanupContainer = (container: ref(option(Dom.element)), ()) => {
  Belt.(
    {
      let _ = container.contents->Option.map(remove);
      container := None;
    }
  );
};

let getContainer = container => {
  Belt.(container.contents->Option.getExn);
};
