open Webapi.Dom;

[@bs.module "./prismjsWrapper"]
external highlightAllUnderJs: Dom.element => unit = "default";

let highlightAllUnder = elementId => {
  let wrapperElement = document |> Document.getElementById(elementId);

  switch (wrapperElement) {
  | Some(element) => highlightAllUnderJs(element)
  | None => ()
  };
};