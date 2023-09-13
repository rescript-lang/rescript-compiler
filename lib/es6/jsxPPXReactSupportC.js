

import * as React from "react";
import * as Caml_splice_call from "./caml_splice_call.js";

function createElementWithKey(key, component, props) {
  return React.createElement(component, key !== undefined ? Object.assign({
                    key: key
                  }, props) : props);
}

function createElementVariadicWithKey(key, component, props, elements) {
  return Caml_splice_call.spliceApply(React.createElement, [
              component,
              key !== undefined ? Object.assign({
                      key: key
                    }, props) : props,
              elements
            ]);
}

var Jsx;

export {
  Jsx ,
  createElementWithKey ,
  createElementVariadicWithKey ,
}
/* react Not a pure module */
