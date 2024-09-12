

import * as React from "react";

function createElementWithKey(key, component, props) {
  return React.createElement(component, key !== undefined ? Object.assign({
      key: key
    }, props) : props);
}

function createElementVariadicWithKey(key, component, props, elements) {
  return React.createElement(component, key !== undefined ? Object.assign({
      key: key
    }, props) : props, ...elements);
}

export {
  createElementWithKey,
  createElementVariadicWithKey,
}
/* react Not a pure module */
