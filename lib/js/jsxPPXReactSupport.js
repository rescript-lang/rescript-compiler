'use strict';

let React = require("react");

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

exports.createElementWithKey = createElementWithKey;
exports.createElementVariadicWithKey = createElementVariadicWithKey;
/* react Not a pure module */
