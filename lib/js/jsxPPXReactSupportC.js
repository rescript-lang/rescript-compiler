'use strict';

var React = require("react");
var Caml_splice_call = require("./caml_splice_call.js");

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

exports.Jsx = Jsx;
exports.createElementWithKey = createElementWithKey;
exports.createElementVariadicWithKey = createElementVariadicWithKey;
/* react Not a pure module */
