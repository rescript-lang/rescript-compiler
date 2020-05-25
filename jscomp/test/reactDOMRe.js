'use strict';

var Block = require("../../lib/js/block.js");
var React = require("react");
var ReactDom = require("react-dom");

function renderToElementWithClassName(reactElement, className) {
  var elements = document.getElementsByClassName(className);
  if (elements.length !== 0) {
    ReactDom.render(reactElement, elements[0]);
  } else {
    console.error("ReactDOMRe.renderToElementWithClassName: no element of class " + (className + " found in the HTML."));
  }
  
}

function renderToElementWithId(reactElement, id) {
  var element = document.getElementById(id);
  if (element == null) {
    console.error("ReactDOMRe.renderToElementWithId : no element of id " + (id + " found in the HTML."));
  } else {
    ReactDom.render(reactElement, element);
  }
  
}

function createRootWithClassName(className) {
  var elements = document.getElementsByClassName(className);
  if (elements.length !== 0) {
    return {
            tag: /* Ok */0,
            _0: ReactDom.createRoot(elements[0])
          };
  } else {
    return {
            tag: /* Error */1,
            _0: "ReactDOMRe.Unstable.createRootWithClassName: no element of class " + (className + " found in the HTML.")
          };
  }
}

function createRootWithId(id) {
  var element = document.getElementById(id);
  if (element == null) {
    return {
            tag: /* Error */1,
            _0: "ReactDOMRe.Unstable.createRootWithId: no element of id " + (id + " found in the HTML.")
          };
  } else {
    return {
            tag: /* Ok */0,
            _0: ReactDom.createRoot(element)
          };
  }
}

var Experimental = {
  createRootWithClassName: createRootWithClassName,
  createRootWithId: createRootWithId
};

function hydrateToElementWithClassName(reactElement, className) {
  var elements = document.getElementsByClassName(className);
  if (elements.length !== 0) {
    ReactDom.hydrate(reactElement, elements[0]);
  } else {
    console.error("ReactDOMRe.hydrateToElementWithClassName: no element of class " + (className + " found in the HTML."));
  }
  
}

function hydrateToElementWithId(reactElement, id) {
  var element = document.getElementById(id);
  if (element == null) {
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "ReactDOMRe.hydrateToElementWithId : no element of id " + (id + " found in the HTML."),
          Error: new Error()
        };
  }
  ReactDom.hydrate(reactElement, element);
  
}

var Ref = { };

function createElementVariadic(domClassName, props, children) {
  var variadicArguments = [
      domClassName,
      props
    ].concat(children);
  return React.createElement.apply(null, variadicArguments);
}

function unsafeAddProp(style, key, value) {
  var dict = { };
  dict[key] = value;
  return Object.assign({}, style, dict);
}

var Style = {
  unsafeAddProp: unsafeAddProp
};

exports.renderToElementWithClassName = renderToElementWithClassName;
exports.renderToElementWithId = renderToElementWithId;
exports.Experimental = Experimental;
exports.hydrateToElementWithClassName = hydrateToElementWithClassName;
exports.hydrateToElementWithId = hydrateToElementWithId;
exports.Ref = Ref;
exports.createElementVariadic = createElementVariadic;
exports.Style = Style;
/* react Not a pure module */
