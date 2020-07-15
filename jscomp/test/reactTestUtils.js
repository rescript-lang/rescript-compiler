'use strict';

var Curry = require("../../lib/js/curry.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_Option = require("../../lib/js/belt_Option.js");
var Caml_option = require("../../lib/js/caml_option.js");
var TestUtils = require("react-dom/test-utils");

function act(func) {
  var reactFunc = function () {
    Curry._1(func, undefined);
    
  };
  TestUtils.act(reactFunc);
  
}

function actAsync(func) {
  return TestUtils.act(function () {
              return Curry._1(func, undefined);
            });
}

function changeWithValue(element, value) {
  var $$event = {
    target: {
      value
    }
  };
  TestUtils.Simulate.change(element, $$event);
  
}

function changeWithChecked(element, value) {
  var $$event = {
    target: {
      checked: value
    }
  };
  TestUtils.Simulate.change(element, $$event);
  
}

var Simulate = {
  changeWithValue,
  changeWithChecked
};

function findBySelector(element, selector) {
  return element.querySelector(selector);
}

function findByAllSelector(element, selector) {
  return Array.from(element.querySelectorAll(selector));
}

function findBySelectorAndTextContent(element, selector, content) {
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), (function (node) {
                return node.textContent === content;
              }));
}

function findBySelectorAndPartialTextContent(element, selector, content) {
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), (function (node) {
                return node.textContent.includes(content);
              }));
}

var DOM = {
  findBySelector,
  findByAllSelector,
  findBySelectorAndTextContent,
  findBySelectorAndPartialTextContent
};

function prepareContainer(container, param) {
  var containerElement = document.createElement("div");
  Belt_Option.map(document.body, (function (body) {
          return body.appendChild(containerElement);
        }));
  container.contents = Caml_option.some(containerElement);
  
}

function cleanupContainer(container, param) {
  Belt_Option.map(container.contents, (function (prim) {
          prim.remove();
          
        }));
  container.contents = undefined;
  
}

function getContainer(container) {
  return Belt_Option.getExn(container.contents);
}

exports.act = act;
exports.actAsync = actAsync;
exports.Simulate = Simulate;
exports.DOM = DOM;
exports.prepareContainer = prepareContainer;
exports.cleanupContainer = cleanupContainer;
exports.getContainer = getContainer;
/* react-dom/test-utils Not a pure module */
