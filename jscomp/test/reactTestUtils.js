// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Belt_Array = require("../../lib/js/belt_Array.js");
let Belt_Option = require("../../lib/js/belt_Option.js");
let Caml_option = require("../../lib/js/caml_option.js");
let TestUtils = require("react-dom/test-utils");

function act(func) {
  let reactFunc = () => {
    func();
  };
  TestUtils.act(reactFunc);
}

function actAsync(func) {
  return TestUtils.act(() => func());
}

function changeWithValue(element, value) {
  let event = {
    target: {
      value: value
    }
  };
  TestUtils.Simulate.change(element, event);
}

function changeWithChecked(element, value) {
  let event = {
    target: {
      checked: value
    }
  };
  TestUtils.Simulate.change(element, event);
}

let Simulate = {
  changeWithValue: changeWithValue,
  changeWithChecked: changeWithChecked
};

function findBySelector(element, selector) {
  return element.querySelector(selector);
}

function findByAllSelector(element, selector) {
  return Array.from(element.querySelectorAll(selector));
}

function findBySelectorAndTextContent(element, selector, content) {
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), (node => node.textContent === content));
}

function findBySelectorAndPartialTextContent(element, selector, content) {
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), (node => node.textContent.includes(content)));
}

let DOM = {
  findBySelector: findBySelector,
  findByAllSelector: findByAllSelector,
  findBySelectorAndTextContent: findBySelectorAndTextContent,
  findBySelectorAndPartialTextContent: findBySelectorAndPartialTextContent
};

function prepareContainer(container, param) {
  let containerElement = document.createElement("div");
  Belt_Option.map(document.body, (body => body.appendChild(containerElement)));
  container.contents = Caml_option.some(containerElement);
}

function cleanupContainer(container, param) {
  Belt_Option.map(container.contents, (prim => {
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
