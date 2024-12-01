// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as TestUtils from "react-dom/test-utils";

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
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), node => node.textContent === content);
}

function findBySelectorAndPartialTextContent(element, selector, content) {
  return Belt_Array.getBy(Array.from(element.querySelectorAll(selector)), node => node.textContent.includes(content));
}

let DOM = {
  findBySelector: findBySelector,
  findByAllSelector: findByAllSelector,
  findBySelectorAndTextContent: findBySelectorAndTextContent,
  findBySelectorAndPartialTextContent: findBySelectorAndPartialTextContent
};

function prepareContainer(container, param) {
  let containerElement = document.createElement("div");
  Belt_Option.map(document.body, body => body.appendChild(containerElement));
  container.contents = Primitive_option.some(containerElement);
}

function cleanupContainer(container, param) {
  Belt_Option.map(container.contents, prim => {
    prim.remove();
  });
  container.contents = undefined;
}

function getContainer(container) {
  return Belt_Option.getExn(container.contents);
}

export {
  act,
  actAsync,
  Simulate,
  DOM,
  prepareContainer,
  cleanupContainer,
  getContainer,
}
/* react-dom/test-utils Not a pure module */
