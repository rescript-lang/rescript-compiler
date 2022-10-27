

import * as Curry from "./curry.js";

function createElementWithKey(key, createElement, component, props) {
  return Curry._2(createElement, component, key !== undefined ? Object.assign({}, props, {
                    key: key
                  }) : props);
}

function createElementVariadicWithKey(key, createElementVariadic, component, props, elements) {
  return Curry._3(createElementVariadic, component, key !== undefined ? Object.assign({}, props, {
                    key: key
                  }) : props, elements);
}

export {
  createElementWithKey ,
  createElementVariadicWithKey ,
}
/* No side effect */
