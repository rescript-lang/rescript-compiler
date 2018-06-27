'use strict';

var Block = require("./block.js");
var Js_primitive = require("./js_primitive.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return /* JSONString */Block.__(0, [x]);
  } else if (ty === "number") {
    return /* JSONNumber */Block.__(1, [x]);
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSONTrue */1;
    } else {
      return /* JSONFalse */0;
    }
  } else if (x === null) {
    return /* JSONNull */2;
  } else if (Array.isArray(x)) {
    return /* JSONArray */Block.__(3, [x]);
  } else {
    return /* JSONObject */Block.__(2, [x]);
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return typeof x === "string";
    case 1 : 
        return typeof x === "number";
    case 2 : 
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
        } else {
          return false;
        }
    case 3 : 
        return Array.isArray(x);
    case 4 : 
        return typeof x === "boolean";
    case 5 : 
        return x === null;
    
  }
}

function decodeString(json) {
  if (typeof json === "string") {
    return json;
  } else {
    return undefined;
  }
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return json;
  } else {
    return undefined;
  }
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return Js_primitive.some(json);
  } else {
    return undefined;
  }
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return json;
  } else {
    return undefined;
  }
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return json;
  } else {
    return undefined;
  }
}

function decodeNull(json) {
  if (json === null) {
    return null;
  } else {
    return undefined;
  }
}

exports.classify = classify;
exports.test = test;
exports.decodeString = decodeString;
exports.decodeNumber = decodeNumber;
exports.decodeObject = decodeObject;
exports.decodeArray = decodeArray;
exports.decodeBoolean = decodeBoolean;
exports.decodeNull = decodeNull;
/* No side effect */
