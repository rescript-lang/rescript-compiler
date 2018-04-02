'use strict';

var Block = require("./block.js");

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
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return /* Some */[json];
  } else {
    return /* None */0;
  }
}

function decodeNull(json) {
  if (json === null) {
    return /* Some */[null];
  } else {
    return /* None */0;
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
