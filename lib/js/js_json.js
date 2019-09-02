'use strict';

var Caml_option = require("./caml_option.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return /* constructor */{
            tag: "JSONString",
            Arg0: x
          };
  } else if (ty === "number") {
    return /* constructor */{
            tag: "JSONNumber",
            Arg0: x
          };
  } else if (ty === "boolean") {
    if (x === true) {
      return "JSONTrue";
    } else {
      return "JSONFalse";
    }
  } else if (x === null) {
    return "JSONNull";
  } else if (Array.isArray(x)) {
    return /* constructor */{
            tag: "JSONArray",
            Arg0: x
          };
  } else {
    return /* constructor */{
            tag: "JSONObject",
            Arg0: x
          };
  }
}

function test(x, v) {
  switch (v) {
    case "String" :
        return typeof x === "string";
    case "Number" :
        return typeof x === "number";
    case "Object" :
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
        } else {
          return false;
        }
    case "Array" :
        return Array.isArray(x);
    case "Boolean" :
        return typeof x === "boolean";
    case "Null" :
        return x === null;
    
  }
}

function decodeString(json) {
  if (typeof json === "string") {
    return json;
  }
  
}

function decodeNumber(json) {
  if (typeof json === "number") {
    return json;
  }
  
}

function decodeObject(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return Caml_option.some(json);
  }
  
}

function decodeArray(json) {
  if (Array.isArray(json)) {
    return json;
  }
  
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
    return json;
  }
  
}

function decodeNull(json) {
  if (json === null) {
    return null;
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
