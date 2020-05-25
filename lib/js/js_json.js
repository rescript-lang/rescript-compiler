'use strict';

var Caml_option = require("./caml_option.js");

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return {
            TAG: /* JSONString */0,
            _0: x
          };
  } else if (ty === "number") {
    return {
            TAG: /* JSONNumber */1,
            _0: x
          };
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSONTrue */1;
    } else {
      return /* JSONFalse */0;
    }
  } else if (x === null) {
    return /* JSONNull */2;
  } else if (Array.isArray(x)) {
    return {
            TAG: /* JSONArray */3,
            _0: x
          };
  } else {
    return {
            TAG: /* JSONObject */2,
            _0: x
          };
  }
}

function test(x, v) {
  switch (v) {
    case /* String */0 :
        return typeof x === "string";
    case /* Number */1 :
        return typeof x === "number";
    case /* Object */2 :
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
        } else {
          return false;
        }
    case /* Array */3 :
        return Array.isArray(x);
    case /* Boolean */4 :
        return typeof x === "boolean";
    case /* Null */5 :
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
