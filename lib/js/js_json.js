'use strict';

var Block = require("./block");

function reify_type(x) {
  return /* tuple */[
          typeof x === "string" ? /* String */0 : (
              typeof x === "number" ? /* Number */1 : (
                  typeof x === "boolean" ? /* Boolean */4 : (
                      x === null ? /* Null */5 : (
                          Array.isArray(x) ? /* Array */3 : /* Object */2
                        )
                    )
                )
            ),
          x
        ];
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return +(typeof x === "string");
    case 1 : 
        return +(typeof x === "number");
    case 2 : 
        if (x !== null && typeof x === "object") {
          return 1 - +Array.isArray(x);
        } else {
          return /* false */0;
        }
    case 3 : 
        return +Array.isArray(x);
    case 4 : 
        return +(typeof x === "boolean");
    case 5 : 
        return +(x === null);
    
  }
}

function parse(s) {
  try {
    return /* Ok */Block.__(0, [JSON.parse(s)]);
  }
  catch (e){
    return /* Error */Block.__(1, [String(e)]);
  }
}

function decodeBoolean(json) {
  if (typeof json === "boolean") {
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

function decodeString(json) {
  if (typeof json === "string") {
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

function decodeArray(json) {
  if (Array.isArray(json)) {
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

var reifyType = reify_type;

var Decode = 0;

var Encode = 0;

exports.reifyType     = reifyType;
exports.test          = test;
exports.parse         = parse;
exports.Decode        = Decode;
exports.Encode        = Encode;
exports.decodeBoolean = decodeBoolean;
exports.decodeNumber  = decodeNumber;
exports.decodeString  = decodeString;
exports.decodeNull    = decodeNull;
exports.decodeArray   = decodeArray;
exports.decodeObject  = decodeObject;
exports.reify_type    = reify_type;
/* No side effect */
