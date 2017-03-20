'use strict';


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
        }
        else {
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

function stringOfJSON(json) {
  if (typeof json === "string") {
    return /* Some */[json];
  }
  else {
    return /* None */0;
  }
}

function numberOfJSON(json) {
  if (typeof json === "number") {
    return /* Some */[json];
  }
  else {
    return /* None */0;
  }
}

function objectOfJSON(json) {
  if (typeof json === "object" && !Array.isArray(json) && json !== null) {
    return /* Some */[json];
  }
  else {
    return /* None */0;
  }
}

function arrayOfJSON(json) {
  if (Array.isArray(json)) {
    return /* Some */[json];
  }
  else {
    return /* None */0;
  }
}

function booleanOfJSON(json) {
  if (typeof json === "boolean") {
    return /* Some */[json];
  }
  else {
    return /* None */0;
  }
}

function nullOfJSON(json) {
  if (json === null) {
    return /* Some */[null];
  }
  else {
    return /* None */0;
  }
}

var reifyType = reify_type;

exports.reifyType     = reifyType;
exports.test          = test;
exports.stringOfJSON  = stringOfJSON;
exports.numberOfJSON  = numberOfJSON;
exports.objectOfJSON  = objectOfJSON;
exports.arrayOfJSON   = arrayOfJSON;
exports.booleanOfJSON = booleanOfJSON;
exports.nullOfJSON    = nullOfJSON;
exports.reify_type    = reify_type;
/* No side effect */
