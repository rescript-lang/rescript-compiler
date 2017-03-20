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

function stringOfJson(json) {
  var match = reify_type(json);
  if (match[0] !== 0) {
    return /* None */0;
  }
  else {
    return /* Some */[match[1]];
  }
}

var reifyType = reify_type;

exports.reifyType    = reifyType;
exports.test         = test;
exports.stringOfJson = stringOfJson;
exports.reify_type   = reify_type;
/* No side effect */
