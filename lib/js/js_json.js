'use strict';


function reify_type(x) {
  if (typeof x === "string") {
    return /* tuple */[
            /* String */0,
            x
          ];
  }
  else if (typeof x === "number") {
    return /* tuple */[
            /* Number */1,
            x
          ];
  }
  else if (typeof x === "boolean") {
    return /* tuple */[
            /* Boolean */4,
            x
          ];
  }
  else if (x === null) {
    return /* tuple */[
            /* Null */5,
            x
          ];
  }
  else if (Array.isArray(x)) {
    return /* tuple */[
            /* Array */3,
            x
          ];
  }
  else {
    return /* tuple */[
            /* Object */2,
            x
          ];
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return +(typeof x === "string");
    case 1 : 
        return +(typeof x === "number");
    case 2 : 
        if (x !== null && typeof x === "object") {
          return !Array.isArray(x);
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

exports.reify_type = reify_type;
exports.test       = test;
/* No side effect */
