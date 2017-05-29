'use strict';


function reify_type(x) {
  if (typeof x === "undefined") {
    return /* tuple */[
            /* Undefined */0,
            x
          ];
  } else if (x === null) {
    return /* tuple */[
            /* Null */1,
            x
          ];
  } else if (typeof x === "number") {
    return /* tuple */[
            /* Number */3,
            x
          ];
  } else if (typeof x === "string") {
    return /* tuple */[
            /* String */4,
            x
          ];
  } else if (typeof x === "boolean") {
    return /* tuple */[
            /* Boolean */2,
            x
          ];
  } else if (typeof x === "function") {
    return /* tuple */[
            /* Function */5,
            x
          ];
  } else if (typeof x === "object") {
    return /* tuple */[
            /* Object */6,
            x
          ];
  } else {
    return /* tuple */[
            /* Symbol */7,
            x
          ];
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        return +(typeof x === "undefined");
    case 1 : 
        return +(x === null);
    case 2 : 
        return +(typeof x === "boolean");
    case 3 : 
        return +(typeof x === "number");
    case 4 : 
        return +(typeof x === "string");
    case 5 : 
        return +(typeof x === "function");
    case 6 : 
        return +(typeof x === "object");
    case 7 : 
        return +(typeof x === "symbol");
    
  }
}

exports.reify_type = reify_type;
exports.test       = test;
/* No side effect */
