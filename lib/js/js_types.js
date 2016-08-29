'use strict';


function reify_type(x) {
  if (typeof x === "undefined") {
    return /* tuple */[
            /* Undefined */0,
            x
          ];
  }
  else if (typeof x === "null") {
    return /* tuple */[
            /* Null */1,
            x
          ];
  }
  else if (typeof x === "number") {
    return /* tuple */[
            /* Number */3,
            x
          ];
  }
  else if (typeof x === "string") {
    return /* tuple */[
            /* String */4,
            x
          ];
  }
  else if (typeof x === "boolean") {
    return /* tuple */[
            /* Boolean */2,
            x
          ];
  }
  else if (typeof x === "function") {
    return /* tuple */[
            /* Function */5,
            x
          ];
  }
  else if (typeof x === "object") {
    return /* tuple */[
            /* Object */6,
            x
          ];
  }
  else {
    return /* tuple */[
            /* Symbol */7,
            x
          ];
  }
}

function test(x, v) {
  switch (v) {
    case 0 : 
        if (typeof x === "undefined") {
          return x;
        }
        else {
          return null;
        }
    case 1 : 
        if (typeof x === "Js.null") {
          return x;
        }
        else {
          return null;
        }
    case 2 : 
        if (typeof x === "boolean") {
          return x;
        }
        else {
          return null;
        }
    case 3 : 
        if (typeof x === "number") {
          return x;
        }
        else {
          return null;
        }
    case 4 : 
        if (typeof x === "string") {
          return x;
        }
        else {
          return null;
        }
    case 5 : 
        if (typeof x === "function") {
          return x;
        }
        else {
          return null;
        }
    case 6 : 
        if (typeof x === "object") {
          return x;
        }
        else {
          return null;
        }
    case 7 : 
        if (typeof x === "symbol") {
          return x;
        }
        else {
          return null;
        }
    
  }
}

exports.reify_type = reify_type;
exports.test       = test;
/* No side effect */
