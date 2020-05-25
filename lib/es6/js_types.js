

import * as Block from "./block.js";

function classify(x) {
  var ty = typeof x;
  if (ty === "undefined") {
    return /* JSUndefined */3;
  } else if (x === null) {
    return /* JSNull */2;
  } else if (ty === "number") {
    return {
            tag: /* JSNumber */0,
            _0: x
          };
  } else if (ty === "string") {
    return {
            tag: /* JSString */1,
            _0: x
          };
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSTrue */1;
    } else {
      return /* JSFalse */0;
    }
  } else if (ty === "function") {
    return {
            tag: /* JSFunction */2,
            _0: x
          };
  } else if (ty === "object") {
    return {
            tag: /* JSObject */3,
            _0: x
          };
  } else {
    return {
            tag: /* JSSymbol */4,
            _0: x
          };
  }
}

function test(x, v) {
  switch (v) {
    case /* Undefined */0 :
        return typeof x === "undefined";
    case /* Null */1 :
        return x === null;
    case /* Boolean */2 :
        return typeof x === "boolean";
    case /* Number */3 :
        return typeof x === "number";
    case /* String */4 :
        return typeof x === "string";
    case /* Function */5 :
        return typeof x === "function";
    case /* Object */6 :
        return typeof x === "object";
    case /* Symbol */7 :
        return typeof x === "symbol";
    
  }
}

export {
  test ,
  classify ,
  
}
/* No side effect */
