

import * as Block from "./block.js";

function classify(x) {
  var ty = typeof x;
  if (ty === "undefined") {
    return /* JSUndefined */3;
  } else if (x === null) {
    return /* JSNull */2;
  } else if (ty === "number") {
    return /* JSNumber */Block.__(0, [x]);
  } else if (ty === "string") {
    return /* JSString */Block.__(1, [x]);
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSTrue */1;
    } else {
      return /* JSFalse */0;
    }
  } else if (ty === "function") {
    return /* JSFunction */Block.__(2, [x]);
  } else if (ty === "object") {
    return /* JSObject */Block.__(3, [x]);
  } else {
    return /* JSSymbol */Block.__(4, [x]);
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
