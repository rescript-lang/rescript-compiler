'use strict';


function classify(x) {
  var ty = typeof x;
  if (ty === "undefined") {
    return "JSUndefined";
  } else if (x === null) {
    return "JSNull";
  } else if (ty === "number") {
    return /* constructor */{
            tag: "JSNumber",
            Arg0: x
          };
  } else if (ty === "string") {
    return /* constructor */{
            tag: "JSString",
            Arg0: x
          };
  } else if (ty === "boolean") {
    if (x === true) {
      return "JSTrue";
    } else {
      return "JSFalse";
    }
  } else if (ty === "function") {
    return /* constructor */{
            tag: "JSFunction",
            Arg0: x
          };
  } else if (ty === "object") {
    return /* constructor */{
            tag: "JSObject",
            Arg0: x
          };
  } else {
    return /* constructor */{
            tag: "JSSymbol",
            Arg0: x
          };
  }
}

function test(x, v) {
  switch (v) {
    case "Undefined" :
        return typeof x === "undefined";
    case "Null" :
        return x === null;
    case "Boolean" :
        return typeof x === "boolean";
    case "Number" :
        return typeof x === "number";
    case "String" :
        return typeof x === "string";
    case "Function" :
        return typeof x === "function";
    case "Object" :
        return typeof x === "object";
    case "Symbol" :
        return typeof x === "symbol";
    
  }
}

exports.test = test;
exports.classify = classify;
/* No side effect */
