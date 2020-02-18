

import * as Block from "./block.js";
import * as Caml_option from "./caml_option.js";

function classify(x) {
  var ty = typeof x;
  if (ty === "string") {
    return /* JSONString */Block.__(0, [x]);
  } else if (ty === "number") {
    return /* JSONNumber */Block.__(1, [x]);
  } else if (ty === "boolean") {
    if (x === true) {
      return /* JSONTrue */1;
    } else {
      return /* JSONFalse */0;
    }
  } else if (x === null) {
    return /* JSONNull */2;
  } else if (Array.isArray(x)) {
    return /* JSONArray */Block.__(3, [x]);
  } else {
    return /* JSONObject */Block.__(2, [x]);
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

export {
  classify ,
  test ,
  decodeString ,
  decodeNumber ,
  decodeObject ,
  decodeArray ,
  decodeBoolean ,
  decodeNull ,
  
}
/* No side effect */
