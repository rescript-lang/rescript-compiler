'use strict';

let Type = require("./Type.js");

function parseJsValue(value) {
  let value$1 = Type.Classify.classify(value);
  switch (value$1.TAG) {
    case "Bool" :
      return {
        NAME: "bool",
        VAL: value$1._0
      };
    case "String" :
      switch (value$1._0) {
        case "always" :
          return "always";
        case "auto" :
          return "auto";
        case "min2" :
          return "min2";
        default:
          return;
      }
    default:
      return;
  }
}

exports.parseJsValue = parseJsValue;
/* No side effect */
