// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_array = require("../../lib/js/caml_array.js");
let Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

let x = [
  1,
  2
];

let y;

try {
  y = Caml_array.get(x, 3);
} catch (raw_msg) {
  let msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
  if (msg.RE_EXN_ID === "Invalid_argument") {
    console.log(msg._1);
    y = 0;
  } else {
    throw new Error(msg.RE_EXN_ID, {
      cause: msg
    });
  }
}

exports.x = x;
exports.y = y;
/* y Not a pure module */
