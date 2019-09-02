'use strict';

var Caml_obj_extern = require("../../lib/js/caml_obj_extern.js");

function f(obj) {
  if (typeof obj === "function") {
    return /* () */0;
  } else {
    var size = Caml_obj_extern.size_of_t(obj);
    if (size !== undefined) {
      console.log(size);
      return /* () */0;
    } else {
      return /* () */0;
    }
  }
}

exports.f = f;
/* No side effect */
