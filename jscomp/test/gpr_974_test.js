'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

if (!Caml_obj.caml_equal(Caml_option.nullable_to_opt(""), "")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_974_test.ml",
          5,
          4
        ],
        Error: new Error()
      };
}

if (!Caml_obj.caml_equal(Caml_option.undefined_to_opt(""), "")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_974_test.ml",
          6,
          4
        ],
        Error: new Error()
      };
}

if (!Caml_obj.caml_equal(Caml_option.null_to_opt(""), "")) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "gpr_974_test.ml",
          7,
          4
        ],
        Error: new Error()
      };
}

/*  Not a pure module */
