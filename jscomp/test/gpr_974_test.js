'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

if (!Caml_obj.caml_equal(Caml_option.nullable_to_opt(""), "")) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "gpr_974_test.ml",
          5,
          4
        ],
        Debug: "Assert_failure"
      };
}

if (!Caml_obj.caml_equal(Caml_option.undefined_to_opt(""), "")) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "gpr_974_test.ml",
          6,
          4
        ],
        Debug: "Assert_failure"
      };
}

if (!Caml_obj.caml_equal(Caml_option.null_to_opt(""), "")) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "gpr_974_test.ml",
          7,
          4
        ],
        Debug: "Assert_failure"
      };
}

/*  Not a pure module */
