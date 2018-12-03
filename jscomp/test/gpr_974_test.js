'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

if (!Caml_obj.caml_equal(Caml_option.nullable_to_opt(""), "")) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_974_test.ml",
          5,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Caml_option.undefined_to_opt(""), "")) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_974_test.ml",
          6,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Caml_option.null_to_opt(""), "")) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "gpr_974_test.ml",
          7,
          4
        ]
      ];
}

/*  Not a pure module */
