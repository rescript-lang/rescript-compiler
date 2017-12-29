'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Js_primitive = require("../../lib/js/js_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

if (!Caml_obj.caml_equal(Js_primitive.null_undefined_to_opt(""), /* Some */[""])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "gpr_974_test.ml",
          5,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Js_primitive.undefined_to_opt(""), /* Some */[""])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "gpr_974_test.ml",
          6,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Js_primitive.null_to_opt(""), /* Some */[""])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "gpr_974_test.ml",
          7,
          4
        ]
      ];
}

/*  Not a pure module */
