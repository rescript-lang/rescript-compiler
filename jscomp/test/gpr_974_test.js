'use strict';

var Js_primitive            = require("../../lib/js/js_primitive");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Caml_obj                = require("../../lib/js/caml_obj");

if (!Caml_obj.caml_equal(Js_primitive.js_from_nullable_def(""), /* Some */[""])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "gpr_974_test.ml",
          5,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Js_primitive.js_from_def(""), /* Some */[""])) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "gpr_974_test.ml",
          6,
          4
        ]
      ];
}

if (!Caml_obj.caml_equal(Js_primitive.js_from_nullable(""), /* Some */[""])) {
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
