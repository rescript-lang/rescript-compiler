'use strict';

var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var shared = ["add"];

var $$class = CamlinternalOO.create_table([
      "hi",
      "id1",
      "id2",
      "hello"
    ]);

var ids = CamlinternalOO.get_method_labels($$class, [
      "id2",
      "id1",
      "hi",
      "hello"
    ]);

var id2 = ids[0];

var id1 = ids[1];

var hi = ids[2];

var hello = ids[3];

CamlinternalOO.set_methods($$class, /* array */[
      hi,
      (function (_, v, z) {
          return v + z | 0;
        }),
      id1,
      (function () {
          return 3;
        }),
      id2,
      (function () {
          return 4;
        }),
      hello,
      (function (_, v) {
          return v;
        })
    ]);

CamlinternalOO.init_class($$class);

var u = CamlinternalOO.create_object_opt(0, $$class);

var $$class$1 = CamlinternalOO.create_table(["id"]);

var id = CamlinternalOO.get_method_label($$class$1, "id");

CamlinternalOO.set_method($$class$1, id, (function () {
        return "uu";
      }));

CamlinternalOO.init_class($$class$1);

var uu = CamlinternalOO.create_object_opt(0, $$class$1);

var $$class$2 = CamlinternalOO.create_table(shared);

var add = CamlinternalOO.get_method_label($$class$2, "add");

CamlinternalOO.set_method($$class$2, add, (function (_, x, y) {
        return x + y | 0;
      }));

CamlinternalOO.init_class($$class$2);

var uuu = CamlinternalOO.create_object_opt(0, $$class$2);

var $$class$3 = CamlinternalOO.create_table(shared);

var add$1 = CamlinternalOO.get_method_label($$class$3, "add");

CamlinternalOO.set_method($$class$3, add$1, (function (_, x, y) {
        return x + y | 0;
      }));

CamlinternalOO.init_class($$class$3);

var v = CamlinternalOO.create_object_opt(0, $$class$3);

function test() {
  if (Caml_oo_curry.js1(23515, 1, uu) !== "uu") {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            21,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js3(4846113, 2, uuu, 1, 20) !== 21) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            22,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js3(4846113, 3, v, 3, 7) !== 10) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            23,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js1(5243894, 4, u) !== 3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            25,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js1(5243895, 5, u) !== 4) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            26,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js3(23297, 6, u, 1, 2) !== 3) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            27,
            4
          ]
        ];
  }
  if (Caml_oo_curry.js2(616641298, 7, u, 32) === 32) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_simple_obj.ml",
            28,
            4
          ]
        ];
  }
}

exports.u = u;
exports.uu = uu;
exports.uuu = uuu;
exports.v = v;
exports.test = test;
/* class Not a pure module */
