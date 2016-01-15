// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_oo = require("../runtime/caml_oo");
var Caml_exceptions = require("../runtime/caml_exceptions");
var CamlinternalOO = require("../stdlib/camlinternalOO");

var shared = [
  0,
  "add"
];

var $$class = CamlinternalOO.create_table([
      0,
      "hi",
      "id1",
      "id2",
      "hello"
    ]);

var ids = CamlinternalOO.get_method_labels($$class, [
      0,
      "id2",
      "id1",
      "hi",
      "hello"
    ]);

var id2 = ids[1];

var id1 = ids[2];

var hi = ids[3];

var hello = ids[4];

CamlinternalOO.set_methods($$class, /* array */[
      hi,
      function (_, v, z) {
        return v + z;
      },
      id1,
      function () {
        return 3;
      },
      id2,
      function () {
        return 4;
      },
      hello,
      function (_, v) {
        return v;
      }
    ]);

function obj_init() {
  return CamlinternalOO.create_object_opt(0, $$class);
}

CamlinternalOO.init_class($$class);

var u = obj_init(0);

var $$class$1 = CamlinternalOO.create_table([
      0,
      "id"
    ]);

var id = CamlinternalOO.get_method_label($$class$1, "id");

CamlinternalOO.set_method($$class$1, id, function () {
      return "uu";
    });

function obj_init$1() {
  return CamlinternalOO.create_object_opt(0, $$class$1);
}

CamlinternalOO.init_class($$class$1);

var uu = obj_init$1(0);

var $$class$2 = CamlinternalOO.create_table(shared);

var add = CamlinternalOO.get_method_label($$class$2, "add");

CamlinternalOO.set_method($$class$2, add, function (_, x, y) {
      return x + y;
    });

function obj_init$2() {
  return CamlinternalOO.create_object_opt(0, $$class$2);
}

CamlinternalOO.init_class($$class$2);

var uuu = obj_init$2(0);

var $$class$3 = CamlinternalOO.create_table(shared);

var add$1 = CamlinternalOO.get_method_label($$class$3, "add");

CamlinternalOO.set_method($$class$3, add$1, function (_, x, y) {
      return x + y;
    });

function obj_init$3() {
  return CamlinternalOO.create_object_opt(0, $$class$3);
}

CamlinternalOO.init_class($$class$3);

var v = obj_init$3(0);

function test() {
  if (Caml_oo.caml_get_public_method(uu, 23515, 1)(uu) !== "uu") {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            21,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(uuu, 4846113, 2)(uuu, 1, 20) !== 21) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            22,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(v, 4846113, 3)(v, 3, 7) !== 10) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            23,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(u, 5243894, 4)(u) !== 3) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            25,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(u, 5243895, 5)(u) !== 4) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            26,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(u, 23297, 6)(u, 1, 2) !== 3) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_simple_obj.ml",
            27,
            4
          ]
        ];
  }
  if (Caml_oo.caml_get_public_method(u, 616641298, 7)(u, 32) === 32) {
    return 0;
  }
  else {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
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
