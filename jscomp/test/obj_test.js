'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var shared = [
  "hi",
  "hello"
];

var shared$1 = [
  "hi",
  "add"
];

var $$class = CamlinternalOO.create_table(shared);

var ids = CamlinternalOO.get_method_labels($$class, shared);

var hi = ids[0];

var hello = ids[1];

CamlinternalOO.set_methods($$class, [
      hi,
      (function (self$1, x, y) {
          return x + y | 0;
        }),
      hello,
      (function (self$1, z) {
          return Curry._3(self$1[0][hi], self$1, 10, z);
        })
    ]);

CamlinternalOO.init_class($$class);

var vv = CamlinternalOO.create_object_opt(undefined, $$class);

var $$class$1 = CamlinternalOO.create_table([
      "x",
      "y"
    ]);

var ids$1 = CamlinternalOO.get_method_labels($$class$1, [
      "y",
      "x"
    ]);

var y = ids$1[0];

var x = ids$1[1];

CamlinternalOO.set_methods($$class$1, [
      x,
      (function (self$2) {
          return 3;
        }),
      y,
      (function (self$2) {
          return 32;
        })
    ]);

CamlinternalOO.init_class($$class$1);

var v = CamlinternalOO.create_object_opt(undefined, $$class$1);

var $$class$2 = CamlinternalOO.create_table([
      "hi",
      "id1",
      "id2",
      "hello"
    ]);

var ids$2 = CamlinternalOO.get_method_labels($$class$2, [
      "id2",
      "id1",
      "hi",
      "hello"
    ]);

var id2 = ids$2[0];

var id1 = ids$2[1];

var hi$1 = ids$2[2];

var hello$1 = ids$2[3];

CamlinternalOO.set_methods($$class$2, [
      hi$1,
      (function (self$3, v, z) {
          return v + z | 0;
        }),
      id1,
      (function (self$3) {
          return 3;
        }),
      id2,
      (function (self$3) {
          return 4;
        }),
      hello$1,
      (function (self$3, v) {
          return v;
        })
    ]);

CamlinternalOO.init_class($$class$2);

var u = CamlinternalOO.create_object_opt(undefined, $$class$2);

var $$class$3 = CamlinternalOO.create_table(["id"]);

var id = CamlinternalOO.get_method_label($$class$3, "id");

CamlinternalOO.set_method($$class$3, id, (function (self$4) {
        return "uu";
      }));

CamlinternalOO.init_class($$class$3);

var uu = CamlinternalOO.create_object_opt(undefined, $$class$3);

var $$class$4 = CamlinternalOO.create_table(["add"]);

var add = CamlinternalOO.get_method_label($$class$4, "add");

CamlinternalOO.set_method($$class$4, add, (function (self$5, x, y) {
        return x + y | 0;
      }));

CamlinternalOO.init_class($$class$4);

var uuu = CamlinternalOO.create_object_opt(undefined, $$class$4);

var $$class$5 = CamlinternalOO.create_table(shared$1);

var ids$3 = CamlinternalOO.get_method_labels($$class$5, shared$1);

var hi$2 = ids$3[0];

var add$1 = ids$3[1];

CamlinternalOO.set_methods($$class$5, [
      add$1,
      (function (self$6, x, y) {
          return x + y | 0;
        }),
      hi$2,
      (function (self$6, x) {
          return Curry._3(self$6[0][add$1], self$6, x, 32);
        })
    ]);

CamlinternalOO.init_class($$class$5);

var vvvv = CamlinternalOO.create_object_opt(undefined, $$class$5);

var suites_0 = [
  "single_obj",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: [
                3,
                32
              ],
              _1: [
                Caml_oo_curry.js1(120, 1, v),
                Caml_oo_curry.js1(121, 2, v)
              ]
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "single_obj_cache",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: [
                  3,
                  32
                ],
                _1: [
                  Caml_oo_curry.js1(120, 3, v),
                  Caml_oo_curry.js1(121, 4, v)
                ]
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "self_obj",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: 13,
                  _1: Caml_oo_curry.js2(616641298, 5, vv, 3)
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "uu_id",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: "uu",
                    _1: Caml_oo_curry.js1(23515, 6, uu)
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "uu_add",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: Caml_oo_curry.js3(4846113, 7, uuu, 1, 20),
                      _1: 21
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "v_add",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: Caml_oo_curry.js3(4846113, 8, vvvv, 3, 7),
                        _1: 10
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "u_id1",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: Caml_oo_curry.js1(5243894, 9, u),
                          _1: 3
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "u_id2",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: Caml_oo_curry.js1(5243895, 10, u),
                            _1: 4
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "u hi",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: Caml_oo_curry.js3(23297, 11, u, 1, 2),
                              _1: 3
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "u hello",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: Caml_oo_curry.js2(616641298, 12, u, 32),
                                _1: 32
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "v hi",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: Caml_oo_curry.js2(23297, 13, vvvv, 31),
                                  _1: 63
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: [
                        "uuu add",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: Caml_oo_curry.js3(4846113, 14, uuu, 3, 4),
                                    _1: 7
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: [
                          "v x",
                          (function (param) {
                              return {
                                      tag: /* Eq */0,
                                      _0: Caml_oo_curry.js1(120, 15, v),
                                      _1: 3
                                    };
                            })
                        ],
                        _1: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Obj_test", suites);

exports.vv = vv;
exports.v = v;
exports.u = u;
exports.uu = uu;
exports.uuu = uuu;
exports.vvvv = vvvv;
exports.suites = suites;
/* class Not a pure module */
