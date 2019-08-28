'use strict';

var Mt = require("./mt.js");
var Lazy = require("../../lib/js/lazy.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = /* record */{
  contents: 3
};

var v = Caml_obj.caml_lazy_make((function (param) {
        u.contents = 32;
        return /* () */0;
      }));

function lazy_test(param) {
  var h = u.contents;
  CamlinternalLazy.force(v);
  var g = u.contents;
  return /* tuple */[
          h,
          g
        ];
}

function f(param) {
  CamlinternalLazy.force(param[0]);
  var match = param[2].contents;
  if (match !== undefined) {
    CamlinternalLazy.force(param[1]);
    var match$1 = param[2].contents;
    if (match$1 !== undefined) {
      return 1;
    } else {
      throw [
            Caml_builtin_exceptions.match_failure,
            /* tuple */[
              "lazy_test.ml",
              11,
              8
            ]
          ];
    }
  } else {
    return 0;
  }
}

var s = /* record */{
  contents: undefined
};

var set_true = Caml_obj.caml_lazy_make((function (param) {
        s.contents = 1;
        return /* () */0;
      }));

var set_false = Caml_obj.caml_lazy_make((function (param) {
        s.contents = undefined;
        return /* () */0;
      }));

var h;

try {
  h = f(/* tuple */[
        set_true,
        set_false,
        s
      ]);
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.match_failure) {
    h = 2;
  } else {
    throw exn;
  }
}

var u_v = /* record */{
  contents: 0
};

var u$1 = Caml_obj.caml_lazy_make((function (param) {
        u_v.contents = 2;
        return /* () */0;
      }));

CamlinternalLazy.force(u$1);

var exotic = CamlinternalLazy.force;

var l_from_fun = Lazy.from_fun((function (param) {
        return 3;
      }));

var forward_test = Caml_obj.caml_lazy_make((function (param) {
        var u = /* record */{
          contents: 3
        };
        Pervasives.incr(u);
        return u.contents;
      }));

var f005 = Caml_obj.caml_lazy_make((function (param) {
        return 6;
      }));

var f006 = Caml_obj.caml_lazy_make((function (param) {
        return (function (param) {
            return 3;
          });
      }));

var f007 = Caml_obj.caml_lazy_make((function (param) {
        throw Caml_builtin_exceptions.not_found;
      }));

var f008 = Caml_obj.caml_lazy_make((function (param) {
        console.log("hi");
        throw Caml_builtin_exceptions.not_found;
      }));

Mt.from_pair_suites("Lazy_test", /* :: */[
      /* tuple */[
        "simple",
        (function (param) {
            return /* Eq */Block.__(0, [
                      lazy_test(/* () */0),
                      /* tuple */[
                        3,
                        32
                      ]
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "lazy_match",
          (function (param) {
              return /* Eq */Block.__(0, [
                        h,
                        2
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "lazy_force",
            (function (param) {
                return /* Eq */Block.__(0, [
                          u_v.contents,
                          2
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "lazy_from_fun",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            CamlinternalLazy.force(l_from_fun),
                            3
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "lazy_from_val",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              CamlinternalLazy.force(Lazy.from_val(3)),
                              3
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "lazy_from_val2",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                CamlinternalLazy.force(CamlinternalLazy.force(Lazy.from_val(3))),
                                3
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "lazy_from_val3",
                    (function (param) {
                        debugger;
                        return /* Eq */Block.__(0, [
                                  CamlinternalLazy.force(CamlinternalLazy.force(Lazy.from_val(forward_test))),
                                  4
                                ]);
                      })
                  ],
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

exports.v = v;
exports.lazy_test = lazy_test;
exports.f = f;
exports.s = s;
exports.set_true = set_true;
exports.set_false = set_false;
exports.h = h;
exports.u_v = u_v;
exports.u = u$1;
exports.exotic = exotic;
exports.l_from_fun = l_from_fun;
exports.forward_test = forward_test;
exports.f005 = f005;
exports.f006 = f006;
exports.f007 = f007;
exports.f008 = f008;
/* h Not a pure module */
