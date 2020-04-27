'use strict';

var Mt = require("./mt.js");
var Lazy = require("../../lib/js/lazy.js");
var Block = require("../../lib/js/block.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = {
  contents: 3
};

var v = {
  tag: 246,
  value: (function () {
      u.contents = 32;
      
    })
};

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
  if (match === undefined) {
    return 0;
  }
  CamlinternalLazy.force(param[1]);
  var x = param[2].contents;
  if (x !== undefined) {
    return 1;
  }
  throw {
        CamlExt: Caml_builtin_exceptions.match_failure,
        _1: /* tuple */[
          "lazy_test.ml",
          11,
          8
        ]
      };
}

var s = {
  contents: undefined
};

var set_true = {
  tag: 246,
  value: (function () {
      s.contents = 1;
      
    })
};

var set_false = {
  tag: 246,
  value: (function () {
      s.contents = undefined;
      
    })
};

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
  if (exn.CamlExt === Caml_builtin_exceptions.match_failure) {
    h = 2;
  } else {
    throw exn;
  }
}

var u_v = {
  contents: 0
};

var u$1 = {
  tag: 246,
  value: (function () {
      u_v.contents = 2;
      
    })
};

CamlinternalLazy.force(u$1);

var exotic = CamlinternalLazy.force;

var l_from_fun = {
  tag: 246,
  value: (function () {
      return 3;
    })
};

var forward_test = {
  tag: 246,
  value: (function () {
      var u = 3;
      u = u + 1 | 0;
      return u;
    })
};

var f005 = {
  tag: 246,
  value: (function () {
      return 6;
    })
};

var f006 = {
  tag: 246,
  value: (function () {
      return (function (param) {
          return 3;
        });
    })
};

var f007 = {
  tag: 246,
  value: (function () {
      throw {
            CamlExt: Caml_builtin_exceptions.not_found
          };
    })
};

var f008 = {
  tag: 246,
  value: (function () {
      console.log("hi");
      throw {
            CamlExt: Caml_builtin_exceptions.not_found
          };
    })
};

var a2 = CamlinternalLazy.from_val;

var a4 = CamlinternalLazy.from_val(3);

var a7 = CamlinternalLazy.force(undefined);

var a8 = CamlinternalLazy.force(undefined);

Mt.from_pair_suites("Lazy_test", /* :: */[
      /* tuple */[
        "simple",
        (function (param) {
            return /* Eq */Block.__(0, [
                      lazy_test(undefined),
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
                  /* :: */[
                    /* tuple */[
                      "lazy_test.ml",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    3,
                                    a4
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "lazy_test.ml",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      a7,
                                      undefined
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "lazy_test.ml",
                          (function (param) {
                              return /* Eq */Block.__(0, [
                                        a8,
                                        undefined
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
          ]
        ]
      ]
    ]);

var a3 = 3;

var a5;

var a6;

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
exports.a2 = a2;
exports.a3 = a3;
exports.a4 = a4;
exports.a5 = a5;
exports.a6 = a6;
exports.a7 = a7;
exports.a8 = a8;
/* h Not a pure module */
