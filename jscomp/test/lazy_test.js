'use strict';

var Mt = require("./mt.js");
var Lazy = require("../../lib/js/lazy.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var u = {
  contents: 3
};

var v = {
  LAZY_DONE: false,
  VAL: (function () {
      u.contents = 32;
      
    })
};

function lazy_test(param) {
  var h = u.contents;
  CamlinternalLazy.force(v);
  var g = u.contents;
  return [
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
        RE_EXN_ID: "Match_failure",
        _1: [
          "lazy_test.ml",
          11,
          8
        ],
        Error: new Error()
      };
}

var s = {
  contents: undefined
};

var set_true = {
  LAZY_DONE: false,
  VAL: (function () {
      s.contents = 1;
      
    })
};

var set_false = {
  LAZY_DONE: false,
  VAL: (function () {
      s.contents = undefined;
      
    })
};

var h;

try {
  h = f([
        set_true,
        set_false,
        s
      ]);
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn.RE_EXN_ID === "Match_failure") {
    h = 2;
  } else {
    throw exn;
  }
}

var u_v = {
  contents: 0
};

var u$1 = {
  LAZY_DONE: false,
  VAL: (function () {
      u_v.contents = 2;
      
    })
};

CamlinternalLazy.force(u$1);

var exotic = CamlinternalLazy.force;

var l_from_fun = {
  LAZY_DONE: false,
  VAL: (function () {
      return 3;
    })
};

var forward_test = {
  LAZY_DONE: false,
  VAL: (function () {
      var u = 3;
      u = u + 1 | 0;
      return u;
    })
};

var f005 = {
  LAZY_DONE: true,
  VAL: 6
};

var f006 = {
  LAZY_DONE: false,
  VAL: (function () {
      return function (param) {
        return 3;
      };
    })
};

var f007 = {
  LAZY_DONE: false,
  VAL: (function () {
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    })
};

var f008 = {
  LAZY_DONE: false,
  VAL: (function () {
      console.log("hi");
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    })
};

function a2(x) {
  return {
          LAZY_DONE: true,
          VAL: x
        };
}

var a3 = {
  LAZY_DONE: true,
  VAL: 3
};

var a4 = {
  LAZY_DONE: true,
  VAL: 3
};

var a5 = {
  LAZY_DONE: true,
  VAL: undefined
};

var a6 = {
  LAZY_DONE: true,
  VAL: undefined
};

var a7 = CamlinternalLazy.force(a5);

var a8 = CamlinternalLazy.force(a6);

Mt.from_pair_suites("Lazy_test", {
      hd: [
        "simple",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: lazy_test(undefined),
                    _1: [
                      3,
                      32
                    ]
                  };
          })
      ],
      tl: {
        hd: [
          "lazy_match",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: h,
                      _1: 2
                    };
            })
        ],
        tl: {
          hd: [
            "lazy_force",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: u_v.contents,
                        _1: 2
                      };
              })
          ],
          tl: {
            hd: [
              "lazy_from_fun",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: CamlinternalLazy.force(l_from_fun),
                          _1: 3
                        };
                })
            ],
            tl: {
              hd: [
                "lazy_from_val",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: CamlinternalLazy.force({
                                  LAZY_DONE: true,
                                  VAL: 3
                                }),
                            _1: 3
                          };
                  })
              ],
              tl: {
                hd: [
                  "lazy_from_val2",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: CamlinternalLazy.force(CamlinternalLazy.force({
                                        LAZY_DONE: true,
                                        VAL: {
                                          LAZY_DONE: true,
                                          VAL: 3
                                        }
                                      })),
                              _1: 3
                            };
                    })
                ],
                tl: {
                  hd: [
                    "lazy_from_val3",
                    (function (param) {
                        debugger;
                        return {
                                TAG: /* Eq */0,
                                _0: CamlinternalLazy.force(CamlinternalLazy.force({
                                          LAZY_DONE: true,
                                          VAL: forward_test
                                        })),
                                _1: 4
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "lazy_test.ml",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: a3,
                                  _1: a4
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "lazy_test.ml",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: a7,
                                    _1: undefined
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "lazy_test.ml",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: a8,
                                      _1: undefined
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "File \"lazy_test.ml\", line 78, characters 0-7",
                            (function (param) {
                                return {
                                        TAG: /* Ok */4,
                                        _0: Lazy.is_val({
                                              LAZY_DONE: true,
                                              VAL: 3
                                            })
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "File \"lazy_test.ml\", line 79, characters 0-7",
                              (function (param) {
                                  return {
                                          TAG: /* Ok */4,
                                          _0: !Lazy.is_val({
                                                LAZY_DONE: false,
                                                VAL: (function () {
                                                    throw {
                                                          RE_EXN_ID: "Not_found",
                                                          Error: new Error()
                                                        };
                                                  })
                                              })
                                        };
                                })
                            ],
                            tl: /* [] */0
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
    });

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
