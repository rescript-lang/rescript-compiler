'use strict';

var Mt = require("./mt.js");
var Lazy = require("../../lib/js/lazy.js");
var Block = require("../../lib/js/block.js");
var Js_exn = require("../../lib/js/js_exn.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var u = [3];

var v = Block.__(246, [(function () {
        u[0] = 32;
        return /* () */0;
      })]);

function lazy_test() {
  var h = u[0];
  var tag = v.tag | 0;
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(v);
    }
    
  }
  var g = u[0];
  return /* tuple */[
          h,
          g
        ];
}

function f(param) {
  var match = param[0];
  var tag = match.tag | 0;
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(match);
    }
    
  }
  var match$1 = param[2][/* contents */0];
  if (match$1) {
    var match$2 = param[1];
    var tag$1 = match$2.tag | 0;
    if (tag$1 !== 250) {
      if (tag$1 === 246) {
        CamlinternalLazy.force_lazy_block(match$2);
      }
      
    }
    var match$3 = param[2][/* contents */0];
    if (match$3) {
      return 1;
    } else {
      throw [
            Caml_builtin_exceptions.match_failure,
            [
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

var s = [/* None */0];

var set_true = Block.__(246, [(function () {
        s[0] = /* Some */[1];
        return /* () */0;
      })]);

var set_false = Block.__(246, [(function () {
        s[0] = /* None */0;
        return /* () */0;
      })]);

var h;

try {
  h = f(/* tuple */[
        set_true,
        set_false,
        s
      ]);
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.match_failure) {
    h = 2;
  } else {
    throw exn;
  }
}

var u_v = [0];

var u$1 = Block.__(246, [(function () {
        u_v[0] = 2;
        return /* () */0;
      })]);

var tag = u$1.tag | 0;

if (tag !== 250) {
  if (tag === 246) {
    CamlinternalLazy.force_lazy_block(u$1);
  }
  
}

function exotic(param) {
  var tag = param.tag | 0;
  if (tag === 250) {
    return param[0];
  } else if (tag === 246) {
    return CamlinternalLazy.force_lazy_block(param);
  } else {
    return param;
  }
}

var l_from_fun = Lazy.from_fun((function () {
        return 3;
      }));

var forward_test = Block.__(246, [(function () {
        var u = 3;
        u = u + 1 | 0;
        return u;
      })]);

Mt.from_pair_suites("lazy_test.ml", /* :: */[
      /* tuple */[
        "simple",
        (function () {
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
          (function () {
              return /* Eq */Block.__(0, [
                        h,
                        2
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "lazy_force",
            (function () {
                return /* Eq */Block.__(0, [
                          u_v[0],
                          2
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "lazy_from_fun",
              (function () {
                  var tag = l_from_fun.tag | 0;
                  return /* Eq */Block.__(0, [
                            tag === 250 ? l_from_fun[0] : (
                                tag === 246 ? CamlinternalLazy.force_lazy_block(l_from_fun) : l_from_fun
                              ),
                            3
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "lazy_from_val",
                (function () {
                    var lzarg = Lazy.from_val(3);
                    var tag = lzarg.tag | 0;
                    return /* Eq */Block.__(0, [
                              tag === 250 ? lzarg[0] : (
                                  tag === 246 ? CamlinternalLazy.force_lazy_block(lzarg) : lzarg
                                ),
                              3
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "lazy_from_val2",
                  (function () {
                      var lzarg = Lazy.from_val(3);
                      var tag = lzarg.tag | 0;
                      var prim = tag === 250 ? lzarg[0] : (
                          tag === 246 ? CamlinternalLazy.force_lazy_block(lzarg) : lzarg
                        );
                      var tag$1 = prim.tag | 0;
                      return /* Eq */Block.__(0, [
                                tag$1 === 250 ? prim[0] : (
                                    tag$1 === 246 ? CamlinternalLazy.force_lazy_block(prim) : prim
                                  ),
                                3
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "lazy_from_val3",
                    (function () {
                        debugger;
                        var lzarg = Lazy.from_val(forward_test);
                        var tag = lzarg.tag | 0;
                        var prim = tag === 250 ? lzarg[0] : (
                            tag === 246 ? CamlinternalLazy.force_lazy_block(lzarg) : lzarg
                          );
                        var tag$1 = prim.tag | 0;
                        return /* Eq */Block.__(0, [
                                  tag$1 === 250 ? prim[0] : (
                                      tag$1 === 246 ? CamlinternalLazy.force_lazy_block(prim) : prim
                                    ),
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
/* h Not a pure module */
