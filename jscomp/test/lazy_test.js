// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var CamlinternalLazy        = require("../stdlib/camlinternalLazy");
var Mt                      = require("./mt");

var u = [3];

var v = {
  0: function () {
    u[0] = 32;
    return /* () */0;
  },
  length: 1,
  tag: 246
};

function lazy_test() {
  var h = u[0];
  var tag = v.tag | 0;
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(v);
    }
    else {
      ;
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
    else {
      ;
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
      else {
        ;
      }
    }
    var match$3 = param[2][/* contents */0];
    if (match$3) {
      return 1;
    }
    else {
      throw [
            Caml_builtin_exceptions.match_failure,
            [
              "lazy_test.ml",
              11,
              8
            ]
          ];
    }
  }
  else {
    return 0;
  }
}

var s = [/* None */0];

var set_true = {
  0: function () {
    s[0] = /* Some */[1];
    return /* () */0;
  },
  length: 1,
  tag: 246
};

var set_false = {
  0: function () {
    s[0] = /* None */0;
    return /* () */0;
  },
  length: 1,
  tag: 246
};

var h;

try {
  h = f(/* tuple */[
        set_true,
        set_false,
        s
      ]);
}
catch (exn){
  if (exn[0] === Caml_builtin_exceptions.match_failure) {
    h = 2;
  }
  else {
    throw exn;
  }
}

function exotic(param) {
  var tag = param.tag | 0;
  if (tag === 250) {
    return param[0];
  }
  else if (tag === 246) {
    return CamlinternalLazy.force_lazy_block(param);
  }
  else {
    return param;
  }
}

Mt.from_pair_suites("lazy_test.ml", /* :: */[
      /* tuple */[
        "simple",
        function () {
          return /* Eq */{
                  0: lazy_test(/* () */0),
                  1: /* tuple */[
                    3,
                    32
                  ],
                  length: 2,
                  tag: 0
                };
        }
      ],
      /* :: */[
        /* tuple */[
          "lazy_match",
          function () {
            return /* Eq */{
                    0: h,
                    1: 2,
                    length: 2,
                    tag: 0
                  };
          }
        ],
        /* [] */0
      ]
    ]);

exports.u         = u;
exports.v         = v;
exports.lazy_test = lazy_test;
exports.f         = f;
exports.s         = s;
exports.set_true  = set_true;
exports.set_false = set_false;
exports.h         = h;
exports.exotic    = exotic;
/* h Not a pure module */
