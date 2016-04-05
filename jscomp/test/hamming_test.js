// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_int64       = require("../runtime/caml_int64");
var Caml_obj         = require("../runtime/caml_obj");
var CamlinternalLazy = require("../stdlib/camlinternalLazy");
var Printf           = require("../stdlib/printf");
var Caml_curry       = require("../runtime/caml_curry");

var n0 = /* int64 */[
  0,
  0
];

var n1 = /* int64 */[
  1,
  0
];

var n2 = /* int64 */[
  2,
  0
];

var n3 = /* int64 */[
  3,
  0
];

var n5 = /* int64 */[
  5,
  0
];

function $percent(prim, prim$1) {
  return Caml_int64.mod_(prim, prim$1);
}

function $star(prim, prim$1) {
  return Caml_int64.mul(prim, prim$1);
}

function $slash(prim, prim$1) {
  return Caml_int64.div(prim, prim$1);
}

function $plus(prim, prim$1) {
  return Caml_int64.add(prim, prim$1);
}

var digit = /* int64 */[
  -1486618624,
  232830643
];

function mul(n, param) {
  var pl = param[0];
  return /* tuple */[
          Caml_int64.mod_(Caml_int64.mul(n, pl), digit),
          Caml_int64.add(Caml_int64.mul(n, param[1]), Caml_int64.div(Caml_int64.mul(n, pl), digit))
        ];
}

function cmp(param, param$1) {
  var ph = param$1[1];
  var pl = param$1[0];
  var nh = param[1];
  var nl = param[0];
  if (Caml_obj.caml_lessthan(nh, ph)) {
    return -1;
  }
  else if (Caml_obj.caml_greaterthan(nh, ph)) {
    return 1;
  }
  else if (Caml_obj.caml_lessthan(nl, pl)) {
    return -1;
  }
  else if (Caml_obj.caml_greaterthan(nl, pl)) {
    return 1;
  }
  else {
    return 0;
  }
}

function x2(p) {
  return mul(n2, p);
}

function x3(p) {
  return mul(n3, p);
}

function x5(p) {
  return mul(n5, p);
}

var nn1 = /* tuple */[
  n1,
  n0
];

function pr(param) {
  return Caml_curry.app2(Printf.printf(/* Format */{
                  0: /* Int64 */{
                    0: /* Int_d */0,
                    1: /* No_padding */0,
                    2: /* No_precision */0,
                    3: /* Int64 */{
                      0: /* Int_d */0,
                      1: /* Lit_padding */{
                        0: /* Zeros */2,
                        1: 18,
                        length: 2,
                        tag: 0
                      },
                      2: /* No_precision */0,
                      3: /* Char_literal */{
                        0: /* "\n" */10,
                        1: /* End_of_format */0,
                        length: 2,
                        tag: 12
                      },
                      length: 4,
                      tag: 7
                    },
                    length: 4,
                    tag: 7
                  },
                  1: "%Ld%018Ld\n",
                  length: 2,
                  tag: 0
                }), param[1], param[0]);
}

function map(f, l) {
  return {
          0: function () {
            var tag = l.tag | 0;
            var match = tag === 250 ? l[0] : (
                tag === 246 ? CamlinternalLazy.force_lazy_block(l) : l
              );
            return /* Cons */{
                    0: Caml_curry.app1(f, match[0]),
                    1: map(f, match[1]),
                    length: 2,
                    tag: 0
                  };
          },
          length: 1,
          tag: 246
        };
}

function merge(cmp, l1, l2) {
  return {
          0: function () {
            var tag = l1.tag | 0;
            var match = tag === 250 ? l1[0] : (
                tag === 246 ? CamlinternalLazy.force_lazy_block(l1) : l1
              );
            var tag$1 = l2.tag | 0;
            var match$1 = tag$1 === 250 ? l2[0] : (
                tag$1 === 246 ? CamlinternalLazy.force_lazy_block(l2) : l2
              );
            var ll2 = match$1[1];
            var x2 = match$1[0];
            var ll1 = match[1];
            var x1 = match[0];
            var c = Caml_curry.app2(cmp, x1, x2);
            if (c) {
              if (c < 0) {
                return /* Cons */{
                        0: x1,
                        1: merge(cmp, ll1, l2),
                        length: 2,
                        tag: 0
                      };
              }
              else {
                return /* Cons */{
                        0: x2,
                        1: merge(cmp, l1, ll2),
                        length: 2,
                        tag: 0
                      };
              }
            }
            else {
              return /* Cons */{
                      0: x1,
                      1: merge(cmp, ll1, ll2),
                      length: 2,
                      tag: 0
                    };
            }
          },
          length: 1,
          tag: 246
        };
}

function iter_interval(f, _l, _param) {
  while(true) {
    var param = _param;
    var l = _l;
    var stop = param[1];
    var start = param[0];
    if (stop) {
      var tag = l.tag | 0;
      var match = tag === 250 ? l[0] : (
          tag === 246 ? CamlinternalLazy.force_lazy_block(l) : l
        );
      if (start <= 0) {
        Caml_curry.app1(f, match[0]);
      }
      _param = /* tuple */[
        start - 1,
        stop - 1
      ];
      _l = match[1];
      continue ;
      
    }
    else {
      return /* () */0;
    }
  };
}

var hamming = {
  
};

var ham2 = {
  
};

var ham3 = {
  
};

var ham5 = {
  
};

Caml_obj.caml_update_dummy(hamming, {
      0: function () {
        return /* Cons */{
                0: nn1,
                1: merge(cmp, ham2, merge(cmp, ham3, ham5)),
                length: 2,
                tag: 0
              };
      },
      length: 1,
      tag: 246
    });

Caml_obj.caml_update_dummy(ham2, {
      0: function () {
        var lzarg = map(x2, hamming);
        var tag = lzarg.tag | 0;
        if (tag === 250) {
          return lzarg[0];
        }
        else if (tag === 246) {
          return CamlinternalLazy.force_lazy_block(lzarg);
        }
        else {
          return lzarg;
        }
      },
      length: 1,
      tag: 246
    });

Caml_obj.caml_update_dummy(ham3, {
      0: function () {
        var lzarg = map(x3, hamming);
        var tag = lzarg.tag | 0;
        if (tag === 250) {
          return lzarg[0];
        }
        else if (tag === 246) {
          return CamlinternalLazy.force_lazy_block(lzarg);
        }
        else {
          return lzarg;
        }
      },
      length: 1,
      tag: 246
    });

Caml_obj.caml_update_dummy(ham5, {
      0: function () {
        var lzarg = map(x5, hamming);
        var tag = lzarg.tag | 0;
        if (tag === 250) {
          return lzarg[0];
        }
        else if (tag === 246) {
          return CamlinternalLazy.force_lazy_block(lzarg);
        }
        else {
          return lzarg;
        }
      },
      length: 1,
      tag: 246
    });

iter_interval(pr, hamming, /* tuple */[
      88000,
      88100
    ]);

exports.n0            = n0;
exports.n1            = n1;
exports.n2            = n2;
exports.n3            = n3;
exports.n5            = n5;
exports.$percent      = $percent;
exports.$star         = $star;
exports.$slash        = $slash;
exports.$plus         = $plus;
exports.digit         = digit;
exports.mul           = mul;
exports.cmp           = cmp;
exports.x2            = x2;
exports.x3            = x3;
exports.x5            = x5;
exports.nn1           = nn1;
exports.pr            = pr;
exports.map           = map;
exports.merge         = merge;
exports.iter_interval = iter_interval;
exports.hamming       = hamming;
exports.ham2          = ham2;
exports.ham3          = ham3;
exports.ham5          = ham5;
/*  Not a pure module */
