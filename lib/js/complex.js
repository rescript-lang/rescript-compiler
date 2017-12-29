'use strict';


var one = /* float array */[
  1.0,
  0.0
];

function add(x, y) {
  return /* float array */[
          x[/* re */0] + y[/* re */0],
          x[/* im */1] + y[/* im */1]
        ];
}

function sub(x, y) {
  return /* float array */[
          x[/* re */0] - y[/* re */0],
          x[/* im */1] - y[/* im */1]
        ];
}

function neg(x) {
  return /* float array */[
          -x[/* re */0],
          -x[/* im */1]
        ];
}

function conj(x) {
  return /* float array */[
          x[/* re */0],
          -x[/* im */1]
        ];
}

function mul(x, y) {
  return /* float array */[
          x[/* re */0] * y[/* re */0] - x[/* im */1] * y[/* im */1],
          x[/* re */0] * y[/* im */1] + x[/* im */1] * y[/* re */0]
        ];
}

function div(x, y) {
  if (Math.abs(y[/* re */0]) >= Math.abs(y[/* im */1])) {
    var r = y[/* im */1] / y[/* re */0];
    var d = y[/* re */0] + r * y[/* im */1];
    return /* float array */[
            (x[/* re */0] + r * x[/* im */1]) / d,
            (x[/* im */1] - r * x[/* re */0]) / d
          ];
  } else {
    var r$1 = y[/* re */0] / y[/* im */1];
    var d$1 = y[/* im */1] + r$1 * y[/* re */0];
    return /* float array */[
            (r$1 * x[/* re */0] + x[/* im */1]) / d$1,
            (r$1 * x[/* im */1] - x[/* re */0]) / d$1
          ];
  }
}

function inv(x) {
  return div(one, x);
}

function norm2(x) {
  return x[/* re */0] * x[/* re */0] + x[/* im */1] * x[/* im */1];
}

function norm(x) {
  var r = Math.abs(x[/* re */0]);
  var i = Math.abs(x[/* im */1]);
  if (r === 0.0) {
    return i;
  } else if (i === 0.0) {
    return r;
  } else if (r >= i) {
    var q = i / r;
    return r * Math.sqrt(1.0 + q * q);
  } else {
    var q$1 = r / i;
    return i * Math.sqrt(1.0 + q$1 * q$1);
  }
}

function arg(x) {
  return Math.atan2(x[/* im */1], x[/* re */0]);
}

function polar(n, a) {
  return /* float array */[
          Math.cos(a) * n,
          Math.sin(a) * n
        ];
}

function sqrt(x) {
  if (x[/* re */0] === 0.0 && x[/* im */1] === 0.0) {
    return /* float array */[
            0.0,
            0.0
          ];
  } else {
    var r = Math.abs(x[/* re */0]);
    var i = Math.abs(x[/* im */1]);
    var w;
    if (r >= i) {
      var q = i / r;
      w = Math.sqrt(r) * Math.sqrt(0.5 * (1.0 + Math.sqrt(1.0 + q * q)));
    } else {
      var q$1 = r / i;
      w = Math.sqrt(i) * Math.sqrt(0.5 * (q$1 + Math.sqrt(1.0 + q$1 * q$1)));
    }
    if (x[/* re */0] >= 0.0) {
      return /* float array */[
              w,
              0.5 * x[/* im */1] / w
            ];
    } else {
      return /* float array */[
              0.5 * i / w,
              x[/* im */1] >= 0.0 ? w : -w
            ];
    }
  }
}

function exp(x) {
  var e = Math.exp(x[/* re */0]);
  return /* float array */[
          e * Math.cos(x[/* im */1]),
          e * Math.sin(x[/* im */1])
        ];
}

function log(x) {
  return /* float array */[
          Math.log(norm(x)),
          Math.atan2(x[/* im */1], x[/* re */0])
        ];
}

function pow(x, y) {
  return exp(mul(y, log(x)));
}

var zero = /* float array */[
  0.0,
  0.0
];

var i = /* float array */[
  0.0,
  1.0
];

exports.zero = zero;
exports.one = one;
exports.i = i;
exports.neg = neg;
exports.conj = conj;
exports.add = add;
exports.sub = sub;
exports.mul = mul;
exports.inv = inv;
exports.div = div;
exports.sqrt = sqrt;
exports.norm2 = norm2;
exports.norm = norm;
exports.arg = arg;
exports.polar = polar;
exports.exp = exp;
exports.log = log;
exports.pow = pow;
/* No side effect */
