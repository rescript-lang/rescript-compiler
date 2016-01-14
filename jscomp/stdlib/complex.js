// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var one = /* float array */[
  1.0,
  0.0
];

function add(x, y) {
  return /* array */[
          x[1] + y[1],
          x[2] + y[2]
        ];
}

function sub(x, y) {
  return /* array */[
          x[1] - y[1],
          x[2] - y[2]
        ];
}

function neg(x) {
  return /* array */[
          -x[1],
          -x[2]
        ];
}

function conj(x) {
  return /* array */[
          x[1],
          -x[2]
        ];
}

function mul(x, y) {
  return /* array */[
          x[1] * y[1] - x[2] * y[2],
          x[1] * y[2] + x[2] * y[1]
        ];
}

function div(x, y) {
  if (Math.abs(y[1]) >= Math.abs(y[2])) {
    var r = y[2] / y[1];
    var d = y[1] + r * y[2];
    return /* array */[
            (x[1] + r * x[2]) / d,
            (x[2] - r * x[1]) / d
          ];
  }
  else {
    var r$1 = y[1] / y[2];
    var d$1 = y[2] + r$1 * y[1];
    return /* array */[
            (r$1 * x[1] + x[2]) / d$1,
            (r$1 * x[2] - x[1]) / d$1
          ];
  }
}

function inv(x) {
  return div(one, x);
}

function norm2(x) {
  return x[1] * x[1] + x[2] * x[2];
}

function norm(x) {
  var r = Math.abs(x[1]);
  var i = Math.abs(x[2]);
  if (r === 0.0) {
    return i;
  }
  else {
    if (i === 0.0) {
      return r;
    }
    else {
      if (r >= i) {
        var q = i / r;
        return r * Math.sqrt(1.0 + q * q);
      }
      else {
        var q$1 = r / i;
        return i * Math.sqrt(1.0 + q$1 * q$1);
      }
    }
  }
}

function arg(x) {
  return Math.atan2(x[2], x[1]);
}

function polar(n, a) {
  return /* array */[
          Math.cos(a) * n,
          Math.sin(a) * n
        ];
}

function sqrt(x) {
  if (x[1] === 0.0 && x[2] === 0.0) {
    return /* float array */[
            0.0,
            0.0
          ];
  }
  else {
    var r = Math.abs(x[1]);
    var i = Math.abs(x[2]);
    var w;
    if (r >= i) {
      var q = i / r;
      w = Math.sqrt(r) * Math.sqrt(0.5 * (1.0 + Math.sqrt(1.0 + q * q)));
    }
    else {
      var q$1 = r / i;
      w = Math.sqrt(i) * Math.sqrt(0.5 * (q$1 + Math.sqrt(1.0 + q$1 * q$1)));
    }
    return x[1] >= 0.0 ? /* array */[
              w,
              0.5 * x[2] / w
            ] : /* array */[
              0.5 * i / w,
              x[2] >= 0.0 ? w : -w
            ];
  }
}

function exp(x) {
  var e = Math.exp(x[1]);
  return /* array */[
          e * Math.cos(x[2]),
          e * Math.sin(x[2])
        ];
}

function log(x) {
  return /* array */[
          Math.log(norm(x)),
          Math.atan2(x[2], x[1])
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
