'use strict';


var int_float_of_bits = (function(x){
    return new Float32Array(new Int32Array([x]).buffer)[0] 
    });

var int_bits_of_float = (function(x){
  return new Int32Array(new Float32Array([x]).buffer)[0] 
});

function modf_float(x) {
  if (!isFinite(x)) {
    if (isNaN(x)) {
      return [
              NaN,
              NaN
            ];
    } else {
      return [
              1 / x,
              x
            ];
    }
  }
  var neg = 1 / x < 0;
  var x$1 = Math.abs(x);
  var i = Math.floor(x$1);
  var f = x$1 - i;
  if (neg) {
    return [
            - f,
            - i
          ];
  } else {
    return [
            f,
            i
          ];
  }
}

function ldexp_float(x, exp) {
  var x$p = x;
  var exp$p = exp;
  if (exp$p > 1023) {
    exp$p = exp$p - 1023;
    x$p = x$p * Math.pow(2, 1023);
    if (exp$p > 1023) {
      exp$p = exp$p - 1023;
      x$p = x$p * Math.pow(2, 1023);
    }
    
  } else if (exp$p < -1023) {
    exp$p = exp$p + 1023;
    x$p = x$p * Math.pow(2, -1023);
  }
  return x$p * Math.pow(2, exp$p);
}

function frexp_float(x) {
  if (x === 0 || !isFinite(x)) {
    return [
            x,
            0
          ];
  }
  var neg = x < 0;
  var x$p = Math.abs(x);
  var exp = Math.floor(Math.LOG2E * Math.log(x$p)) + 1;
  x$p = x$p * Math.pow(2, - exp);
  if (x$p < 0.5) {
    x$p = x$p * 2;
    exp = exp - 1;
  }
  if (neg) {
    x$p = - x$p;
  }
  return [
          x$p,
          exp | 0
        ];
}

function copysign_float(x, y) {
  var x$1 = Math.abs(x);
  var y$1 = y === 0 ? 1 / y : y;
  if (y$1 < 0) {
    return - x$1;
  } else {
    return x$1;
  }
}

function expm1_float(x) {
  var y = Math.exp(x);
  var z = y - 1;
  if (Math.abs(x) > 1) {
    return z;
  } else if (z === 0) {
    return x;
  } else {
    return x * z / Math.log(y);
  }
}

function hypot_float(x, y) {
  var x0 = Math.abs(x);
  var y0 = Math.abs(y);
  var a = x0 > y0 ? x0 : y0;
  var b = (
    x0 < y0 ? x0 : y0
  ) / (
    a !== 0 ? a : 1
  );
  return a * Math.sqrt(1 + b * b);
}

exports.int_float_of_bits = int_float_of_bits;
exports.int_bits_of_float = int_bits_of_float;
exports.modf_float = modf_float;
exports.ldexp_float = ldexp_float;
exports.frexp_float = frexp_float;
exports.copysign_float = copysign_float;
exports.expm1_float = expm1_float;
exports.hypot_float = hypot_float;
/* No side effect */
