'use strict';


var caml_int32_float_of_bits = (function(x){
    return new Float32Array(new Int32Array([x]).buffer)[0] 
    });

var caml_int32_bits_of_float = (function(x){
  return new Int32Array(new Float32Array([x]).buffer)[0] 
});

function caml_modf_float(x) {
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
            -f,
            -i
          ];
  } else {
    return [
            f,
            i
          ];
  }
}

function caml_ldexp_float(x, exp) {
  var x$prime = x;
  var exp$prime = exp;
  if (exp$prime > 1023) {
    exp$prime = exp$prime - 1023;
    x$prime = x$prime * Math.pow(2, 1023);
    if (exp$prime > 1023) {
      exp$prime = exp$prime - 1023;
      x$prime = x$prime * Math.pow(2, 1023);
    }
    
  } else if (exp$prime < -1023) {
    exp$prime = exp$prime + 1023;
    x$prime = x$prime * Math.pow(2, -1023);
  }
  return x$prime * Math.pow(2, exp$prime);
}

function caml_frexp_float(x) {
  if (x === 0 || !isFinite(x)) {
    return [
            x,
            0
          ];
  }
  var neg = x < 0;
  var x$prime = Math.abs(x);
  var exp = Math.floor(Math.LOG2E * Math.log(x$prime)) + 1;
  x$prime = x$prime * Math.pow(2, -exp);
  if (x$prime < 0.5) {
    x$prime = x$prime * 2;
    exp = exp - 1;
  }
  if (neg) {
    x$prime = -x$prime;
  }
  return [
          x$prime,
          exp | 0
        ];
}

function caml_copysign_float(x, y) {
  var x$1 = Math.abs(x);
  var y$1 = y === 0 ? 1 / y : y;
  if (y$1 < 0) {
    return -x$1;
  } else {
    return x$1;
  }
}

function caml_expm1_float(x) {
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

function caml_hypot_float(x, y) {
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

function caml_log10_float(x) {
  return Math.LOG10E * Math.log(x);
}

exports.caml_int32_float_of_bits = caml_int32_float_of_bits;
exports.caml_int32_bits_of_float = caml_int32_bits_of_float;
exports.caml_modf_float = caml_modf_float;
exports.caml_ldexp_float = caml_ldexp_float;
exports.caml_frexp_float = caml_frexp_float;
exports.caml_copysign_float = caml_copysign_float;
exports.caml_expm1_float = caml_expm1_float;
exports.caml_hypot_float = caml_hypot_float;
exports.caml_log10_float = caml_log10_float;
/* No side effect */
