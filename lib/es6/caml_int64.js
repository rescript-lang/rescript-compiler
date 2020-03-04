

import * as Caml_int32 from "./caml_int32.js";
import * as Caml_utils from "./caml_utils.js";
import * as Caml_primitive from "./caml_primitive.js";
import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

var min_int = /* Int64 */[
  /* hi */-2147483648,
  /* lo */0
];

var max_int = /* Int64 */[
  /* hi */2147483647,
  /* lo */1
];

var one = /* Int64 */[
  /* hi */0,
  /* lo */1
];

var zero = /* Int64 */[
  /* hi */0,
  /* lo */0
];

var neg_one = /* Int64 */[
  /* hi */-1,
  /* lo */4294967295
];

function neg_signed(x) {
  return (x & 2147483648) !== 0;
}

function add(param, param$1) {
  var y_lo = param$1[/* lo */1];
  var x_lo = param[/* lo */1];
  var lo = x_lo + y_lo & 4294967295;
  var overflow = neg_signed(x_lo) && (neg_signed(y_lo) || !neg_signed(lo)) || neg_signed(y_lo) && !neg_signed(lo) ? 1 : 0;
  var hi = param[/* hi */0] + param$1[/* hi */0] + overflow & 4294967295;
  return /* Int64 */[
          /* hi */hi,
          /* lo */(lo >>> 0)
        ];
}

function not(param) {
  var hi = param[/* hi */0] ^ -1;
  var lo = param[/* lo */1] ^ -1;
  return /* Int64 */[
          /* hi */hi,
          /* lo */(lo >>> 0)
        ];
}

function eq(param, param$1) {
  if (param[/* hi */0] === param$1[/* hi */0]) {
    return param[/* lo */1] === param$1[/* lo */1];
  } else {
    return false;
  }
}

function equal_null(x, y) {
  if (y !== null) {
    return eq(x, y);
  } else {
    return false;
  }
}

function equal_undefined(x, y) {
  if (y !== undefined) {
    return eq(x, y);
  } else {
    return false;
  }
}

function equal_nullable(x, y) {
  if (y == null) {
    return false;
  } else {
    return eq(x, y);
  }
}

function neg(x) {
  if (eq(x, min_int)) {
    return min_int;
  } else {
    var param = not(x);
    var x_lo = param[/* lo */1];
    var lo = x_lo + 1 & 4294967295;
    var overflow = neg_signed(x_lo) && (neg_signed(1) || !neg_signed(lo)) || neg_signed(1) && !neg_signed(lo) ? 1 : 0;
    var hi = param[/* hi */0] + overflow & 4294967295;
    return /* Int64 */[
            /* hi */hi,
            /* lo */(lo >>> 0)
          ];
  }
}

function sub(x, y) {
  return add(x, neg(y));
}

function lsl_(x, numBits) {
  if (numBits === 0) {
    return x;
  } else {
    var lo = x[/* lo */1];
    if (numBits >= 32) {
      return /* Int64 */[
              /* hi */(lo << (numBits - 32 | 0)),
              /* lo */0
            ];
    } else {
      var hi = (lo >>> (32 - numBits | 0)) | (x[/* hi */0] << numBits);
      return /* Int64 */[
              /* hi */hi,
              /* lo */((lo << numBits) >>> 0)
            ];
    }
  }
}

function lsr_(x, numBits) {
  if (numBits === 0) {
    return x;
  } else {
    var hi = x[/* hi */0];
    var offset = numBits - 32 | 0;
    if (offset === 0) {
      return /* Int64 */[
              /* hi */0,
              /* lo */(hi >>> 0)
            ];
    } else if (offset > 0) {
      var lo = (hi >>> offset);
      return /* Int64 */[
              /* hi */0,
              /* lo */(lo >>> 0)
            ];
    } else {
      var hi$1 = (hi >>> numBits);
      var lo$1 = (hi << (-offset | 0)) | (x[/* lo */1] >>> numBits);
      return /* Int64 */[
              /* hi */hi$1,
              /* lo */(lo$1 >>> 0)
            ];
    }
  }
}

function asr_(x, numBits) {
  if (numBits === 0) {
    return x;
  } else {
    var hi = x[/* hi */0];
    if (numBits < 32) {
      var hi$1 = (hi >> numBits);
      var lo = (hi << (32 - numBits | 0)) | (x[/* lo */1] >>> numBits);
      return /* Int64 */[
              /* hi */hi$1,
              /* lo */(lo >>> 0)
            ];
    } else {
      var lo$1 = (hi >> (numBits - 32 | 0));
      return /* Int64 */[
              /* hi */hi >= 0 ? 0 : -1,
              /* lo */(lo$1 >>> 0)
            ];
    }
  }
}

function is_zero(param) {
  if (param[/* hi */0] !== 0 || param[/* lo */1] !== 0) {
    return false;
  } else {
    return true;
  }
}

function mul(_this, _other) {
  while(true) {
    var other = _other;
    var $$this = _this;
    var lo;
    var exit = 0;
    var exit$1 = 0;
    if ($$this[/* hi */0] !== 0 || $$this[/* lo */1] !== 0) {
      exit$1 = 3;
    } else {
      return zero;
    }
    if (exit$1 === 3) {
      if (other[/* hi */0] !== 0 || other[/* lo */1] !== 0) {
        exit = 2;
      } else {
        return zero;
      }
    }
    if (exit === 2) {
      var this_hi = $$this[/* hi */0];
      var exit$2 = 0;
      if (this_hi !== -2147483648 || $$this[/* lo */1] !== 0) {
        exit$2 = 3;
      } else {
        lo = other[/* lo */1];
      }
      if (exit$2 === 3) {
        var other_hi = other[/* hi */0];
        var lo$1 = $$this[/* lo */1];
        var exit$3 = 0;
        if (other_hi !== -2147483648 || other[/* lo */1] !== 0) {
          exit$3 = 4;
        } else {
          lo = lo$1;
        }
        if (exit$3 === 4) {
          var other_lo = other[/* lo */1];
          if (this_hi < 0) {
            if (other_hi < 0) {
              _other = neg(other);
              _this = neg($$this);
              continue ;
            } else {
              return neg(mul(neg($$this), other));
            }
          } else if (other_hi < 0) {
            return neg(mul($$this, neg(other)));
          } else {
            var a48 = (this_hi >>> 16);
            var a32 = this_hi & 65535;
            var a16 = (lo$1 >>> 16);
            var a00 = lo$1 & 65535;
            var b48 = (other_hi >>> 16);
            var b32 = other_hi & 65535;
            var b16 = (other_lo >>> 16);
            var b00 = other_lo & 65535;
            var c48 = 0;
            var c32 = 0;
            var c16 = 0;
            var c00 = a00 * b00;
            c16 = (c00 >>> 16) + a16 * b00;
            c32 = (c16 >>> 16);
            c16 = (c16 & 65535) + a00 * b16;
            c32 = c32 + (c16 >>> 16) + a32 * b00;
            c48 = (c32 >>> 16);
            c32 = (c32 & 65535) + a16 * b16;
            c48 = c48 + (c32 >>> 16);
            c32 = (c32 & 65535) + a00 * b32;
            c48 = c48 + (c32 >>> 16);
            c32 = c32 & 65535;
            c48 = c48 + (a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48) & 65535;
            var hi = c32 | (c48 << 16);
            var lo$2 = c00 & 65535 | ((c16 & 65535) << 16);
            return /* Int64 */[
                    /* hi */hi,
                    /* lo */(lo$2 >>> 0)
                  ];
          }
        }
        
      }
      
    }
    if ((lo & 1) === 0) {
      return zero;
    } else {
      return min_int;
    }
  };
}

function swap(param) {
  var hi = Caml_int32.caml_int32_bswap(param[/* lo */1]);
  var lo = Caml_int32.caml_int32_bswap(param[/* hi */0]);
  return /* Int64 */[
          /* hi */hi,
          /* lo */(lo >>> 0)
        ];
}

function xor(param, param$1) {
  return /* Int64 */[
          /* hi */param[/* hi */0] ^ param$1[/* hi */0],
          /* lo */((param[/* lo */1] ^ param$1[/* lo */1]) >>> 0)
        ];
}

function or_(param, param$1) {
  return /* Int64 */[
          /* hi */param[/* hi */0] | param$1[/* hi */0],
          /* lo */((param[/* lo */1] | param$1[/* lo */1]) >>> 0)
        ];
}

function and_(param, param$1) {
  return /* Int64 */[
          /* hi */param[/* hi */0] & param$1[/* hi */0],
          /* lo */((param[/* lo */1] & param$1[/* lo */1]) >>> 0)
        ];
}

function ge(param, param$1) {
  var other_hi = param$1[/* hi */0];
  var hi = param[/* hi */0];
  if (hi > other_hi) {
    return true;
  } else if (hi < other_hi) {
    return false;
  } else {
    return param[/* lo */1] >= param$1[/* lo */1];
  }
}

function neq(x, y) {
  return !eq(x, y);
}

function lt(x, y) {
  return !ge(x, y);
}

function gt(param, param$1) {
  if (param[/* hi */0] > param$1[/* hi */0]) {
    return true;
  } else if (param[/* hi */0] < param$1[/* hi */0]) {
    return false;
  } else {
    return param[/* lo */1] > param$1[/* lo */1];
  }
}

function le(x, y) {
  return !gt(x, y);
}

function min(x, y) {
  if (ge(x, y)) {
    return y;
  } else {
    return x;
  }
}

function max(x, y) {
  if (gt(x, y)) {
    return x;
  } else {
    return y;
  }
}

function to_float(param) {
  return param[/* hi */0] * 0x100000000 + param[/* lo */1];
}

function of_float(x) {
  if (isNaN(x) || !isFinite(x)) {
    return zero;
  } else if (x <= -9.22337203685477581e+18) {
    return min_int;
  } else if (x + 1 >= 9.22337203685477581e+18) {
    return max_int;
  } else if (x < 0) {
    return neg(of_float(-x));
  } else {
    var hi = x / 4294967296 | 0;
    var lo = x % 4294967296 | 0;
    return /* Int64 */[
            /* hi */hi,
            /* lo */(lo >>> 0)
          ];
  }
}

function to_string(self) {
  var self_hi = self[/* hi */0];
  if (self_hi === 0) {
    return String(self[/* lo */1]);
  } else if (self_hi < 0) {
    if (eq(self, min_int)) {
      return "-9223372036854775808";
    } else {
      return "-" + to_string(neg(self));
    }
  } else {
    var approx_div1 = of_float(Math.floor(to_float(self) / 10));
    var y = lsl_(approx_div1, 1);
    var y$1 = lsl_(approx_div1, 3);
    var x = add(self, neg(y$1));
    var rem = add(x, neg(y));
    var rem_lo = rem[/* lo */1];
    var rem_hi = rem[/* hi */0];
    if (rem_lo === 0 && rem_hi === 0) {
      return to_string(approx_div1) + "0";
    } else if (rem_hi < 0) {
      var match = neg(rem);
      var rem_lo$1 = match[/* lo */1];
      var delta = Math.ceil(rem_lo$1 / 10);
      var remainder = 10 * delta - rem_lo$1;
      var lo = delta | 0;
      return to_string(add(approx_div1, neg(/* Int64 */[
                          /* hi */0,
                          /* lo */(lo >>> 0)
                        ]))) + String(remainder | 0);
    } else {
      var rem_lo$2 = rem_lo;
      var delta$1 = Math.floor(rem_lo$2 / 10);
      var remainder$1 = rem_lo$2 - 10 * delta$1;
      var lo$1 = delta$1 | 0;
      return to_string(add(approx_div1, /* Int64 */[
                      /* hi */0,
                      /* lo */(lo$1 >>> 0)
                    ])) + String(remainder$1 | 0);
    }
  }
}

function div(_self, _other) {
  while(true) {
    var other = _other;
    var self = _self;
    var exit = 0;
    var exit$1 = 0;
    if (other[/* hi */0] !== 0 || other[/* lo */1] !== 0) {
      exit$1 = 3;
    } else {
      throw Caml_builtin_exceptions.division_by_zero;
    }
    if (exit$1 === 3) {
      var match = self[/* hi */0];
      if (match !== -2147483648) {
        if (match !== 0 || self[/* lo */1] !== 0) {
          exit = 2;
        } else {
          return zero;
        }
      } else if (self[/* lo */1] !== 0) {
        exit = 2;
      } else if (eq(other, one) || eq(other, neg_one)) {
        return self;
      } else if (eq(other, min_int)) {
        return one;
      } else {
        var half_this = asr_(self, 1);
        var approx = lsl_(div(half_this, other), 1);
        var exit$2 = 0;
        if (approx[/* hi */0] !== 0 || approx[/* lo */1] !== 0) {
          exit$2 = 4;
        } else if (other[/* hi */0] < 0) {
          return one;
        } else {
          return neg(one);
        }
        if (exit$2 === 4) {
          var y = mul(other, approx);
          var rem = add(self, neg(y));
          return add(approx, div(rem, other));
        }
        
      }
    }
    if (exit === 2 && other[/* hi */0] === -2147483648 && other[/* lo */1] === 0) {
      return zero;
    }
    var other_hi = other[/* hi */0];
    if (self[/* hi */0] < 0) {
      if (other_hi < 0) {
        _other = neg(other);
        _self = neg(self);
        continue ;
      } else {
        return neg(div(neg(self), other));
      }
    } else if (other_hi < 0) {
      return neg(div(self, neg(other)));
    } else {
      var res = zero;
      var rem$1 = self;
      while(ge(rem$1, other)) {
        var approx$1 = Caml_primitive.caml_float_max(1, Math.floor(to_float(rem$1) / to_float(other)));
        var log2 = Math.ceil(Math.log(approx$1) / Math.LN2);
        var delta = log2 <= 48 ? 1 : Math.pow(2, log2 - 48);
        var approxRes = of_float(approx$1);
        var approxRem = mul(approxRes, other);
        while(approxRem[/* hi */0] < 0 || gt(approxRem, rem$1)) {
          approx$1 = approx$1 - delta;
          approxRes = of_float(approx$1);
          approxRem = mul(approxRes, other);
        };
        if (is_zero(approxRes)) {
          approxRes = one;
        }
        res = add(res, approxRes);
        rem$1 = add(rem$1, neg(approxRem));
      };
      return res;
    }
  };
}

function mod_(self, other) {
  var y = mul(div(self, other), other);
  return add(self, neg(y));
}

function div_mod(self, other) {
  var quotient = div(self, other);
  var y = mul(quotient, other);
  return /* tuple */[
          quotient,
          add(self, neg(y))
        ];
}

function compare(param, param$1) {
  var v = Caml_primitive.caml_nativeint_compare(param[/* hi */0], param$1[/* hi */0]);
  if (v === 0) {
    return Caml_primitive.caml_nativeint_compare(param[/* lo */1], param$1[/* lo */1]);
  } else {
    return v;
  }
}

function of_int32(lo) {
  return /* Int64 */[
          /* hi */lo < 0 ? -1 : 0,
          /* lo */(lo >>> 0)
        ];
}

function to_int32(param) {
  return param[/* lo */1] | 0;
}

function to_hex(x) {
  var x_lo = x[/* lo */1];
  var x_hi = x[/* hi */0];
  var aux = function (v) {
    return (v >>> 0).toString(16);
  };
  if (x_hi === 0 && x_lo === 0) {
    return "0";
  }
  if (x_lo !== 0) {
    if (x_hi !== 0) {
      var lo = aux(x_lo);
      var pad = 8 - lo.length | 0;
      if (pad <= 0) {
        return aux(x_hi) + lo;
      } else {
        return aux(x_hi) + (Caml_utils.repeat(pad, "0") + lo);
      }
    } else {
      return aux(x_lo);
    }
  } else {
    return aux(x_hi) + "00000000";
  }
}

function discard_sign(x) {
  return /* Int64 */[
          /* hi */2147483647 & x[/* hi */0],
          /* lo */x[/* lo */1]
        ];
}

function float_of_bits(param) {
  return (function(lo,hi){ return (new Float64Array(new Int32Array([lo,hi]).buffer))[0]})(param[/* lo */1], param[/* hi */0]);
}

function bits_of_float(x) {
  var buf = (function(x){return new Int32Array(new Float64Array([x]).buffer)})(x);
  return /* Int64 */[
          /* hi */buf[1],
          /* lo */(buf[0] >>> 0)
        ];
}

export {
  min_int ,
  max_int ,
  one ,
  zero ,
  not ,
  of_int32 ,
  to_int32 ,
  add ,
  neg ,
  sub ,
  lsl_ ,
  lsr_ ,
  asr_ ,
  is_zero ,
  mul ,
  xor ,
  or_ ,
  and_ ,
  swap ,
  ge ,
  eq ,
  neq ,
  lt ,
  gt ,
  le ,
  equal_null ,
  equal_undefined ,
  equal_nullable ,
  min ,
  max ,
  to_float ,
  of_float ,
  div ,
  mod_ ,
  compare ,
  float_of_bits ,
  bits_of_float ,
  div_mod ,
  to_hex ,
  discard_sign ,
  to_string ,
  
}
/* Caml_int32 Not a pure module */
