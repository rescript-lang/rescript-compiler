// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");
var Caml_primitive          = require("./caml_primitive");

var min_int = /* record */[
  0,
  -2147483648
];

var max_int = /* record */[
  -4294967295,
  134217727
];

var one = /* record */[
  1,
  0
];

var zero = /* record */[
  0,
  0
];

function not(param) {
  return /* record */[
          param[0] ^ -1,
          param[1] ^ -1
        ];
}

function of_int32(lo) {
  if (lo < 0) {
    return /* record */[
            lo,
            -1
          ];
  }
  else {
    return /* record */[
            lo,
            0
          ];
  }
}

function add(param, param$1) {
  var other_high_ = param$1[1];
  var other_low_ = param$1[0];
  var this_high_ = param[1];
  var this_low_ = param[0];
  var a48 = (this_high_ >>> 16);
  var a32 = this_high_ & 65535;
  var a16 = (this_low_ >>> 16);
  var a00 = this_low_ & 65535;
  var b48 = (other_high_ >>> 16);
  var b32 = other_high_ & 65535;
  var b16 = (other_low_ >>> 16);
  var b00 = other_low_ & 65535;
  var c00 = a00 + b00;
  var c16 = (c00 >>> 16) + a16 + b16;
  var c32 = (c16 >>> 16) + a32 + b32;
  var c48 = (c32 >>> 16) + a48 + b48;
  return /* record */[
          ((c16 & 65535) << 16) | c00 & 65535,
          ((c48 & 65535) << 16) | c32 & 65535
        ];
}

function neg(x) {
  if (x === min_int) {
    return min_int;
  }
  else {
    return add(not(x), one);
  }
}

function sub(x, y) {
  return add(x, neg(y));
}

function lsl_(x, numBits) {
  var lo = x[0];
  if (numBits) {
    if (numBits >= 32) {
      return /* record */[
              0,
              (lo << numBits - 32)
            ];
    }
    else {
      return /* record */[
              (lo << numBits),
              (lo >>> 32 - numBits) | 0 | (x[1] << numBits)
            ];
    }
  }
  else {
    return x;
  }
}

function lsr_(x, numBits) {
  var hi = x[1];
  if (numBits) {
    var offset = numBits - 32;
    if (offset) {
      if (offset > 0) {
        return /* record */[
                (hi >>> offset) | 0,
                0
              ];
      }
      else {
        return /* record */[
                (hi << -offset) | (x[0] >>> numBits) | 0,
                (hi >>> numBits) | 0
              ];
      }
    }
    else {
      return /* record */[
              hi,
              0
            ];
    }
  }
  else {
    return x;
  }
}

function asr_(x, numBits) {
  var hi = x[1];
  if (numBits) {
    if (numBits < 32) {
      return /* record */[
              (hi << 32 - numBits) | (x[0] >>> numBits) | 0,
              (hi >> numBits)
            ];
    }
    else {
      return /* record */[
              (hi >> numBits - 32),
              hi >= 0 ? 0 : -1
            ];
    }
  }
  else {
    return x;
  }
}

function is_zero(param) {
  if (param[0] !== 0 || param[1] !== 0) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function mul(_this, _other) {
  while(true) {
    var other = _other;
    var $$this = _this;
    var exit = 0;
    var lo;
    var lo$1 = $$this[0];
    var exit$1 = 0;
    var exit$2 = 0;
    var exit$3 = 0;
    if (lo$1 !== 0) {
      exit$3 = 4;
    }
    else if ($$this[1] !== 0) {
      exit$3 = 4;
    }
    else {
      return zero;
    }
    if (exit$3 === 4) {
      if (other[0] !== 0) {
        exit$2 = 3;
      }
      else if (other[1] !== 0) {
        exit$2 = 3;
      }
      else {
        return zero;
      }
    }
    if (exit$2 === 3) {
      if (lo$1 !== 0) {
        exit$1 = 2;
      }
      else if ($$this[1] !== -2147483648) {
        exit$1 = 2;
      }
      else {
        lo = other[0];
        exit = 1;
      }
    }
    if (exit$1 === 2) {
      var other_lo = other[0];
      var this_hi = $$this[1];
      var exit$4 = 0;
      if (other_lo !== 0) {
        exit$4 = 3;
      }
      else if (other[1] !== -2147483648) {
        exit$4 = 3;
      }
      else {
        lo = lo$1;
        exit = 1;
      }
      if (exit$4 === 3) {
        var other_hi = other[1];
        if (this_hi < 0) {
          if (other_hi < 0) {
            _other = neg(other);
            _this = neg($$this);
            continue ;
            
          }
          else {
            return neg(mul(neg($$this), other));
          }
        }
        else if (other_hi < 0) {
          return neg(mul($$this, neg(other)));
        }
        else {
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
          var c00 = 0;
          c00 = a00 * b00;
          c16 = (c00 >>> 16);
          c00 = c00 & 65535;
          c16 += (a16 * b00);
          c32 = (c16 >>> 16);
          c16 = c16 & 65535;
          c16 += (a00 * b16);
          c32 += (c16 >>> 16);
          c16 = c16 & 65535;
          c32 += (a32 * b00);
          c48 = (c32 >>> 16);
          c32 = c32 & 65535;
          c32 += (a16 * b16);
          c48 += (c32 >>> 16);
          c32 = c32 & 65535;
          c32 += (a00 * b32);
          c48 += (c32 >>> 16);
          c32 = c32 & 65535;
          c48 += (a48 * b00 + (a32 * b16 + (a16 * b32 + a00 * b48)));
          c48 = c48 & 65535;
          return /* record */[
                  c00 | (c16 << 16),
                  c32 | (c48 << 16)
                ];
        }
      }
      
    }
    if (exit === 1) {
      if ((lo & 1) === 0) {
        return zero;
      }
      else {
        return min_int;
      }
    }
    
  };
}

function swap(param) {
  return /* record */[
          Caml_primitive.caml_int32_bswap(param[1]),
          Caml_primitive.caml_int32_bswap(param[0])
        ];
}

function ge(param, param$1) {
  var other_hi = param$1[1];
  var hi = param[1];
  if (hi > other_hi) {
    return /* true */1;
  }
  else if (hi < other_hi) {
    return /* false */0;
  }
  else {
    return +((param[0] >>> 0) >= (param$1[0] >>> 0));
  }
}

function eq(x, y) {
  if (x[1] === y[1]) {
    return +(x[0] === y[0]);
  }
  else {
    return /* false */0;
  }
}

function neq(x, y) {
  return !eq(x, y);
}

function lt(x, y) {
  return !ge(x, y);
}

function gt(x, y) {
  if (x[1] > y[1]) {
    return /* true */1;
  }
  else if (x[1] < y[1]) {
    return /* false */0;
  }
  else {
    return +((x[0] >>> 0) > (y[0] >>> 0));
  }
}

function le(x, y) {
  return !gt(x, y);
}

function to_float(param) {
  var lo = param[0];
  var low_bits_unsigned = lo >= 0 ? lo : lo + 4294967296;
  return param[1] * 4294967296 + low_bits_unsigned;
}

var two_ptr_32_dbl = Math.pow(2, 32);

var two_ptr_63_dbl = Math.pow(2, 63);

var neg_two_ptr_63 = -Math.pow(2, 63);

function of_float(x) {
  if (isNaN(x) || !isFinite(x)) {
    return zero;
  }
  else if (x <= neg_two_ptr_63) {
    return min_int;
  }
  else if (x + 1 >= two_ptr_63_dbl) {
    return max_int;
  }
  else if (x < 0) {
    return neg(of_float(-x));
  }
  else {
    return /* record */[
            x % two_ptr_32_dbl | 0,
            x / two_ptr_32_dbl | 0
          ];
  }
}

function div(_self, _other) {
  while(true) {
    var other = _other;
    var self = _self;
    var exit = 0;
    var exit$1 = 0;
    if (other[0] !== 0) {
      exit$1 = 2;
    }
    else if (other[1] !== 0) {
      exit$1 = 2;
    }
    else {
      throw Caml_builtin_exceptions.division_by_zero;
    }
    if (exit$1 === 2) {
      if (self[0] !== 0) {
        exit = 1;
      }
      else {
        var match = self[1];
        if (match !== -2147483648) {
          if (match !== 0) {
            exit = 1;
          }
          else {
            return zero;
          }
        }
        else {
          var match$1 = other[0];
          var exit$2 = 0;
          if (match$1 !== -1) {
            if (match$1 !== 0) {
              if (match$1 !== 1) {
                exit$2 = 3;
              }
              else if (other[1] !== 0) {
                exit$2 = 3;
              }
              else {
                return self;
              }
            }
            else if (other[1] !== -2147483648) {
              exit$2 = 3;
            }
            else {
              return one;
            }
          }
          else if (other[1] !== -1) {
            exit$2 = 3;
          }
          else {
            return self;
          }
          if (exit$2 === 3) {
            var half_this = asr_(self, 1);
            var approx = lsl_(div(half_this, other), 1);
            var exit$3 = 0;
            if (approx[0] !== 0) {
              exit$3 = 4;
            }
            else if (approx[1] !== 0) {
              exit$3 = 4;
            }
            else if (other[1] < 0) {
              return one;
            }
            else {
              return neg(one);
            }
            if (exit$3 === 4) {
              var y = mul(other, approx);
              var rem = add(self, neg(y));
              return add(approx, div(rem, other));
            }
            
          }
          
        }
      }
    }
    if (exit === 1) {
      var exit$4 = 0;
      if (other[0] !== 0) {
        exit$4 = 2;
      }
      else if (other[1] !== -2147483648) {
        exit$4 = 2;
      }
      else {
        return zero;
      }
      if (exit$4 === 2) {
        var other_hi = other[1];
        if (self[1] < 0) {
          if (other_hi < 0) {
            _other = neg(other);
            _self = neg(self);
            continue ;
            
          }
          else {
            return neg(div(neg(self), other));
          }
        }
        else if (other_hi < 0) {
          return neg(div(self, neg(other)));
        }
        else {
          var res = zero;
          var rem$1 = self;
          while(ge(rem$1, other)) {
            var approx$1 = Math.max(1, Math.floor(to_float(rem$1) / to_float(other)));
            var log2 = Math.ceil(Math.log(approx$1) / Math.LN2);
            var delta = log2 <= 48 ? 1 : Math.pow(2, log2 - 48);
            var approxRes = of_float(approx$1);
            var approxRem = mul(approxRes, other);
            while(approxRem[1] < 0 || gt(approxRem, rem$1)) {
              approx$1 -= delta;
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
      }
      
    }
    
  };
}

function mod_(self, other) {
  var y = mul(div(self, other), other);
  return add(self, neg(y));
}

exports.min_int  = min_int;
exports.max_int  = max_int;
exports.one      = one;
exports.zero     = zero;
exports.not      = not;
exports.of_int32 = of_int32;
exports.add      = add;
exports.neg      = neg;
exports.sub      = sub;
exports.lsl_     = lsl_;
exports.lsr_     = lsr_;
exports.asr_     = asr_;
exports.is_zero  = is_zero;
exports.mul      = mul;
exports.swap     = swap;
exports.ge       = ge;
exports.eq       = eq;
exports.neq      = neq;
exports.lt       = lt;
exports.gt       = gt;
exports.le       = le;
exports.to_float = to_float;
exports.of_float = of_float;
exports.div      = div;
exports.mod_     = mod_;
/* two_ptr_32_dbl Not a pure module */
