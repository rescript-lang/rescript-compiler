

import * as Caml from "./caml.js";

function mk(lo, hi) {
  return [
    hi,
    (lo >>> 0)
  ];
}

let min_int = [
  -2147483648,
  0
];

let max_int = [
  2147483647,
  4294967295
];

let one = [
  0,
  1
];

let zero = [
  0,
  0
];

let neg_one = [
  -1,
  4294967295
];

function neg_signed(x) {
  return (x & -2147483648) !== 0;
}

function non_neg_signed(x) {
  return (x & -2147483648) === 0;
}

function succ(param) {
  let x_lo = param[1];
  let x_hi = param[0];
  let lo = x_lo + 1 | 0;
  return [
    x_hi + (
      lo === 0 ? 1 : 0
    ) | 0,
    (lo >>> 0)
  ];
}

function neg(param) {
  let other_lo = (param[1] ^ -1) + 1 | 0;
  return [
    (param[0] ^ -1) + (
      other_lo === 0 ? 1 : 0
    ) | 0,
    (other_lo >>> 0)
  ];
}

function add_aux(param, y_lo, y_hi) {
  let x_lo = param[1];
  let lo = x_lo + y_lo | 0;
  let overflow = neg_signed(x_lo) && (neg_signed(y_lo) || non_neg_signed(lo)) || neg_signed(y_lo) && non_neg_signed(lo) ? 1 : 0;
  return [
    param[0] + y_hi + overflow | 0,
    (lo >>> 0)
  ];
}

function add(self, param) {
  return add_aux(self, param[1], param[0]);
}

function equal(x, y) {
  if (x[1] === y[1]) {
    return x[0] === y[0];
  } else {
    return false;
  }
}

function equal_null(x, y) {
  if (y !== null) {
    return Caml.i64_eq(x, y);
  } else {
    return false;
  }
}

function equal_undefined(x, y) {
  if (y !== undefined) {
    return Caml.i64_eq(x, y);
  } else {
    return false;
  }
}

function equal_nullable(x, y) {
  if (y == null) {
    return false;
  } else {
    return Caml.i64_eq(x, y);
  }
}

function sub_aux(x, lo, hi) {
  let y_lo = ((lo ^ -1) + 1 >>> 0);
  let y_hi = (hi ^ -1) + (
    y_lo === 0 ? 1 : 0
  ) | 0;
  return add_aux(x, y_lo, y_hi);
}

function sub(self, param) {
  return sub_aux(self, param[1], param[0]);
}

function lsl_(x, numBits) {
  if (numBits === 0) {
    return x;
  }
  let lo = x[1];
  if (numBits >= 32) {
    return [
      (lo << (numBits - 32 | 0)),
      0
    ];
  } else {
    return [
      (lo >>> (32 - numBits | 0)) | (x[0] << numBits),
      ((lo << numBits) >>> 0)
    ];
  }
}

function lsr_(x, numBits) {
  if (numBits === 0) {
    return x;
  }
  let hi = x[0];
  let offset = numBits - 32 | 0;
  if (offset === 0) {
    return [
      0,
      (hi >>> 0)
    ];
  } else if (offset > 0) {
    return [
      0,
      (hi >>> offset)
    ];
  } else {
    return [
      (hi >>> numBits),
      (((hi << (-offset | 0)) | (x[1] >>> numBits)) >>> 0)
    ];
  }
}

function asr_(x, numBits) {
  if (numBits === 0) {
    return x;
  }
  let hi = x[0];
  if (numBits < 32) {
    return [
      (hi >> numBits),
      (((hi << (32 - numBits | 0)) | (x[1] >>> numBits)) >>> 0)
    ];
  } else {
    return [
      hi >= 0 ? 0 : -1,
      ((hi >> (numBits - 32 | 0)) >>> 0)
    ];
  }
}

function is_zero(x) {
  if (x[0] !== 0) {
    return false;
  } else {
    return x[1] === 0;
  }
}

function mul(_this, _other) {
  while(true) {
    let other = _other;
    let $$this = _this;
    let lo;
    let this_hi = $$this[0];
    let exit = 0;
    let exit$1 = 0;
    let exit$2 = 0;
    if (this_hi !== 0) {
      exit$2 = 4;
    } else {
      if ($$this[1] === 0) {
        return zero;
      }
      exit$2 = 4;
    }
    if (exit$2 === 4) {
      if (other[0] !== 0) {
        exit$1 = 3;
      } else {
        if (other[1] === 0) {
          return zero;
        }
        exit$1 = 3;
      }
    }
    if (exit$1 === 3) {
      if (this_hi !== -2147483648 || $$this[1] !== 0) {
        exit = 2;
      } else {
        lo = other[1];
      }
    }
    if (exit === 2) {
      let other_hi = other[0];
      let lo$1 = $$this[1];
      let exit$3 = 0;
      if (other_hi !== -2147483648 || other[1] !== 0) {
        exit$3 = 3;
      } else {
        lo = lo$1;
      }
      if (exit$3 === 3) {
        let other_lo = other[1];
        if (this_hi < 0) {
          if (other_hi >= 0) {
            return neg(mul(neg($$this), other));
          }
          _other = neg(other);
          _this = neg($$this);
          continue;
        }
        if (other_hi < 0) {
          return neg(mul($$this, neg(other)));
        }
        let a48 = (this_hi >>> 16);
        let a32 = this_hi & 65535;
        let a16 = (lo$1 >>> 16);
        let a00 = lo$1 & 65535;
        let b48 = (other_hi >>> 16);
        let b32 = other_hi & 65535;
        let b16 = (other_lo >>> 16);
        let b00 = other_lo & 65535;
        let c48 = 0;
        let c32 = 0;
        let c16 = 0;
        let c00 = a00 * b00;
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
        return [
          c32 | (c48 << 16),
          ((c00 & 65535 | ((c16 & 65535) << 16)) >>> 0)
        ];
      }
      
    }
    if ((lo & 1) === 0) {
      return zero;
    } else {
      return min_int;
    }
  };
}

function xor(param, param$1) {
  return [
    param[0] ^ param$1[0],
    ((param[1] ^ param$1[1]) >>> 0)
  ];
}

function or_(param, param$1) {
  return [
    param[0] | param$1[0],
    ((param[1] | param$1[1]) >>> 0)
  ];
}

function and_(param, param$1) {
  return [
    param[0] & param$1[0],
    ((param[1] & param$1[1]) >>> 0)
  ];
}

function to_float(param) {
  return param[0] * 0x100000000 + param[1];
}

function of_float(x) {
  if (isNaN(x) || !isFinite(x)) {
    return zero;
  }
  if (x <= -9.22337203685477581e+18) {
    return min_int;
  }
  if (x + 1 >= 9.22337203685477581e+18) {
    return max_int;
  }
  if (x < 0) {
    return neg(of_float(- x));
  }
  let hi = x / 4294967296 | 0;
  let lo = x % 4294967296 | 0;
  return [
    hi,
    (lo >>> 0)
  ];
}

function isSafeInteger(param) {
  let hi = param[0];
  let top11Bits = (hi >> 21);
  if (top11Bits === 0) {
    return true;
  } else if (top11Bits === -1) {
    return !(param[1] === 0 && hi === -2097152);
  } else {
    return false;
  }
}

function to_string(self) {
  if (isSafeInteger(self)) {
    return String(to_float(self));
  }
  if (self[0] < 0) {
    if (Caml.i64_eq(self, min_int)) {
      return "-9223372036854775808";
    } else {
      return "-" + to_string(neg(self));
    }
  }
  let approx_div1 = of_float(Math.floor(to_float(self) / 10));
  let lo = approx_div1[1];
  let hi = approx_div1[0];
  let match = sub_aux(sub_aux(self, (lo << 3), (lo >>> 29) | (hi << 3)), (lo << 1), (lo >>> 31) | (hi << 1));
  let rem_lo = match[1];
  let rem_hi = match[0];
  if (rem_lo === 0 && rem_hi === 0) {
    return to_string(approx_div1) + "0";
  }
  if (rem_hi < 0) {
    let rem_lo$1 = ((rem_lo ^ -1) + 1 >>> 0);
    let delta = Math.ceil(rem_lo$1 / 10);
    let remainder = 10 * delta - rem_lo$1;
    return to_string(sub_aux(approx_div1, delta | 0, 0)) + String(remainder | 0);
  }
  let delta$1 = Math.floor(rem_lo / 10);
  let remainder$1 = rem_lo - 10 * delta$1;
  return to_string(add_aux(approx_div1, delta$1 | 0, 0)) + String(remainder$1 | 0);
}

function div(_self, _other) {
  while(true) {
    let other = _other;
    let self = _self;
    let self_hi = self[0];
    let exit = 0;
    let exit$1 = 0;
    if (other[0] !== 0 || other[1] !== 0) {
      exit$1 = 2;
    } else {
      throw new Error("Division_by_zero", {
            cause: {
              RE_EXN_ID: "Division_by_zero"
            }
          });
    }
    if (exit$1 === 2) {
      if (self_hi !== -2147483648) {
        if (self_hi !== 0) {
          exit = 1;
        } else {
          if (self[1] === 0) {
            return zero;
          }
          exit = 1;
        }
      } else if (self[1] !== 0) {
        exit = 1;
      } else {
        if (Caml.i64_eq(other, one) || Caml.i64_eq(other, neg_one)) {
          return self;
        }
        if (Caml.i64_eq(other, min_int)) {
          return one;
        }
        let half_this = asr_(self, 1);
        let approx = lsl_(div(half_this, other), 1);
        let exit$2 = 0;
        if (approx[0] !== 0) {
          exit$2 = 3;
        } else {
          if (approx[1] === 0) {
            if (other[0] < 0) {
              return one;
            } else {
              return neg(one);
            }
          }
          exit$2 = 3;
        }
        if (exit$2 === 3) {
          let rem = sub(self, mul(other, approx));
          return add(approx, div(rem, other));
        }
        
      }
    }
    if (exit === 1) {
      let other_hi = other[0];
      let exit$3 = 0;
      if (other_hi !== -2147483648) {
        exit$3 = 2;
      } else {
        if (other[1] === 0) {
          return zero;
        }
        exit$3 = 2;
      }
      if (exit$3 === 2) {
        if (self_hi < 0) {
          if (other_hi >= 0) {
            return neg(div(neg(self), other));
          }
          _other = neg(other);
          _self = neg(self);
          continue;
        }
        if (other_hi < 0) {
          return neg(div(self, neg(other)));
        }
        let res = zero;
        let rem$1 = self;
        while(Caml.i64_ge(rem$1, other)) {
          let b = Math.floor(to_float(rem$1) / to_float(other));
          let approx$1 = 1 > b ? 1 : b;
          let log2 = Math.ceil(Math.log(approx$1) / Math.LN2);
          let delta = log2 <= 48 ? 1 : Math.pow(2, log2 - 48);
          let approxRes = of_float(approx$1);
          let approxRem = mul(approxRes, other);
          while(approxRem[0] < 0 || Caml.i64_gt(approxRem, rem$1)) {
            approx$1 = approx$1 - delta;
            approxRes = of_float(approx$1);
            approxRem = mul(approxRes, other);
          };
          if (is_zero(approxRes)) {
            approxRes = one;
          }
          res = add(res, approxRes);
          rem$1 = sub(rem$1, approxRem);
        };
        return res;
      }
      
    }
    
  };
}

function mod_(self, other) {
  return sub(self, mul(div(self, other), other));
}

function div_mod(self, other) {
  let quotient = div(self, other);
  return [
    quotient,
    sub(self, mul(quotient, other))
  ];
}

function compare(self, other) {
  let y = other[0];
  let x = self[0];
  let v = x < y ? -1 : (
      x === y ? 0 : 1
    );
  if (v !== 0) {
    return v;
  }
  let y$1 = other[1];
  let x$1 = self[1];
  if (x$1 < y$1) {
    return -1;
  } else if (x$1 === y$1) {
    return 0;
  } else {
    return 1;
  }
}

function of_int32(lo) {
  return [
    lo < 0 ? -1 : 0,
    (lo >>> 0)
  ];
}

function to_int32(x) {
  return x[1] | 0;
}

function to_hex(x) {
  let x_lo = x[1];
  let x_hi = x[0];
  let aux = function (v) {
    return (v >>> 0).toString(16);
  };
  if (x_hi === 0 && x_lo === 0) {
    return "0";
  }
  if (x_lo === 0) {
    return aux(x_hi) + "00000000";
  }
  if (x_hi === 0) {
    return aux(x_lo);
  }
  let lo = aux(x_lo);
  let pad = 8 - lo.length | 0;
  if (pad <= 0) {
    return aux(x_hi) + lo;
  } else {
    return aux(x_hi) + ("0".repeat(pad) + lo);
  }
}

function discard_sign(x) {
  return [
    2147483647 & x[0],
    x[1]
  ];
}

function float_of_bits(x) {
  return (function(lo,hi){ return (new Float64Array(new Int32Array([lo,hi]).buffer))[0]})(x[1], x[0]);
}

function bits_of_float(x) {
  let match = (function(x){return new Int32Array(new Float64Array([x]).buffer)})(x);
  return [
    match[1],
    (match[0] >>> 0)
  ];
}

export {
  mk,
  succ,
  min_int,
  max_int,
  one,
  zero,
  neg_one,
  of_int32,
  to_int32,
  add,
  neg,
  sub,
  lsl_,
  lsr_,
  asr_,
  is_zero,
  mul,
  xor,
  or_,
  and_,
  equal,
  equal_null,
  equal_undefined,
  equal_nullable,
  to_float,
  of_float,
  div,
  mod_,
  compare,
  float_of_bits,
  bits_of_float,
  div_mod,
  to_hex,
  discard_sign,
  to_string,
}
/* No side effect */
