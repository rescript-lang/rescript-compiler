// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


var min_int = /* record */[
  0,
  2147483648
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

var min_int$1 = /* record */[
  0,
  2147483648
];

function mul(_this, _other) {
  while(true) {
    var other = _other;
    var $$this = _this;
    var exit = 0;
    var lo;
    var lo$1 = $$this[0];
    var exit$1 = 0;
    if (lo$1 !== 0) {
      exit$1 = 2;
    }
    else if ($$this[1] !== 0) {
      exit$1 = 2;
    }
    else {
      return zero;
    }
    switch (exit$1) {
      case 2 : 
          if (other[0] !== 0) {
            exit$1 = 3;
          }
          else if (other[1] !== 0) {
            exit$1 = 3;
          }
          else {
            return zero;
          }
          break;
      case 3 : 
          if (lo$1 !== 0) {
            exit$1 = 4;
          }
          else if ($$this[1] !== 2147483648) {
            exit$1 = 4;
          }
          else {
            lo = other[0];
            exit = 1;
          }
          break;
      case 4 : 
          var this_hi = $$this[1];
          var exit$2 = 0;
          if (other[0] !== 0) {
            exit$2 = 5;
          }
          else if (other[1] !== 2147483648) {
            exit$2 = 5;
          }
          else {
            lo = lo$1;
            exit = 1;
          }
          if (exit$2 === 5) {
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
              var b16 = (other_hi >>> 16);
              var b00 = other_hi & 65535;
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
          break;
      
    }
    if (exit === 1) {
      if ((lo & 1) === 0) {
        return zero;
      }
      else {
        return min_int$1;
      }
    }
    
  };
}

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
exports.min_int  = min_int$1;
exports.mul      = mul;
/* No side effect */
