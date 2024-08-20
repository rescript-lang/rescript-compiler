

import * as Caml from "./caml.js";
import * as Caml_int64 from "./caml_int64.js";

function parse_digit(c) {
  if (c >= 65) {
    if (c >= 97) {
      if (c >= 123) {
        return -1;
      } else {
        return c - 87 | 0;
      }
    } else if (c >= 91) {
      return -1;
    } else {
      return c - 55 | 0;
    }
  } else if (c > 57 || c < 48) {
    return -1;
  } else {
    return c - 48 | 0;
  }
}

function int_of_string_base(x) {
  switch (x) {
    case "Oct" :
      return 8;
    case "Hex" :
      return 16;
    case "Dec" :
      return 10;
    case "Bin" :
      return 2;
  }
}

function parse_sign_and_base(s) {
  let sign = 1;
  let base = "Dec";
  let i = 0;
  let match = s.codePointAt(i);
  switch (match) {
    case 43 :
      i = i + 1 | 0;
      break;
    case 45 :
      sign = -1;
      i = i + 1 | 0;
      break;
  }
  if (s.codePointAt(i) === /* '0' */48) {
    let match$1 = s.codePointAt(i + 1 | 0);
    if (match$1 >= 89) {
      if (match$1 >= 111) {
        if (match$1 < 121) {
          switch (match$1) {
            case 111 :
              base = "Oct";
              i = i + 2 | 0;
              break;
            case 117 :
              i = i + 2 | 0;
              break;
            case 112 :
            case 113 :
            case 114 :
            case 115 :
            case 116 :
            case 118 :
            case 119 :
              break;
            case 120 :
              base = "Hex";
              i = i + 2 | 0;
              break;
          }
        }
        
      } else if (match$1 === 98) {
        base = "Bin";
        i = i + 2 | 0;
      }
      
    } else if (match$1 !== 66) {
      if (match$1 >= 79) {
        switch (match$1) {
          case 79 :
            base = "Oct";
            i = i + 2 | 0;
            break;
          case 85 :
            i = i + 2 | 0;
            break;
          case 80 :
          case 81 :
          case 82 :
          case 83 :
          case 84 :
          case 86 :
          case 87 :
            break;
          case 88 :
            base = "Hex";
            i = i + 2 | 0;
            break;
        }
      }
      
    } else {
      base = "Bin";
      i = i + 2 | 0;
    }
  }
  return [
    i,
    sign,
    base
  ];
}

function int_of_string(s) {
  let match = parse_sign_and_base(s);
  let i = match[0];
  let base = int_of_string_base(match[2]);
  let threshold = 4294967295;
  let len = s.length;
  let c = i < len ? s.codePointAt(i) : /* '\000' */0;
  let d = parse_digit(c);
  if (d < 0 || d >= base) {
    throw new Error("Failure", {
      cause: {
        RE_EXN_ID: "Failure",
        _1: "int_of_string"
      }
    });
  }
  let aux = function (_acc, _k) {
    while (true) {
      let k = _k;
      let acc = _acc;
      if (k === len) {
        return acc;
      }
      let a = s.codePointAt(k);
      if (a === /* '_' */95) {
        _k = k + 1 | 0;
        continue;
      }
      let v = parse_digit(a);
      if (v < 0 || v >= base) {
        throw new Error("Failure", {
          cause: {
            RE_EXN_ID: "Failure",
            _1: "int_of_string"
          }
        });
      }
      let acc$1 = base * acc + v;
      if (acc$1 > threshold) {
        throw new Error("Failure", {
          cause: {
            RE_EXN_ID: "Failure",
            _1: "int_of_string"
          }
        });
      }
      _k = k + 1 | 0;
      _acc = acc$1;
      continue;
    };
  };
  let res = match[1] * aux(d, i + 1 | 0);
  let or_res = res | 0;
  if (base === 10 && res !== or_res) {
    throw new Error("Failure", {
      cause: {
        RE_EXN_ID: "Failure",
        _1: "int_of_string"
      }
    });
  }
  return or_res;
}

function int64_of_string(s) {
  let match = parse_sign_and_base(s);
  let hbase = match[2];
  let i = match[0];
  let base = Caml_int64.of_int32(int_of_string_base(hbase));
  let sign = Caml_int64.of_int32(match[1]);
  let threshold;
  switch (hbase) {
    case "Oct" :
      threshold = [
        536870911,
        4294967295
      ];
      break;
    case "Hex" :
      threshold = [
        268435455,
        4294967295
      ];
      break;
    case "Dec" :
      threshold = [
        429496729,
        2576980377
      ];
      break;
    case "Bin" :
      threshold = Caml_int64.max_int;
      break;
  }
  let len = s.length;
  let c = i < len ? s.codePointAt(i) : /* '\000' */0;
  let d = Caml_int64.of_int32(parse_digit(c));
  if (Caml.i64_lt(d, Caml_int64.zero) || Caml.i64_ge(d, base)) {
    throw new Error("Failure", {
      cause: {
        RE_EXN_ID: "Failure",
        _1: "int64_of_string"
      }
    });
  }
  let aux = function (_acc, _k) {
    while (true) {
      let k = _k;
      let acc = _acc;
      if (k === len) {
        return acc;
      }
      let a = s.codePointAt(k);
      if (a === /* '_' */95) {
        _k = k + 1 | 0;
        continue;
      }
      let v = Caml_int64.of_int32(parse_digit(a));
      if (Caml.i64_lt(v, Caml_int64.zero) || Caml.i64_ge(v, base) || Caml.i64_gt(acc, threshold)) {
        throw new Error("Failure", {
          cause: {
            RE_EXN_ID: "Failure",
            _1: "int64_of_string"
          }
        });
      }
      let acc$1 = Caml_int64.add(Caml_int64.mul(base, acc), v);
      _k = k + 1 | 0;
      _acc = acc$1;
      continue;
    };
  };
  let res = Caml_int64.mul(sign, aux(d, i + 1 | 0));
  let or_res = Caml_int64.or_(res, Caml_int64.zero);
  if (Caml.i64_eq(base, [
      0,
      10
    ]) && Caml.i64_neq(res, or_res)) {
    throw new Error("Failure", {
      cause: {
        RE_EXN_ID: "Failure",
        _1: "int64_of_string"
      }
    });
  }
  return or_res;
}

function int_of_base(x) {
  switch (x) {
    case "Oct" :
      return 8;
    case "Hex" :
      return 16;
    case "Dec" :
      return 10;
  }
}

function lowercase(c) {
  if (c >= /* 'A' */65 && c <= /* 'Z' */90 || c >= /* '\192' */192 && c <= /* '\214' */214 || c >= /* '\216' */216 && c <= /* '\222' */222) {
    return c + 32 | 0;
  } else {
    return c;
  }
}

function parse_format(fmt) {
  let len = fmt.length;
  if (len > 31) {
    throw new Error("Invalid_argument", {
      cause: {
        RE_EXN_ID: "Invalid_argument",
        _1: "format_int: format too long"
      }
    });
  }
  let f = {
    justify: "+",
    signstyle: "-",
    filter: " ",
    alternate: false,
    base: "Dec",
    signedconv: false,
    width: 0,
    uppercase: false,
    sign: 1,
    prec: -1,
    conv: "f"
  };
  let _i = 0;
  while (true) {
    let i = _i;
    if (i >= len) {
      return f;
    }
    let c = fmt.codePointAt(i);
    let exit = 0;
    if (c >= 69) {
      if (c >= 88) {
        if (c >= 121) {
          exit = 1;
        } else {
          switch (c) {
            case 88 :
              f.base = "Hex";
              f.uppercase = true;
              _i = i + 1 | 0;
              continue;
            case 101 :
            case 102 :
            case 103 :
              exit = 5;
              break;
            case 100 :
            case 105 :
              exit = 4;
              break;
            case 111 :
              f.base = "Oct";
              _i = i + 1 | 0;
              continue;
            case 117 :
              f.base = "Dec";
              _i = i + 1 | 0;
              continue;
            case 89 :
            case 90 :
            case 91 :
            case 92 :
            case 93 :
            case 94 :
            case 95 :
            case 96 :
            case 97 :
            case 98 :
            case 99 :
            case 104 :
            case 106 :
            case 107 :
            case 108 :
            case 109 :
            case 110 :
            case 112 :
            case 113 :
            case 114 :
            case 115 :
            case 116 :
            case 118 :
            case 119 :
              exit = 1;
              break;
            case 120 :
              f.base = "Hex";
              _i = i + 1 | 0;
              continue;
          }
        }
      } else if (c >= 72) {
        exit = 1;
      } else {
        f.signedconv = true;
        f.uppercase = true;
        f.conv = String.fromCharCode(lowercase(c));
        _i = i + 1 | 0;
        continue;
      }
    } else {
      switch (c) {
        case 35 :
          f.alternate = true;
          _i = i + 1 | 0;
          continue;
        case 32 :
        case 43 :
          exit = 2;
          break;
        case 45 :
          f.justify = "-";
          _i = i + 1 | 0;
          continue;
        case 46 :
          f.prec = 0;
          let j = i + 1 | 0;
          while ((function () {
              let w = fmt.codePointAt(j) - 48 | 0;
              return w >= 0 && w <= 9;
            })()) {
            f.prec = (Math.imul(f.prec, 10) + fmt.codePointAt(j) | 0) - 48 | 0;
            j = j + 1 | 0;
          };
          _i = j;
          continue;
        case 48 :
          f.filter = "0";
          _i = i + 1 | 0;
          continue;
        case 49 :
        case 50 :
        case 51 :
        case 52 :
        case 53 :
        case 54 :
        case 55 :
        case 56 :
        case 57 :
          exit = 3;
          break;
        default:
          exit = 1;
      }
    }
    switch (exit) {
      case 1 :
        _i = i + 1 | 0;
        continue;
      case 2 :
        f.signstyle = String.fromCharCode(c);
        _i = i + 1 | 0;
        continue;
      case 3 :
        f.width = 0;
        let j$1 = i;
        while ((function () {
            let w = fmt.codePointAt(j$1) - 48 | 0;
            return w >= 0 && w <= 9;
          })()) {
          f.width = (Math.imul(f.width, 10) + fmt.codePointAt(j$1) | 0) - 48 | 0;
          j$1 = j$1 + 1 | 0;
        };
        _i = j$1;
        continue;
      case 4 :
        f.signedconv = true;
        f.base = "Dec";
        _i = i + 1 | 0;
        continue;
      case 5 :
        f.signedconv = true;
        f.conv = String.fromCharCode(c);
        _i = i + 1 | 0;
        continue;
    }
  };
}

function finish_formatting(config, rawbuffer) {
  let justify = config.justify;
  let signstyle = config.signstyle;
  let filter = config.filter;
  let alternate = config.alternate;
  let base = config.base;
  let signedconv = config.signedconv;
  let width = config.width;
  let uppercase = config.uppercase;
  let sign = config.sign;
  let len = rawbuffer.length;
  if (signedconv && (sign < 0 || signstyle !== "-")) {
    len = len + 1 | 0;
  }
  if (alternate) {
    if (base === "Oct") {
      len = len + 1 | 0;
    } else if (base === "Hex") {
      len = len + 2 | 0;
    }
    
  }
  let buffer = "";
  if (justify === "+" && filter === " ") {
    for (let _for = len; _for < width; ++_for) {
      buffer = buffer + filter;
    }
  }
  if (signedconv) {
    if (sign < 0) {
      buffer = buffer + "-";
    } else if (signstyle !== "-") {
      buffer = buffer + signstyle;
    }
    
  }
  if (alternate && base === "Oct") {
    buffer = buffer + "0";
  }
  if (alternate && base === "Hex") {
    buffer = buffer + "0x";
  }
  if (justify === "+" && filter === "0") {
    for (let _for$1 = len; _for$1 < width; ++_for$1) {
      buffer = buffer + filter;
    }
  }
  buffer = uppercase ? buffer + rawbuffer.toUpperCase() : buffer + rawbuffer;
  if (justify === "-") {
    for (let _for$2 = len; _for$2 < width; ++_for$2) {
      buffer = buffer + " ";
    }
  }
  return buffer;
}

function format_int(fmt, i) {
  if (fmt === "%d") {
    return String(i);
  }
  let f = parse_format(fmt);
  let i$1 = i < 0 ? (
      f.signedconv ? (f.sign = -1, (-i >>> 0)) : (i >>> 0)
    ) : i;
  let s = i$1.toString(int_of_base(f.base));
  if (f.prec >= 0) {
    f.filter = " ";
    let n = f.prec - s.length | 0;
    if (n > 0) {
      s = "0".repeat(n) + s;
    }
    
  }
  return finish_formatting(f, s);
}

function dec_of_pos_int64(x) {
  if (!Caml.i64_lt(x, Caml_int64.zero)) {
    return Caml_int64.to_string(x);
  }
  let wbase = [
    0,
    10
  ];
  let y = Caml_int64.discard_sign(x);
  let match = Caml_int64.div_mod(y, wbase);
  let match$1 = Caml_int64.div_mod(Caml_int64.add([
    0,
    8
  ], match[1]), wbase);
  let quotient = Caml_int64.add(Caml_int64.add([
    214748364,
    3435973836
  ], match[0]), match$1[0]);
  return Caml_int64.to_string(quotient) + "0123456789"[Caml_int64.to_int32(match$1[1])];
}

function oct_of_int64(x) {
  let s = "";
  let wbase = [
    0,
    8
  ];
  let cvtbl = "01234567";
  if (Caml.i64_lt(x, Caml_int64.zero)) {
    let y = Caml_int64.discard_sign(x);
    let match = Caml_int64.div_mod(y, wbase);
    let quotient = Caml_int64.add([
      268435456,
      0
    ], match[0]);
    let modulus = match[1];
    s = cvtbl[Caml_int64.to_int32(modulus)] + s;
    while (Caml.i64_neq(quotient, Caml_int64.zero)) {
      let match$1 = Caml_int64.div_mod(quotient, wbase);
      quotient = match$1[0];
      modulus = match$1[1];
      s = cvtbl[Caml_int64.to_int32(modulus)] + s;
    };
  } else {
    let match$2 = Caml_int64.div_mod(x, wbase);
    let quotient$1 = match$2[0];
    let modulus$1 = match$2[1];
    s = cvtbl[Caml_int64.to_int32(modulus$1)] + s;
    while (Caml.i64_neq(quotient$1, Caml_int64.zero)) {
      let match$3 = Caml_int64.div_mod(quotient$1, wbase);
      quotient$1 = match$3[0];
      modulus$1 = match$3[1];
      s = cvtbl[Caml_int64.to_int32(modulus$1)] + s;
    };
  }
  return s;
}

function int64_format(fmt, x) {
  if (fmt === "%d") {
    return Caml_int64.to_string(x);
  }
  let f = parse_format(fmt);
  let x$1 = f.signedconv && Caml.i64_lt(x, Caml_int64.zero) ? (f.sign = -1, Caml_int64.neg(x)) : x;
  let match = f.base;
  let s;
  switch (match) {
    case "Oct" :
      s = oct_of_int64(x$1);
      break;
    case "Hex" :
      s = Caml_int64.to_hex(x$1);
      break;
    case "Dec" :
      s = dec_of_pos_int64(x$1);
      break;
  }
  let fill_s;
  if (f.prec >= 0) {
    f.filter = " ";
    let n = f.prec - s.length | 0;
    fill_s = n > 0 ? "0".repeat(n) + s : s;
  } else {
    fill_s = s;
  }
  return finish_formatting(f, fill_s);
}

function format_float(fmt, x) {
  let f = parse_format(fmt);
  let prec = f.prec < 0 ? 6 : f.prec;
  let x$1 = x < 0 ? (f.sign = -1, - x) : x;
  let s = "";
  if (isNaN(x$1)) {
    s = "nan";
    f.filter = " ";
  } else if (isFinite(x$1)) {
    let match = f.conv;
    switch (match) {
      case "e" :
        s = x$1.toExponential(prec);
        let i = s.length;
        if (s.codePointAt(i - 3 | 0) === /* 'e' */101) {
          s = s.slice(0, i - 1 | 0) + ("0" + s.slice(i - 1 | 0));
        }
        break;
      case "f" :
        s = x$1.toFixed(prec);
        break;
      case "g" :
        let prec$1 = prec !== 0 ? prec : 1;
        s = x$1.toExponential(prec$1 - 1 | 0);
        let j = s.indexOf("e");
        let exp = Number(s.slice(j + 1 | 0)) | 0;
        if (exp < -4 || x$1 >= 1e21 || x$1.toFixed().length > prec$1) {
          let i$1 = j - 1 | 0;
          while (s.codePointAt(i$1) === /* '0' */48) {
            i$1 = i$1 - 1 | 0;
          };
          if (s.codePointAt(i$1) === /* '.' */46) {
            i$1 = i$1 - 1 | 0;
          }
          s = s.slice(0, i$1 + 1 | 0) + s.slice(j);
          let i$2 = s.length;
          if (s.codePointAt(i$2 - 3 | 0) === /* 'e' */101) {
            s = s.slice(0, i$2 - 1 | 0) + ("0" + s.slice(i$2 - 1 | 0));
          }
          
        } else {
          let p = prec$1;
          if (exp < 0) {
            p = p - (exp + 1 | 0) | 0;
            s = x$1.toFixed(p);
          } else {
            while ((function () {
                s = x$1.toFixed(p);
                return s.length > (prec$1 + 1 | 0);
              })()) {
              p = p - 1 | 0;
            };
          }
          if (p !== 0) {
            let k = s.length - 1 | 0;
            while (s.codePointAt(k) === /* '0' */48) {
              k = k - 1 | 0;
            };
            if (s.codePointAt(k) === /* '.' */46) {
              k = k - 1 | 0;
            }
            s = s.slice(0, k + 1 | 0);
          }
          
        }
        break;
    }
  } else {
    s = "inf";
    f.filter = " ";
  }
  return finish_formatting(f, s);
}

let hexstring_of_float = (function(x,prec,style){
  if (!isFinite(x)) {
    if (isNaN(x)) return "nan";
    return x > 0 ? "infinity":"-infinity";
  }
  var sign = (x==0 && 1/x == -Infinity)?1:(x>=0)?0:1;
  if(sign) x = -x;
  var exp = 0;
  if (x == 0) { }
  else if (x < 1) {
    while (x < 1 && exp > -1022)  { x *= 2; exp-- }
  } else {
    while (x >= 2) { x /= 2; exp++ }
  }
  var exp_sign = exp < 0 ? '' : '+';
  var sign_str = '';
  if (sign) sign_str = '-'
  else {
    switch(style){
    case 43 /* '+' */: sign_str = '+'; break;
    case 32 /* ' ' */: sign_str = ' '; break;
    default: break;
    }
  }
  if (prec >= 0 && prec < 13) {
    /* If a precision is given, and is small, round mantissa accordingly */
      var cst = Math.pow(2,prec * 4);
      x = Math.round(x * cst) / cst;
  }
  var x_str = x.toString(16);
  if(prec >= 0){
      var idx = x_str.indexOf('.');
    if(idx<0) {
      x_str += '.' +  '0'.repeat(prec);
    }
    else {
      var size = idx+1+prec;
      if(x_str.length < size)
        x_str += '0'.repeat(size - x_str.length);
      else
        x_str = x_str.substr(0,size);
    }
  }
  return  (sign_str + '0x' + x_str + 'p' + exp_sign + exp.toString(10));
});

let float_of_string = (function(s,exn){

    var res = +s;
    if ((s.length > 0) && (res === res))
        return res;
    s = s.replace(/_/g, "");
    res = +s;
    if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) {
        return res;
    };
    var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)p([+-]?[0-9]+)/i.exec(s);
    //            1        2             3           4
    if(m){
        var m3 = m[3].replace(/0+$/,'');
        var mantissa = parseInt(m[1] + m[2] + m3, 16);
        var exponent = (m[4]|0) - 4*m3.length;
        res = mantissa * Math.pow(2, exponent);
        return res;
    }
    if (/^\+?inf(inity)?$/i.test(s))
        return Infinity;
    if (/^-inf(inity)?$/i.test(s))
        return -Infinity;
    throw new Error(exn.RE_EXN_ID, { cause: exn });;
});

function float_of_string$1(s) {
  return float_of_string(s, new Error("Failure", {
    cause: {
      RE_EXN_ID: "Failure",
      _1: "float_of_string"
    }
  }));
}

export {
  format_float,
  hexstring_of_float,
  format_int,
  float_of_string$1 as float_of_string,
  int64_format,
  int_of_string,
  int64_of_string,
}
/* No side effect */
