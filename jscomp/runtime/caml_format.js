// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("./caml_builtin_exceptions");
var Caml_utils              = require("./caml_utils");
var Caml_curry              = require("./caml_curry");

var repeat = Caml_utils.repeat;

function caml_failwith(s) {
  throw [
        Caml_builtin_exceptions.failure,
        s
      ];
}

function caml_invalid_argument(s) {
  throw [
        Caml_builtin_exceptions.invalid_argument,
        s
      ];
}

function parse_digit(c) {
  if (c >= 65) {
    if (c >= 97) {
      if (c >= 123) {
        return -1;
      }
      else {
        return c - 87;
      }
    }
    else if (c >= 91) {
      return -1;
    }
    else {
      return c - 55;
    }
  }
  else if (c > 57 || c < 48) {
    return -1;
  }
  else {
    return c - /* "0" */48;
  }
}

function parse_sign_and_base(s) {
  var sign = 1;
  var base = 10;
  var i = 0;
  if (s[i] === "-") {
    sign = -1;
    ++ i;
  }
  var match = s.charCodeAt(i);
  var match$1 = s.charCodeAt(i + 1);
  if (match === 48) {
    if (match$1 >= 89) {
      if (match$1 !== 98) {
        if (match$1 !== 111) {
          if (match$1 === 120) {
            base = 16;
            i += 2;
          }
          
        }
        else {
          base = 8;
          i += 2;
        }
      }
      else {
        base = 2;
        i += 2;
      }
    }
    else if (match$1 !== 66) {
      if (match$1 !== 79) {
        if (match$1 >= 88) {
          base = 16;
          i += 2;
        }
        
      }
      else {
        base = 8;
        i += 2;
      }
    }
    else {
      base = 2;
      i += 2;
    }
  }
  return /* tuple */[
          i,
          sign,
          base
        ];
}

function caml_int_of_string(s) {
  var match = parse_sign_and_base(s);
  var base = match[2];
  var i = match[0];
  var threshold = (-1 >>> 0);
  var len = s.length;
  var c = i < len ? s.charCodeAt(i) : /* "\000" */0;
  var d = parse_digit(c);
  if (d < 0 || d >= base) {
    throw [
          Caml_builtin_exceptions.failure,
          "int_of_string"
        ];
  }
  var aux = function (_acc, _k) {
    while(true) {
      var k = _k;
      var acc = _acc;
      if (k === len) {
        return acc;
      }
      else {
        var a = s.charCodeAt(k);
        if (a === /* "_" */95) {
          _k = k + 1;
          continue ;
          
        }
        else {
          var v = parse_digit(a);
          if (v < 0 || v >= base) {
            throw [
                  Caml_builtin_exceptions.failure,
                  "int_of_string"
                ];
          }
          else {
            var acc$1 = base * acc + v;
            if (acc$1 > threshold) {
              throw [
                    Caml_builtin_exceptions.failure,
                    "int_of_string"
                  ];
            }
            else {
              _k = k + 1;
              _acc = acc$1;
              continue ;
              
            }
          }
        }
      }
    };
  };
  var res = match[1] * aux(d, i + 1);
  var or_res = res | 0;
  if (base === 10 && res !== or_res) {
    throw [
          Caml_builtin_exceptions.failure,
          "int_of_string"
        ];
  }
  return or_res;
}

function lowercase(c) {
  if (c >= /* "A" */65 && c <= /* "Z" */90 || c >= /* "\192" */192 && c <= /* "\214" */214 || c >= /* "\216" */216 && c <= /* "\222" */222) {
    return c + 32;
  }
  else {
    return c;
  }
}

function _parse_format(fmt) {
  var len = fmt.length;
  if (len > 31) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "format_int: format too long"
        ];
  }
  var f = /* record */[
    "+",
    "-",
    " ",
    /* false */0,
    0,
    /* false */0,
    0,
    /* false */0,
    1,
    -1,
    "f"
  ];
  var _i = 0;
  while(true) {
    var i = _i;
    if (i >= len) {
      return f;
    }
    else {
      var c = fmt.charCodeAt(i);
      var exit = 0;
      if (c >= 69) {
        if (c >= 88) {
          if (c >= 121) {
            exit = 1;
          }
          else {
            switch (c - 88) {
              case 0 : 
                  f[/* base */4] = 16;
                  f[/* uppercase */7] = /* true */1;
                  _i = i + 1;
                  continue ;
                  case 13 : 
              case 14 : 
              case 15 : 
                  exit = 5;
                  break;
              case 12 : 
              case 17 : 
                  exit = 4;
                  break;
              case 23 : 
                  f[/* base */4] = 8;
                  _i = i + 1;
                  continue ;
                  case 29 : 
                  f[/* base */4] = 10;
                  _i = i + 1;
                  continue ;
                  case 1 : 
              case 2 : 
              case 3 : 
              case 4 : 
              case 5 : 
              case 6 : 
              case 7 : 
              case 8 : 
              case 9 : 
              case 10 : 
              case 11 : 
              case 16 : 
              case 18 : 
              case 19 : 
              case 20 : 
              case 21 : 
              case 22 : 
              case 24 : 
              case 25 : 
              case 26 : 
              case 27 : 
              case 28 : 
              case 30 : 
              case 31 : 
                  exit = 1;
                  break;
              case 32 : 
                  f[/* base */4] = 16;
                  _i = i + 1;
                  continue ;
                  
            }
          }
        }
        else if (c >= 72) {
          exit = 1;
        }
        else {
          f[/* signedconv */5] = /* true */1;
          f[/* uppercase */7] = /* true */1;
          f[/* conv */10] = String.fromCharCode(lowercase(c));
          _i = i + 1;
          continue ;
          
        }
      }
      else {
        var switcher = c - 32;
        if (switcher > 25 || switcher < 0) {
          exit = 1;
        }
        else {
          switch (switcher) {
            case 3 : 
                f[/* alternate */3] = /* true */1;
                _i = i + 1;
                continue ;
                case 0 : 
            case 11 : 
                exit = 2;
                break;
            case 13 : 
                f[/* justify */0] = "-";
                _i = i + 1;
                continue ;
                case 14 : 
                f[/* prec */9] = 0;
                var j = i + 1;
                while((function(j){
                    return function () {
                      var w = fmt.charCodeAt(j) - /* "0" */48;
                      return +(w >= 0 && w <= 9);
                    }
                    }(j))()) {
                  f[/* prec */9] = f[/* prec */9] * 10 + fmt.charCodeAt(j) - /* "0" */48;
                  ++ j;
                };
                _i = j;
                continue ;
                case 1 : 
            case 2 : 
            case 4 : 
            case 5 : 
            case 6 : 
            case 7 : 
            case 8 : 
            case 9 : 
            case 10 : 
            case 12 : 
            case 15 : 
                exit = 1;
                break;
            case 16 : 
                f[/* filter */2] = "0";
                _i = i + 1;
                continue ;
                case 17 : 
            case 18 : 
            case 19 : 
            case 20 : 
            case 21 : 
            case 22 : 
            case 23 : 
            case 24 : 
            case 25 : 
                exit = 3;
                break;
            
          }
        }
      }
      switch (exit) {
        case 1 : 
            _i = i + 1;
            continue ;
            case 2 : 
            f[/* signstyle */1] = String.fromCharCode(c);
            _i = i + 1;
            continue ;
            case 3 : 
            f[/* width */6] = 0;
            var j$1 = i;
            while((function(j$1){
                return function () {
                  var w = fmt.charCodeAt(j$1) - /* "0" */48;
                  return +(w >= 0 && w <= 9);
                }
                }(j$1))()) {
              f[/* width */6] = f[/* width */6] * 10 + fmt.charCodeAt(j$1) - /* "0" */48;
              ++ j$1;
            };
            _i = j$1;
            continue ;
            case 4 : 
            f[/* signedconv */5] = /* true */1;
            f[/* base */4] = 10;
            _i = i + 1;
            continue ;
            case 5 : 
            f[/* signedconv */5] = /* true */1;
            f[/* conv */10] = String.fromCharCode(c);
            _i = i + 1;
            continue ;
            
      }
    }
  };
}

function _finish_formatting(param, rawbuffer) {
  var justify = param[/* justify */0];
  var signstyle = param[/* signstyle */1];
  var filter = param[/* filter */2];
  var alternate = param[/* alternate */3];
  var base = param[/* base */4];
  var signedconv = param[/* signedconv */5];
  var width = param[/* width */6];
  var uppercase = param[/* uppercase */7];
  var sign = param[/* sign */8];
  var len = rawbuffer.length;
  if (signedconv && (sign < 0 || signstyle !== "-")) {
    ++ len;
  }
  if (alternate) {
    if (base === 8) {
      ++ len;
    }
    else if (base === 16) {
      len += 2;
    }
    
  }
  var buffer = "";
  if (justify === "+" && filter === " ") {
    for(var i = len ,i_finish = width - 1; i<= i_finish; ++i){
      buffer = buffer + filter;
    }
  }
  if (signedconv) {
    if (sign < 0) {
      buffer = buffer + "-";
    }
    else if (signstyle !== "-") {
      buffer = buffer + signstyle;
    }
    
  }
  if (alternate && base === 8) {
    buffer = buffer + "0";
  }
  if (alternate && base === 16) {
    buffer = buffer + "0x";
  }
  if (justify === "+" && filter === "0") {
    for(var i$1 = len ,i_finish$1 = width - 1; i$1<= i_finish$1; ++i$1){
      buffer = buffer + filter;
    }
  }
  buffer = uppercase ? buffer + rawbuffer.toUpperCase() : buffer + rawbuffer;
  if (justify === "-") {
    for(var i$2 = len ,i_finish$2 = width - 1; i$2<= i_finish$2; ++i$2){
      buffer = buffer + " ";
    }
  }
  return buffer;
}

function caml_format_int(fmt, i) {
  if (fmt === "%d") {
    return "" + i;
  }
  else {
    var f = _parse_format(fmt);
    var i$1 = i < 0 ? (
        f[/* signedconv */5] ? (f[/* sign */8] = -1, -i) : (i >>> 0)
      ) : i;
    var s = i$1.toString(f[/* base */4]);
    if (f[/* prec */9] >= 0) {
      f[/* filter */2] = " ";
      var n = f[/* prec */9] - s.length;
      if (n > 0) {
        s = Caml_curry.app2(repeat, n, "0") + s;
      }
      
    }
    return _finish_formatting(f, s);
  }
}


function $$js_anything_to_number(x){
   return +x;
}

;

function caml_format_float(fmt, x) {
  var f = _parse_format(fmt);
  var prec = f[/* prec */9] < 0 ? 6 : f[/* prec */9];
  var x$1 = x < 0 ? (f[/* sign */8] = -1, -x) : x;
  var s = "";
  if (isNaN(x$1)) {
    s = "nan";
    f[/* filter */2] = " ";
  }
  else if (isFinite(x$1)) {
    var match = f[/* conv */10];
    switch (match) {
      case "e" : 
          s = x$1.toExponential(prec);
          var i = s.length;
          if (s[i - 3] === "e") {
            s = s.slice(0, i - 1) + ("0" + s.slice(i - 1));
          }
          break;
      case "f" : 
          s = x$1.toFixed(prec);
          break;
      case "g" : 
          var prec$1 = prec !== 0 ? prec : 1;
          s = x$1.toExponential(prec$1 - 1);
          var j = s.indexOf("e");
          var exp = $$js_anything_to_number(s.slice(j + 1));
          if (exp < -4 || x$1 >= 1e21 || x$1.toFixed(0).length > prec$1) {
            var i$1 = j - 1;
            while(s[i$1] === "0") {
              -- i$1;
            };
            if (s[i$1] === ".") {
              -- i$1;
            }
            s = s.slice(0, i$1 + 1) + s.slice(j);
            var i$2 = s.length;
            if (s[i$2 - 3] === "e") {
              s = s.slice(0, i$2 - 1) + ("0" + s.slice(i$2 - 1));
            }
            
          }
          else {
            var p = prec$1;
            if (exp < 0) {
              p -= (exp + 1);
              s = x$1.toFixed(p);
            }
            else {
              while(function () {
                    s = x$1.toFixed(p);
                    return +(s.length > prec$1 + 1);
                  }()) {
                -- p;
              };
            }
            if (p !== 0) {
              var k = s.length - 1;
              while(s[k] === "0") {
                -- k;
              };
              if (s[k] === ".") {
                -- k;
              }
              s = s.slice(0, k + 1);
            }
            
          }
          break;
      default:
        
    }
  }
  else {
    s = "inf";
    f[/* filter */2] = " ";
  }
  return _finish_formatting(f, s);
}



/**
 * external float_of_string : string -> float = "caml_float_of_string"
 * pervasives.ml
 * Semantics is slightly different from javascript :
 * console.assert(caml_float_of_string('infinity')===Infinity)
 * console.assert(caml_float_of_string('Infinity')===Infinity
 *
 * parseFloat('Infinity') === Infinity
 * parseFloat('infinity') === Nan
 * */
function $$caml_float_of_string(s) {
    //s = caml_bytes_of_string (s);
    var res = +s;
    if ((s.length > 0) && (res === res))
        return res;
    s = s.replace(/_/g, "");
    res = +s;
    if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) {
        return res;
    }
    ;
    if (/^ *0x[0-9a-f_]+p[+-]?[0-9_]+/i.test(s)) {
        var pidx = s.indexOf('p');
        pidx = (pidx == -1) ? s.indexOf('P') : pidx;
        var exp = +s.substring(pidx + 1);
        res = +s.substring(0, pidx);
        return res * Math.pow(2, exp);
    }
    if (/^\+?inf(inity)?$/i.test(s))
        return Infinity;
    if (/^-inf(inity)?$/i.test(s))
        return -Infinity;
    caml_failwith("float_of_string");
}


;

var caml_nativeint_format = caml_format_int;

var caml_int32_format = caml_format_int;

function caml_float_of_string(prim) {
  return $$caml_float_of_string(prim);
}

var caml_int32_of_string = caml_int_of_string;

var caml_nativeint_of_string = caml_int_of_string;

exports.parse_digit              = parse_digit;
exports.caml_invalid_argument    = caml_invalid_argument;
exports.repeat                   = repeat;
exports.parse_sign_and_base      = parse_sign_and_base;
exports.caml_failwith            = caml_failwith;
exports._parse_format            = _parse_format;
exports._finish_formatting       = _finish_formatting;
exports.caml_format_float        = caml_format_float;
exports.caml_format_int          = caml_format_int;
exports.caml_nativeint_format    = caml_nativeint_format;
exports.caml_int32_format        = caml_int32_format;
exports.caml_float_of_string     = caml_float_of_string;
exports.caml_int_of_string       = caml_int_of_string;
exports.caml_int32_of_string     = caml_int32_of_string;
exports.caml_nativeint_of_string = caml_nativeint_of_string;
/*  Not a pure module */
