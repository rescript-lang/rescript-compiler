'use strict';

var List = require("./list.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_bytes = require("./caml_bytes.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var CamlinternalFormat = require("./camlinternalFormat.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var CamlinternalFormatBasics = require("./camlinternalFormatBasics.js");

function next_char(ib) {
  try {
    var c = Curry._1(ib.ic_get_next_char, undefined);
    ib.ic_current_char = c;
    ib.ic_current_char_is_valid = true;
    ib.ic_char_count = ib.ic_char_count + 1 | 0;
    if (c === /* '\n' */10) {
      ib.ic_line_count = ib.ic_line_count + 1 | 0;
    }
    return c;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "End_of_file") {
      ib.ic_current_char = /* '\000' */0;
      ib.ic_current_char_is_valid = false;
      ib.ic_eof = true;
      return /* '\000' */0;
    }
    throw exn;
  }
}

function peek_char(ib) {
  if (ib.ic_current_char_is_valid) {
    return ib.ic_current_char;
  } else {
    return next_char(ib);
  }
}

function checked_peek_char(ib) {
  var c = peek_char(ib);
  if (ib.ic_eof) {
    throw {
          RE_EXN_ID: "End_of_file",
          Error: new Error()
        };
  }
  return c;
}

function end_of_input(ib) {
  peek_char(ib);
  return ib.ic_eof;
}

function beginning_of_input(ib) {
  return ib.ic_char_count === 0;
}

function name_of_input(ib) {
  var _ic = ib.ic_input_name;
  if (typeof _ic === "number") {
    if (_ic === /* From_function */0) {
      return "unnamed function";
    } else {
      return "unnamed character string";
    }
  } else if (_ic.TAG === /* From_channel */0) {
    return "unnamed Pervasives input channel";
  } else {
    return _ic._0;
  }
}

function char_count(ib) {
  if (ib.ic_current_char_is_valid) {
    return ib.ic_char_count - 1 | 0;
  } else {
    return ib.ic_char_count;
  }
}

function token(ib) {
  var token_buffer = ib.ic_token_buffer;
  var tok = $$Buffer.contents(token_buffer);
  token_buffer.position = 0;
  ib.ic_token_count = ib.ic_token_count + 1 | 0;
  return tok;
}

function ignore_char(width, ib) {
  var width$1 = width - 1 | 0;
  ib.ic_current_char_is_valid = false;
  return width$1;
}

function store_char(width, ib, c) {
  $$Buffer.add_char(ib.ic_token_buffer, c);
  return ignore_char(width, ib);
}

function create(iname, next) {
  return {
          ic_eof: false,
          ic_current_char: /* '\000' */0,
          ic_current_char_is_valid: false,
          ic_char_count: 0,
          ic_line_count: 0,
          ic_token_count: 0,
          ic_get_next_char: next,
          ic_token_buffer: $$Buffer.create(1024),
          ic_input_name: iname
        };
}

function from_string(s) {
  var i = {
    contents: 0
  };
  var len = s.length;
  var next = function (param) {
    if (i.contents >= len) {
      throw {
            RE_EXN_ID: "End_of_file",
            Error: new Error()
          };
    }
    var c = Caml_string.get(s, i.contents);
    i.contents = i.contents + 1 | 0;
    return c;
  };
  return create(/* From_string */1, next);
}

function from_function(param) {
  return create(/* From_function */0, param);
}

var file_buffer_size = {
  contents: 1024
};

function scan_close_at_end(ic) {
  Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
  throw {
        RE_EXN_ID: "End_of_file",
        Error: new Error()
      };
}

function scan_raise_at_end(_ic) {
  throw {
        RE_EXN_ID: "End_of_file",
        Error: new Error()
      };
}

function from_ic(scan_close_ic, iname, ic) {
  var len = file_buffer_size.contents;
  var buf = Caml_bytes.caml_create_bytes(len);
  var i = {
    contents: 0
  };
  var lim = {
    contents: 0
  };
  var eof = {
    contents: false
  };
  var next = function (param) {
    if (i.contents < lim.contents) {
      var c = Caml_bytes.get(buf, i.contents);
      i.contents = i.contents + 1 | 0;
      return c;
    }
    if (eof.contents) {
      throw {
            RE_EXN_ID: "End_of_file",
            Error: new Error()
          };
    }
    lim.contents = Pervasives.input(ic, buf, 0, len);
    if (lim.contents === 0) {
      eof.contents = true;
      return Curry._1(scan_close_ic, ic);
    } else {
      i.contents = 1;
      return Caml_bytes.get(buf, 0);
    }
  };
  return create(iname, next);
}

var stdin = from_ic(scan_raise_at_end, {
      TAG: /* From_file */1,
      _0: "-",
      _1: Pervasives.stdin
    }, Pervasives.stdin);

function open_in_file(open_in, fname) {
  if (fname === "-") {
    return stdin;
  }
  var ic = Curry._1(open_in, fname);
  return from_ic(scan_close_at_end, {
              TAG: /* From_file */1,
              _0: fname,
              _1: ic
            }, ic);
}

function open_in(param) {
  return open_in_file(Pervasives.open_in, param);
}

function open_in_bin(param) {
  return open_in_file(Pervasives.open_in_bin, param);
}

function from_channel(ic) {
  return from_ic(scan_raise_at_end, {
              TAG: /* From_channel */0,
              _0: ic
            }, ic);
}

function close_in(ib) {
  var ic = ib.ic_input_name;
  if (typeof ic === "number") {
    return ;
  } else if (ic.TAG === /* From_channel */0) {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(ic._0);
  } else {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(ic._1);
  }
}

var memo = {
  contents: /* [] */0
};

function memo_from_ic(scan_close_ic, ic) {
  try {
    return List.assq(ic, memo.contents);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      var ib = from_ic(scan_close_ic, {
            TAG: /* From_channel */0,
            _0: ic
          }, ic);
      memo.contents = {
        hd: [
          ic,
          ib
        ],
        tl: memo.contents
      };
      return ib;
    }
    throw exn;
  }
}

var Scan_failure = /* @__PURE__ */Caml_exceptions.create("Scanf.Scan_failure");

function bad_input_escape(c) {
  var s = Curry._1(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "illegal escape character ",
              _1: {
                TAG: /* Caml_char */1,
                _0: /* End_of_format */0
              }
            },
            _1: "illegal escape character %C"
          }), c);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function bad_token_length(message) {
  var s = Curry._1(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "scanning of ",
              _1: {
                TAG: /* String */2,
                _0: /* No_padding */0,
                _1: {
                  TAG: /* String_literal */11,
                  _0: " failed: the specified length was too short for token",
                  _1: /* End_of_format */0
                }
              }
            },
            _1: "scanning of %s failed: the specified length was too short for token"
          }), message);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function bad_hex_float(param) {
  throw {
        RE_EXN_ID: Scan_failure,
        _1: "not a valid float in hexadecimal notation",
        Error: new Error()
      };
}

function character_mismatch_err(c, ci) {
  return Curry._2(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* String_literal */11,
                    _0: "looking for ",
                    _1: {
                      TAG: /* Caml_char */1,
                      _0: {
                        TAG: /* String_literal */11,
                        _0: ", found ",
                        _1: {
                          TAG: /* Caml_char */1,
                          _0: /* End_of_format */0
                        }
                      }
                    }
                  },
                  _1: "looking for %C, found %C"
                }), c, ci);
}

function check_this_char(ib, c) {
  var ci = checked_peek_char(ib);
  if (ci === c) {
    ib.ic_current_char_is_valid = false;
    return ;
  }
  var s = character_mismatch_err(c, ci);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function check_char(ib, c) {
  if (c !== 10) {
    if (c !== 32) {
      return check_this_char(ib, c);
    } else {
      while(true) {
        var c$1 = peek_char(ib);
        if (ib.ic_eof) {
          return ;
        }
        if (c$1 > 13 || c$1 < 9) {
          if (c$1 !== 32) {
            return ;
          }
          ib.ic_current_char_is_valid = false;
          continue ;
        }
        if (c$1 === 12 || c$1 === 11) {
          return ;
        }
        ib.ic_current_char_is_valid = false;
        continue ;
      };
    }
  } else {
    var ci = checked_peek_char(ib);
    if (ci === 10) {
      ib.ic_current_char_is_valid = false;
      return ;
    }
    if (ci !== 13) {
      var s = character_mismatch_err(/* '\n' */10, ci);
      throw {
            RE_EXN_ID: Scan_failure,
            _1: s,
            Error: new Error()
          };
    }
    ib.ic_current_char_is_valid = false;
    return check_this_char(ib, /* '\n' */10);
  }
}

function token_bool(ib) {
  var s = token(ib);
  switch (s) {
    case "false" :
        return false;
    case "true" :
        return true;
    default:
      var s$1 = Curry._1(Printf.sprintf(/* Format */{
                _0: {
                  TAG: /* String_literal */11,
                  _0: "invalid boolean '",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* '\'' */39,
                      _1: /* End_of_format */0
                    }
                  }
                },
                _1: "invalid boolean '%s'"
              }), s);
      throw {
            RE_EXN_ID: Scan_failure,
            _1: s$1,
            Error: new Error()
          };
  }
}

function integer_conversion_of_char(param) {
  switch (param) {
    case 98 :
        return /* B_conversion */0;
    case 100 :
        return /* D_conversion */1;
    case 105 :
        return /* I_conversion */2;
    case 111 :
        return /* O_conversion */3;
    case 117 :
        return /* U_conversion */4;
    case 89 :
    case 90 :
    case 91 :
    case 92 :
    case 93 :
    case 94 :
    case 95 :
    case 96 :
    case 97 :
    case 99 :
    case 101 :
    case 102 :
    case 103 :
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
        break;
    case 88 :
    case 120 :
        return /* X_conversion */5;
    default:
      
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "scanf.ml",
          555,
          9
        ],
        Error: new Error()
      };
}

function token_int_literal(conv, ib) {
  var tok;
  switch (conv) {
    case /* B_conversion */0 :
        tok = "0b" + token(ib);
        break;
    case /* D_conversion */1 :
    case /* I_conversion */2 :
        tok = token(ib);
        break;
    case /* O_conversion */3 :
        tok = "0o" + token(ib);
        break;
    case /* U_conversion */4 :
        tok = "0u" + token(ib);
        break;
    case /* X_conversion */5 :
        tok = "0x" + token(ib);
        break;
    
  }
  var l = tok.length;
  if (l === 0 || Caml_string.get(tok, 0) !== /* '+' */43) {
    return tok;
  } else {
    return $$String.sub(tok, 1, l - 1 | 0);
  }
}

function token_float(ib) {
  return Caml_format.caml_float_of_string(token(ib));
}

function scan_decimal_digit_star(_width, ib) {
  while(true) {
    var width = _width;
    if (width === 0) {
      return width;
    }
    var c = peek_char(ib);
    if (ib.ic_eof) {
      return width;
    }
    if (c >= 58) {
      if (c !== 95) {
        return width;
      }
      var width$1 = ignore_char(width, ib);
      _width = width$1;
      continue ;
    }
    if (c < 48) {
      return width;
    }
    var width$2 = store_char(width, ib, c);
    _width = width$2;
    continue ;
  };
}

function scan_decimal_digit_plus(width, ib) {
  if (width === 0) {
    return bad_token_length("decimal digits");
  }
  var c = checked_peek_char(ib);
  if (c > 57 || c < 48) {
    var s = Curry._1(Printf.sprintf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "character ",
                _1: {
                  TAG: /* Caml_char */1,
                  _0: {
                    TAG: /* String_literal */11,
                    _0: " is not a decimal digit",
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "character %C is not a decimal digit"
            }), c);
    throw {
          RE_EXN_ID: Scan_failure,
          _1: s,
          Error: new Error()
        };
  }
  var width$1 = store_char(width, ib, c);
  return scan_decimal_digit_star(width$1, ib);
}

function scan_digit_plus(basis, digitp, width, ib) {
  if (width === 0) {
    return bad_token_length("digits");
  }
  var c = checked_peek_char(ib);
  if (Curry._1(digitp, c)) {
    var width$1 = store_char(width, ib, c);
    var _width = width$1;
    while(true) {
      var width$2 = _width;
      if (width$2 === 0) {
        return width$2;
      }
      var c$1 = peek_char(ib);
      if (ib.ic_eof) {
        return width$2;
      }
      if (Curry._1(digitp, c$1)) {
        var width$3 = store_char(width$2, ib, c$1);
        _width = width$3;
        continue ;
      }
      if (c$1 !== 95) {
        return width$2;
      }
      var width$4 = ignore_char(width$2, ib);
      _width = width$4;
      continue ;
    };
  }
  var s = Curry._2(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "character ",
              _1: {
                TAG: /* Caml_char */1,
                _0: {
                  TAG: /* String_literal */11,
                  _0: " is not a valid ",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* String_literal */11,
                      _0: " digit",
                      _1: /* End_of_format */0
                    }
                  }
                }
              }
            },
            _1: "character %C is not a valid %s digit"
          }), c, basis);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function is_binary_digit(param) {
  return param === 49 || param === 48;
}

function scan_binary_int(param, param$1) {
  return scan_digit_plus("binary", is_binary_digit, param, param$1);
}

function is_octal_digit(param) {
  return !(param > 55 || param < 48);
}

function scan_octal_int(param, param$1) {
  return scan_digit_plus("octal", is_octal_digit, param, param$1);
}

function is_hexa_digit(param) {
  if (param > 70 || param < 48) {
    return !(param > 102 || param < 97);
  } else {
    return param > 64 || param < 58;
  }
}

function scan_hexadecimal_int(param, param$1) {
  return scan_digit_plus("hexadecimal", is_hexa_digit, param, param$1);
}

function scan_sign(width, ib) {
  var c = checked_peek_char(ib);
  if (c !== 43 && c !== 45) {
    return width;
  } else {
    return store_char(width, ib, c);
  }
}

function scan_optionally_signed_decimal_int(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digit_plus(width$1, ib);
}

function scan_int_conversion(conv, width, ib) {
  switch (conv) {
    case /* B_conversion */0 :
        return scan_binary_int(width, ib);
    case /* D_conversion */1 :
        return scan_optionally_signed_decimal_int(width, ib);
    case /* I_conversion */2 :
        var width$1 = scan_sign(width, ib);
        var c = checked_peek_char(ib);
        if (c !== 48) {
          return scan_decimal_digit_plus(width$1, ib);
        }
        var width$2 = store_char(width$1, ib, c);
        if (width$2 === 0) {
          return width$2;
        }
        var c$1 = peek_char(ib);
        if (ib.ic_eof) {
          return width$2;
        } else if (c$1 >= 99) {
          if (c$1 !== 111) {
            if (c$1 !== 120) {
              return scan_decimal_digit_star(width$2, ib);
            } else {
              return scan_hexadecimal_int(store_char(width$2, ib, c$1), ib);
            }
          } else {
            return scan_octal_int(store_char(width$2, ib, c$1), ib);
          }
        } else if (c$1 !== 88) {
          if (c$1 >= 98) {
            return scan_binary_int(store_char(width$2, ib, c$1), ib);
          } else {
            return scan_decimal_digit_star(width$2, ib);
          }
        } else {
          return scan_hexadecimal_int(store_char(width$2, ib, c$1), ib);
        }
    case /* O_conversion */3 :
        return scan_octal_int(width, ib);
    case /* U_conversion */4 :
        return scan_decimal_digit_plus(width, ib);
    case /* X_conversion */5 :
        return scan_hexadecimal_int(width, ib);
    
  }
}

function scan_fractional_part(width, ib) {
  if (width === 0) {
    return width;
  }
  var c = peek_char(ib);
  if (ib.ic_eof || c > 57 || c < 48) {
    return width;
  } else {
    return scan_decimal_digit_star(store_char(width, ib, c), ib);
  }
}

function scan_exponent_part(width, ib) {
  if (width === 0) {
    return width;
  }
  var c = peek_char(ib);
  if (ib.ic_eof || c !== 69 && c !== 101) {
    return width;
  } else {
    return scan_optionally_signed_decimal_int(store_char(width, ib, c), ib);
  }
}

function scan_integer_part(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digit_star(width$1, ib);
}

function scan_float(width, precision, ib) {
  var width$1 = scan_integer_part(width, ib);
  if (width$1 === 0) {
    return [
            width$1,
            precision
          ];
  }
  var c = peek_char(ib);
  if (ib.ic_eof) {
    return [
            width$1,
            precision
          ];
  }
  if (c !== 46) {
    return [
            scan_exponent_part(width$1, ib),
            precision
          ];
  }
  var width$2 = store_char(width$1, ib, c);
  var precision$1 = width$2 < precision ? width$2 : precision;
  var width$3 = width$2 - (precision$1 - scan_fractional_part(precision$1, ib) | 0) | 0;
  return [
          scan_exponent_part(width$3, ib),
          precision$1
        ];
}

function check_case_insensitive_string(width, ib, error, str) {
  var lowercase = function (c) {
    if (c > 90 || c < 65) {
      return c;
    } else {
      return Pervasives.char_of_int((c - /* 'A' */65 | 0) + /* 'a' */97 | 0);
    }
  };
  var len = str.length;
  var width$1 = width;
  for(var i = 0; i < len; ++i){
    var c = peek_char(ib);
    if (lowercase(c) !== lowercase(Caml_string.get(str, i))) {
      Curry._1(error, undefined);
    }
    if (width$1 === 0) {
      Curry._1(error, undefined);
    }
    width$1 = store_char(width$1, ib, c);
  }
  return width$1;
}

function scan_hex_float(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "not a valid float in hexadecimal notation",
          Error: new Error()
        };
  }
  var width$1 = scan_sign(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "not a valid float in hexadecimal notation",
          Error: new Error()
        };
  }
  var c = peek_char(ib);
  if (c >= 78) {
    if (c > 109 || c < 79) {
      if (c >= 111) {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "not a valid float in hexadecimal notation",
              Error: new Error()
            };
      }
      var width$2 = store_char(width$1, ib, c);
      if (width$2 === 0 || end_of_input(ib)) {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "not a valid float in hexadecimal notation",
              Error: new Error()
            };
      }
      return check_case_insensitive_string(width$2, ib, bad_hex_float, "an");
    }
    if (c !== 105) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "not a valid float in hexadecimal notation",
            Error: new Error()
          };
    }
    
  } else if (c !== 48) {
    if (c !== 73) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "not a valid float in hexadecimal notation",
            Error: new Error()
          };
    }
    
  } else {
    var width$3 = store_char(width$1, ib, c);
    if (width$3 === 0 || end_of_input(ib)) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "not a valid float in hexadecimal notation",
            Error: new Error()
          };
    }
    var width$4 = check_case_insensitive_string(width$3, ib, bad_hex_float, "x");
    if (width$4 === 0 || end_of_input(ib)) {
      return width$4;
    }
    var match = peek_char(ib);
    var width$5 = match > 80 || match < 46 ? (
        match !== 112 ? scan_hexadecimal_int(width$4, ib) : width$4
      ) : (
        match > 79 || match < 47 ? width$4 : scan_hexadecimal_int(width$4, ib)
      );
    if (width$5 === 0 || end_of_input(ib)) {
      return width$5;
    }
    var c$1 = peek_char(ib);
    var width$6;
    if (c$1 !== 46) {
      width$6 = width$5;
    } else {
      var width$7 = store_char(width$5, ib, c$1);
      if (width$7 === 0 || end_of_input(ib)) {
        width$6 = width$7;
      } else {
        var match$1 = peek_char(ib);
        if (match$1 !== 80 && match$1 !== 112) {
          var precision$1 = width$7 < precision ? width$7 : precision;
          width$6 = width$7 - (precision$1 - scan_hexadecimal_int(precision$1, ib) | 0) | 0;
        } else {
          width$6 = width$7;
        }
      }
    }
    if (width$6 === 0 || end_of_input(ib)) {
      return width$6;
    }
    var c$2 = peek_char(ib);
    var exit = 0;
    if (c$2 !== 80) {
      if (c$2 !== 112) {
        return width$6;
      }
      exit = 2;
    } else {
      exit = 2;
    }
    if (exit === 2) {
      var width$8 = store_char(width$6, ib, c$2);
      if (width$8 === 0 || end_of_input(ib)) {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "not a valid float in hexadecimal notation",
              Error: new Error()
            };
      }
      return scan_optionally_signed_decimal_int(width$8, ib);
    }
    
  }
  var width$9 = store_char(width$1, ib, c);
  if (width$9 === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "not a valid float in hexadecimal notation",
          Error: new Error()
        };
  }
  return check_case_insensitive_string(width$9, ib, bad_hex_float, "nfinity");
}

function scan_caml_float_rest(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "no dot or exponent part found in float token",
          Error: new Error()
        };
  }
  var width$1 = scan_decimal_digit_star(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "no dot or exponent part found in float token",
          Error: new Error()
        };
  }
  var c = peek_char(ib);
  if (c > 101 || c < 69) {
    if (c !== 46) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "no dot or exponent part found in float token",
            Error: new Error()
          };
    }
    var width$2 = store_char(width$1, ib, c);
    var precision$1 = width$2 < precision ? width$2 : precision;
    var width_precision = scan_fractional_part(precision$1, ib);
    var frac_width = precision$1 - width_precision | 0;
    var width$3 = width$2 - frac_width | 0;
    return scan_exponent_part(width$3, ib);
  }
  if (c > 100 || c < 70) {
    return scan_exponent_part(width$1, ib);
  }
  throw {
        RE_EXN_ID: Scan_failure,
        _1: "no dot or exponent part found in float token",
        Error: new Error()
      };
}

function scan_caml_float(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "no dot or exponent part found in float token",
          Error: new Error()
        };
  }
  var width$1 = scan_sign(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "no dot or exponent part found in float token",
          Error: new Error()
        };
  }
  var c = peek_char(ib);
  if (c >= 49) {
    if (c >= 58) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "no dot or exponent part found in float token",
            Error: new Error()
          };
    }
    var width$2 = store_char(width$1, ib, c);
    if (width$2 === 0 || end_of_input(ib)) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "no dot or exponent part found in float token",
            Error: new Error()
          };
    }
    return scan_caml_float_rest(width$2, precision, ib);
  }
  if (c >= 48) {
    var width$3 = store_char(width$1, ib, c);
    if (width$3 === 0 || end_of_input(ib)) {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: "no dot or exponent part found in float token",
            Error: new Error()
          };
    }
    var c$1 = peek_char(ib);
    var exit = 0;
    if (c$1 !== 88) {
      if (c$1 !== 120) {
        return scan_caml_float_rest(width$3, precision, ib);
      }
      exit = 1;
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var width$4 = store_char(width$3, ib, c$1);
      if (width$4 === 0 || end_of_input(ib)) {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "no dot or exponent part found in float token",
              Error: new Error()
            };
      }
      var width$5 = scan_hexadecimal_int(width$4, ib);
      if (width$5 === 0 || end_of_input(ib)) {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "no dot or exponent part found in float token",
              Error: new Error()
            };
      }
      var c$2 = peek_char(ib);
      var width$6;
      if (c$2 > 112 || c$2 < 80) {
        if (c$2 !== 46) {
          throw {
                RE_EXN_ID: Scan_failure,
                _1: "no dot or exponent part found in float token",
                Error: new Error()
              };
        }
        var width$7 = store_char(width$5, ib, c$2);
        if (width$7 === 0 || end_of_input(ib)) {
          width$6 = width$7;
        } else {
          var match = peek_char(ib);
          if (match !== 80 && match !== 112) {
            var precision$1 = width$7 < precision ? width$7 : precision;
            width$6 = width$7 - (precision$1 - scan_hexadecimal_int(precision$1, ib) | 0) | 0;
          } else {
            width$6 = width$7;
          }
        }
      } else if (c$2 > 111 || c$2 < 81) {
        width$6 = width$5;
      } else {
        throw {
              RE_EXN_ID: Scan_failure,
              _1: "no dot or exponent part found in float token",
              Error: new Error()
            };
      }
      if (width$6 === 0 || end_of_input(ib)) {
        return width$6;
      }
      var c$3 = peek_char(ib);
      var exit$1 = 0;
      if (c$3 !== 80) {
        if (c$3 !== 112) {
          return width$6;
        }
        exit$1 = 2;
      } else {
        exit$1 = 2;
      }
      if (exit$1 === 2) {
        var width$8 = store_char(width$6, ib, c$3);
        if (width$8 === 0 || end_of_input(ib)) {
          throw {
                RE_EXN_ID: Scan_failure,
                _1: "not a valid float in hexadecimal notation",
                Error: new Error()
              };
        }
        return scan_optionally_signed_decimal_int(width$8, ib);
      }
      
    }
    
  } else {
    throw {
          RE_EXN_ID: Scan_failure,
          _1: "no dot or exponent part found in float token",
          Error: new Error()
        };
  }
}

function scan_string(stp, width, ib) {
  var _width = width;
  while(true) {
    var width$1 = _width;
    if (width$1 === 0) {
      return width$1;
    }
    var c = peek_char(ib);
    if (ib.ic_eof) {
      return width$1;
    }
    if (stp !== undefined) {
      if (c === stp) {
        ib.ic_current_char_is_valid = false;
        return width$1;
      }
      _width = store_char(width$1, ib, c);
      continue ;
    }
    if (c > 13 || c < 9) {
      if (c === 32) {
        return width$1;
      }
      _width = store_char(width$1, ib, c);
      continue ;
    }
    if (!(c === 12 || c === 11)) {
      return width$1;
    }
    _width = store_char(width$1, ib, c);
    continue ;
  };
}

function scan_char(width, ib) {
  return store_char(width, ib, checked_peek_char(ib));
}

function char_for_backslash(c) {
  if (c < 110) {
    if (c !== 98) {
      return c;
    } else {
      return /* '\b' */8;
    }
  }
  if (c >= 117) {
    return c;
  }
  switch (c) {
    case 110 :
        return /* '\n' */10;
    case 114 :
        return /* '\r' */13;
    case 111 :
    case 112 :
    case 113 :
    case 115 :
        return c;
    case 116 :
        return /* '\t' */9;
    
  }
}

function char_for_decimal_code(c0, c1, c2) {
  var c = (Math.imul(100, c0 - /* '0' */48 | 0) + Math.imul(10, c1 - /* '0' */48 | 0) | 0) + (c2 - /* '0' */48 | 0) | 0;
  if (!(c < 0 || c > 255)) {
    return Pervasives.char_of_int(c);
  }
  var s = Curry._3(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "bad character decimal encoding \\",
              _1: {
                TAG: /* Char */0,
                _0: {
                  TAG: /* Char */0,
                  _0: {
                    TAG: /* Char */0,
                    _0: /* End_of_format */0
                  }
                }
              }
            },
            _1: "bad character decimal encoding \\%c%c%c"
          }), c0, c1, c2);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function hexadecimal_value_of_char(c) {
  if (c >= /* 'a' */97) {
    return c - 87 | 0;
  } else if (c >= /* 'A' */65) {
    return c - 55 | 0;
  } else {
    return c - /* '0' */48 | 0;
  }
}

function char_for_hexadecimal_code(c1, c2) {
  var c = (hexadecimal_value_of_char(c1) << 4) + hexadecimal_value_of_char(c2) | 0;
  if (!(c < 0 || c > 255)) {
    return Pervasives.char_of_int(c);
  }
  var s = Curry._2(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "bad character hexadecimal encoding \\",
              _1: {
                TAG: /* Char */0,
                _0: {
                  TAG: /* Char */0,
                  _0: /* End_of_format */0
                }
              }
            },
            _1: "bad character hexadecimal encoding \\%c%c"
          }), c1, c2);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function check_next_char(message, width, ib) {
  if (width === 0) {
    return bad_token_length(message);
  }
  var c = peek_char(ib);
  if (ib.ic_eof) {
    var s = Curry._1(Printf.sprintf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "scanning of ",
                _1: {
                  TAG: /* String */2,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: " failed: premature end of file occurred before end of token",
                    _1: /* End_of_format */0
                  }
                }
              },
              _1: "scanning of %s failed: premature end of file occurred before end of token"
            }), message);
    throw {
          RE_EXN_ID: Scan_failure,
          _1: s,
          Error: new Error()
        };
  } else {
    return c;
  }
}

function scan_backslash_char(width, ib) {
  var c = check_next_char("a Char", width, ib);
  if (c >= 40) {
    if (c >= 58) {
      switch (c) {
        case 92 :
        case 98 :
        case 110 :
        case 114 :
        case 116 :
            break;
        case 93 :
        case 94 :
        case 95 :
        case 96 :
        case 97 :
        case 99 :
        case 100 :
        case 101 :
        case 102 :
        case 103 :
        case 104 :
        case 105 :
        case 106 :
        case 107 :
        case 108 :
        case 109 :
        case 111 :
        case 112 :
        case 113 :
        case 115 :
        case 117 :
        case 118 :
        case 119 :
            return bad_input_escape(c);
        case 120 :
            var get_digit = function (param) {
              var c = next_char(ib);
              if (c > 70 || c < 48) {
                if (c > 102 || c < 97) {
                  return bad_input_escape(c);
                } else {
                  return c;
                }
              } else if (c > 64 || c < 58) {
                return c;
              } else {
                return bad_input_escape(c);
              }
            };
            var c1 = get_digit(undefined);
            var c2 = get_digit(undefined);
            return store_char(width - 2 | 0, ib, char_for_hexadecimal_code(c1, c2));
        default:
          return bad_input_escape(c);
      }
    } else {
      if (c < 48) {
        return bad_input_escape(c);
      }
      var get_digit$1 = function (param) {
        var c = next_char(ib);
        if (c > 57 || c < 48) {
          return bad_input_escape(c);
        } else {
          return c;
        }
      };
      var c1$1 = get_digit$1(undefined);
      var c2$1 = get_digit$1(undefined);
      return store_char(width - 2 | 0, ib, char_for_decimal_code(c, c1$1, c2$1));
    }
  } else if (c !== 34 && c < 39) {
    return bad_input_escape(c);
  }
  return store_char(width, ib, char_for_backslash(c));
}

function scan_caml_char(width, ib) {
  var find_stop = function (width) {
    var c = check_next_char("a Char", width, ib);
    if (c === 39) {
      return ignore_char(width, ib);
    }
    var s = character_mismatch_err(/* '\'' */39, c);
    throw {
          RE_EXN_ID: Scan_failure,
          _1: s,
          Error: new Error()
        };
  };
  var c = checked_peek_char(ib);
  if (c === 39) {
    var width$1 = ignore_char(width, ib);
    var c$1 = check_next_char("a Char", width$1, ib);
    if (c$1 !== 92) {
      return find_stop(store_char(width$1, ib, c$1));
    } else {
      return find_stop(scan_backslash_char(ignore_char(width$1, ib), ib));
    }
  }
  var s = character_mismatch_err(/* '\'' */39, c);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function scan_caml_string(width, ib) {
  var find_stop = function (_width) {
    while(true) {
      var width = _width;
      var c = check_next_char("a String", width, ib);
      if (c === 34) {
        return ignore_char(width, ib);
      }
      if (c === 92) {
        var width$1 = ignore_char(width, ib);
        var match = check_next_char("a String", width$1, ib);
        if (match !== 10) {
          if (match !== 13) {
            return find_stop(scan_backslash_char(width$1, ib));
          } else {
            var width$2 = ignore_char(width$1, ib);
            var match$1 = check_next_char("a String", width$2, ib);
            if (match$1 !== 10) {
              return find_stop(store_char(width$2, ib, /* '\r' */13));
            } else {
              return skip_spaces(ignore_char(width$2, ib));
            }
          }
        } else {
          return skip_spaces(ignore_char(width$1, ib));
        }
      }
      _width = store_char(width, ib, c);
      continue ;
    };
  };
  var skip_spaces = function (_width) {
    while(true) {
      var width = _width;
      var match = check_next_char("a String", width, ib);
      if (match !== 32) {
        return find_stop(width);
      }
      _width = ignore_char(width, ib);
      continue ;
    };
  };
  var c = checked_peek_char(ib);
  if (c === 34) {
    return find_stop(ignore_char(width, ib));
  }
  var s = character_mismatch_err(/* '"' */34, c);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function scan_chars_in_char_set(char_set, scan_indic, width, ib) {
  var scan_chars = function (_i, stp) {
    while(true) {
      var i = _i;
      var c = peek_char(ib);
      if (!(i > 0 && !ib.ic_eof && CamlinternalFormat.is_in_char_set(char_set, c) && c !== stp)) {
        return ;
      }
      store_char(Pervasives.max_int, ib, c);
      _i = i - 1 | 0;
      continue ;
    };
  };
  if (scan_indic === undefined) {
    return scan_chars(width, -1);
  }
  scan_chars(width, scan_indic);
  if (ib.ic_eof) {
    return ;
  }
  var ci = peek_char(ib);
  if (scan_indic === ci) {
    ib.ic_current_char_is_valid = false;
    return ;
  }
  var s = character_mismatch_err(scan_indic, ci);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s,
        Error: new Error()
      };
}

function scanf_bad_input(ib, x) {
  var s;
  if (x.RE_EXN_ID === Scan_failure || x.RE_EXN_ID === "Failure") {
    s = x._1;
  } else {
    throw x;
  }
  var i = char_count(ib);
  var s$1 = Curry._2(Printf.sprintf(/* Format */{
            _0: {
              TAG: /* String_literal */11,
              _0: "scanf: bad input at char number ",
              _1: {
                TAG: /* Int */4,
                _0: /* Int_i */3,
                _1: /* No_padding */0,
                _2: /* No_precision */0,
                _3: {
                  TAG: /* String_literal */11,
                  _0: ": ",
                  _1: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  }
                }
              }
            },
            _1: "scanf: bad input at char number %i: %s"
          }), i, s);
  throw {
        RE_EXN_ID: Scan_failure,
        _1: s$1,
        Error: new Error()
      };
}

function get_counter(ib, counter) {
  switch (counter) {
    case /* Line_counter */0 :
        return ib.ic_line_count;
    case /* Char_counter */1 :
        return char_count(ib);
    case /* Token_counter */2 :
        return ib.ic_token_count;
    
  }
}

function stopper_of_formatting_lit(fmting) {
  if (fmting === /* Escaped_percent */6) {
    return [
            /* '%' */37,
            ""
          ];
  }
  var str = CamlinternalFormat.string_of_formatting_lit(fmting);
  var stp = Caml_string.get(str, 1);
  var sub_str = $$String.sub(str, 2, str.length - 2 | 0);
  return [
          stp,
          sub_str
        ];
}

function take_format_readers(k, _fmt) {
  while(true) {
    var fmt = _fmt;
    if (typeof fmt === "number") {
      return Curry._1(k, /* Nil */0);
    }
    switch (fmt.TAG | 0) {
      case /* Int */4 :
      case /* Int32 */5 :
      case /* Nativeint */6 :
      case /* Int64 */7 :
      case /* Float */8 :
          _fmt = fmt._3;
          continue ;
      case /* Format_subst */14 :
          return take_fmtty_format_readers(k, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmt._1)), fmt._2);
      case /* Formatting_gen */18 :
          _fmt = CamlinternalFormatBasics.concat_fmt(fmt._0._0._0, fmt._1);
          continue ;
      case /* Reader */19 :
          var fmt_rest = fmt._0;
          return (function(fmt_rest){
          return function (reader) {
            var new_k = function (readers_rest) {
              return Curry._1(k, /* Cons */{
                          _0: reader,
                          _1: readers_rest
                        });
            };
            return take_format_readers(new_k, fmt_rest);
          }
          }(fmt_rest));
      case /* Char */0 :
      case /* Caml_char */1 :
      case /* Flush */10 :
      case /* Alpha */15 :
      case /* Theta */16 :
      case /* Scan_next_char */22 :
          _fmt = fmt._0;
          continue ;
      case /* Ignored_param */23 :
          var ign = fmt._0;
          var fmt$1 = fmt._1;
          if (typeof ign === "number") {
            if (ign === /* Ignored_reader */2) {
              return (function(fmt$1){
              return function (reader) {
                var new_k = function (readers_rest) {
                  return Curry._1(k, /* Cons */{
                              _0: reader,
                              _1: readers_rest
                            });
                };
                return take_format_readers(new_k, fmt$1);
              }
              }(fmt$1));
            } else {
              return take_format_readers(k, fmt$1);
            }
          } else if (ign.TAG === /* Ignored_format_subst */9) {
            return take_fmtty_format_readers(k, ign._1, fmt$1);
          } else {
            return take_format_readers(k, fmt$1);
          }
      case /* Format_arg */13 :
      case /* Scan_char_set */20 :
      case /* Custom */24 :
          _fmt = fmt._2;
          continue ;
      default:
        _fmt = fmt._1;
        continue ;
    }
  };
}

function take_fmtty_format_readers(k, _fmtty, fmt) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "number") {
      return take_format_readers(k, fmt);
    }
    switch (fmtty.TAG | 0) {
      case /* Format_arg_ty */8 :
          _fmtty = fmtty._1;
          continue ;
      case /* Format_subst_ty */9 :
          var ty = CamlinternalFormat.trans(CamlinternalFormat.symm(fmtty._0), fmtty._1);
          _fmtty = CamlinternalFormatBasics.concat_fmtty(ty, fmtty._2);
          continue ;
      case /* Reader_ty */13 :
          var fmt_rest = fmtty._0;
          return (function(fmt_rest){
          return function (reader) {
            var new_k = function (readers_rest) {
              return Curry._1(k, /* Cons */{
                          _0: reader,
                          _1: readers_rest
                        });
            };
            return take_fmtty_format_readers(new_k, fmt_rest, fmt);
          }
          }(fmt_rest));
      case /* Ignored_reader_ty */14 :
          var fmt_rest$1 = fmtty._0;
          return (function(fmt_rest$1){
          return function (reader) {
            var new_k = function (readers_rest) {
              return Curry._1(k, /* Cons */{
                          _0: reader,
                          _1: readers_rest
                        });
            };
            return take_fmtty_format_readers(new_k, fmt_rest$1, fmt);
          }
          }(fmt_rest$1));
      default:
        _fmtty = fmtty._0;
        continue ;
    }
  };
}

function make_scanf(ib, _fmt, readers) {
  while(true) {
    var fmt = _fmt;
    if (typeof fmt === "number") {
      return /* Nil */0;
    }
    switch (fmt.TAG | 0) {
      case /* Char */0 :
          scan_char(0, ib);
          var c = Caml_string.get(token(ib), 0);
          return /* Cons */{
                  _0: c,
                  _1: make_scanf(ib, fmt._0, readers)
                };
      case /* Caml_char */1 :
          scan_caml_char(0, ib);
          var c$1 = Caml_string.get(token(ib), 0);
          return /* Cons */{
                  _0: c$1,
                  _1: make_scanf(ib, fmt._0, readers)
                };
      case /* String */2 :
          var rest = fmt._1;
          var pad = fmt._0;
          if (typeof rest !== "number") {
            switch (rest.TAG | 0) {
              case /* Formatting_lit */17 :
                  var match = stopper_of_formatting_lit(rest._0);
                  var stp = match[0];
                  var scan = (function(stp){
                  return function scan(width, param, ib) {
                    return scan_string(stp, width, ib);
                  }
                  }(stp));
                  var str_rest_0 = match[1];
                  var str_rest_1 = rest._1;
                  var str_rest = {
                    TAG: /* String_literal */11,
                    _0: str_rest_0,
                    _1: str_rest_1
                  };
                  return pad_prec_scanf(ib, str_rest, readers, pad, /* No_precision */0, scan, token);
              case /* Formatting_gen */18 :
                  var match$1 = rest._0;
                  if (match$1.TAG === /* Open_tag */0) {
                    var scan$1 = function (width, param, ib) {
                      return scan_string(/* '{' */123, width, ib);
                    };
                    return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1._0._0, rest._1), readers, pad, /* No_precision */0, scan$1, token);
                  }
                  var scan$2 = function (width, param, ib) {
                    return scan_string(/* '[' */91, width, ib);
                  };
                  return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1._0._0, rest._1), readers, pad, /* No_precision */0, scan$2, token);
              default:
                
            }
          }
          var scan$3 = function (width, param, ib) {
            return scan_string(undefined, width, ib);
          };
          return pad_prec_scanf(ib, rest, readers, pad, /* No_precision */0, scan$3, token);
      case /* Caml_string */3 :
          var scan$4 = function (width, param, ib) {
            return scan_caml_string(width, ib);
          };
          return pad_prec_scanf(ib, fmt._1, readers, fmt._0, /* No_precision */0, scan$4, token);
      case /* Int */4 :
          var c$2 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt._0));
          var scan$5 = (function(c$2){
          return function scan$5(width, param, ib) {
            return scan_int_conversion(c$2, width, ib);
          }
          }(c$2));
          return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan$5, (function(c$2){
                    return function (param) {
                      return Caml_format.caml_int_of_string(token_int_literal(c$2, param));
                    }
                    }(c$2)));
      case /* Int32 */5 :
          var c$3 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt._0));
          var scan$6 = (function(c$3){
          return function scan$6(width, param, ib) {
            return scan_int_conversion(c$3, width, ib);
          }
          }(c$3));
          return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan$6, (function(c$3){
                    return function (param) {
                      return Caml_format.caml_int32_of_string(token_int_literal(c$3, param));
                    }
                    }(c$3)));
      case /* Nativeint */6 :
          var c$4 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt._0));
          var scan$7 = (function(c$4){
          return function scan$7(width, param, ib) {
            return scan_int_conversion(c$4, width, ib);
          }
          }(c$4));
          return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan$7, (function(c$4){
                    return function (param) {
                      return Caml_format.caml_nativeint_of_string(token_int_literal(c$4, param));
                    }
                    }(c$4)));
      case /* Int64 */7 :
          var c$5 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt._0));
          var scan$8 = (function(c$5){
          return function scan$8(width, param, ib) {
            return scan_int_conversion(c$5, width, ib);
          }
          }(c$5));
          return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan$8, (function(c$5){
                    return function (param) {
                      return Caml_format.caml_int64_of_string(token_int_literal(c$5, param));
                    }
                    }(c$5)));
      case /* Float */8 :
          var match$2 = fmt._0;
          if (match$2 !== 15) {
            if (match$2 >= 16) {
              return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan_hex_float, token_float);
            } else {
              return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan_float, token_float);
            }
          } else {
            return pad_prec_scanf(ib, fmt._3, readers, fmt._1, fmt._2, scan_caml_float, token_float);
          }
      case /* Bool */9 :
          var scan$9 = function (param, param$1, ib) {
            var c = checked_peek_char(ib);
            var m;
            if (c !== 102) {
              if (c !== 116) {
                var s = Curry._1(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "the character ",
                            _1: {
                              TAG: /* Caml_char */1,
                              _0: {
                                TAG: /* String_literal */11,
                                _0: " cannot start a boolean",
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "the character %C cannot start a boolean"
                        }), c);
                throw {
                      RE_EXN_ID: Scan_failure,
                      _1: s,
                      Error: new Error()
                    };
              }
              m = 4;
            } else {
              m = 5;
            }
            return scan_string(undefined, m, ib);
          };
          return pad_prec_scanf(ib, fmt._1, readers, fmt._0, /* No_precision */0, scan$9, token_bool);
      case /* Flush */10 :
          if (end_of_input(ib)) {
            _fmt = fmt._0;
            continue ;
          }
          throw {
                RE_EXN_ID: Scan_failure,
                _1: "end of input not found",
                Error: new Error()
              };
      case /* String_literal */11 :
          $$String.iter((function (param) {
                  return check_char(ib, param);
                }), fmt._0);
          _fmt = fmt._1;
          continue ;
      case /* Char_literal */12 :
          check_char(ib, fmt._0);
          _fmt = fmt._1;
          continue ;
      case /* Format_arg */13 :
          var pad_opt = fmt._0;
          scan_caml_string(pad_opt !== undefined ? pad_opt : Pervasives.max_int, ib);
          var s = token(ib);
          var fmt$1;
          try {
            fmt$1 = CamlinternalFormat.format_of_string_fmtty(s, fmt._1);
          }
          catch (raw_msg){
            var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
            if (msg.RE_EXN_ID === "Failure") {
              throw {
                    RE_EXN_ID: Scan_failure,
                    _1: msg._1,
                    Error: new Error()
                  };
            }
            throw msg;
          }
          return /* Cons */{
                  _0: fmt$1,
                  _1: make_scanf(ib, fmt._2, readers)
                };
      case /* Format_subst */14 :
          var fmtty = fmt._1;
          var pad_opt$1 = fmt._0;
          scan_caml_string(pad_opt$1 !== undefined ? pad_opt$1 : Pervasives.max_int, ib);
          var s$1 = token(ib);
          var match$3;
          try {
            var fmt$2 = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
            var fmt$p = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
            match$3 = [
              CamlinternalFormat.type_format(fmt$2._0, CamlinternalFormatBasics.erase_rel(fmtty)),
              CamlinternalFormat.type_format(fmt$p._0, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmtty)))
            ];
          }
          catch (raw_msg$1){
            var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg$1);
            if (msg$1.RE_EXN_ID === "Failure") {
              throw {
                    RE_EXN_ID: Scan_failure,
                    _1: msg$1._1,
                    Error: new Error()
                  };
            }
            throw msg$1;
          }
          return /* Cons */{
                  _0: /* Format */{
                    _0: match$3[0],
                    _1: s$1
                  },
                  _1: make_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$3[1], fmt._2), readers)
                };
      case /* Alpha */15 :
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "scanf: bad conversion \"%a\"",
                Error: new Error()
              };
      case /* Theta */16 :
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "scanf: bad conversion \"%t\"",
                Error: new Error()
              };
      case /* Formatting_lit */17 :
          $$String.iter((function (param) {
                  return check_char(ib, param);
                }), CamlinternalFormat.string_of_formatting_lit(fmt._0));
          _fmt = fmt._1;
          continue ;
      case /* Formatting_gen */18 :
          var match$4 = fmt._0;
          if (match$4.TAG === /* Open_tag */0) {
            check_char(ib, /* '@' */64);
            check_char(ib, /* '{' */123);
            _fmt = CamlinternalFormatBasics.concat_fmt(match$4._0._0, fmt._1);
            continue ;
          }
          check_char(ib, /* '@' */64);
          check_char(ib, /* '[' */91);
          _fmt = CamlinternalFormatBasics.concat_fmt(match$4._0._0, fmt._1);
          continue ;
      case /* Reader */19 :
          if (readers) {
            var x = Curry._1(readers._0, ib);
            return /* Cons */{
                    _0: x,
                    _1: make_scanf(ib, fmt._0, readers._1)
                  };
          }
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "scanf: missing reader",
                Error: new Error()
              };
      case /* Scan_char_set */20 :
          var rest$1 = fmt._2;
          var char_set = fmt._1;
          var width_opt = fmt._0;
          if (typeof rest$1 !== "number" && rest$1.TAG === /* Formatting_lit */17) {
            var match$5 = stopper_of_formatting_lit(rest$1._0);
            var width = width_opt !== undefined ? width_opt : Pervasives.max_int;
            scan_chars_in_char_set(char_set, match$5[0], width, ib);
            var s$2 = token(ib);
            var str_rest_0$1 = match$5[1];
            var str_rest_1$1 = rest$1._1;
            var str_rest$1 = {
              TAG: /* String_literal */11,
              _0: str_rest_0$1,
              _1: str_rest_1$1
            };
            return /* Cons */{
                    _0: s$2,
                    _1: make_scanf(ib, str_rest$1, readers)
                  };
          }
          var width$1 = width_opt !== undefined ? width_opt : Pervasives.max_int;
          scan_chars_in_char_set(char_set, undefined, width$1, ib);
          var s$3 = token(ib);
          return /* Cons */{
                  _0: s$3,
                  _1: make_scanf(ib, rest$1, readers)
                };
      case /* Scan_get_counter */21 :
          var count = get_counter(ib, fmt._0);
          return /* Cons */{
                  _0: count,
                  _1: make_scanf(ib, fmt._1, readers)
                };
      case /* Scan_next_char */22 :
          var c$6 = checked_peek_char(ib);
          return /* Cons */{
                  _0: c$6,
                  _1: make_scanf(ib, fmt._0, readers)
                };
      case /* Ignored_param */23 :
          var fmt$p$1 = CamlinternalFormat.param_format_of_ignored_format(fmt._0, fmt._1);
          var match$6 = make_scanf(ib, fmt$p$1._0, readers);
          if (match$6) {
            return match$6._1;
          }
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "scanf.ml",
                  1455,
                  13
                ],
                Error: new Error()
              };
      case /* Custom */24 :
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "scanf: bad conversion \"%?\" (custom converter)",
                Error: new Error()
              };
      
    }
  };
}

function pad_prec_scanf(ib, fmt, readers, pad, prec, scan, token) {
  if (typeof pad === "number") {
    if (typeof prec === "number") {
      if (prec !== 0) {
        throw {
              RE_EXN_ID: "Invalid_argument",
              _1: "scanf: bad conversion \"%*\"",
              Error: new Error()
            };
      }
      Curry._3(scan, Pervasives.max_int, Pervasives.max_int, ib);
      var x = Curry._1(token, ib);
      return /* Cons */{
              _0: x,
              _1: make_scanf(ib, fmt, readers)
            };
    }
    Curry._3(scan, Pervasives.max_int, prec._0, ib);
    var x$1 = Curry._1(token, ib);
    return /* Cons */{
            _0: x$1,
            _1: make_scanf(ib, fmt, readers)
          };
  }
  if (pad.TAG === /* Lit_padding */0) {
    if (pad._0 !== 0) {
      var w = pad._1;
      if (typeof prec === "number") {
        if (prec !== 0) {
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: "scanf: bad conversion \"%*\"",
                Error: new Error()
              };
        }
        Curry._3(scan, w, Pervasives.max_int, ib);
        var x$2 = Curry._1(token, ib);
        return /* Cons */{
                _0: x$2,
                _1: make_scanf(ib, fmt, readers)
              };
      }
      Curry._3(scan, w, prec._0, ib);
      var x$3 = Curry._1(token, ib);
      return /* Cons */{
              _0: x$3,
              _1: make_scanf(ib, fmt, readers)
            };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "scanf: bad conversion \"%-\"",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "scanf: bad conversion \"%*\"",
        Error: new Error()
      };
}

function kscanf(ib, ef, param) {
  var str = param._1;
  var fmt = param._0;
  var k = function (readers, f) {
    $$Buffer.reset(ib.ic_token_buffer);
    var args;
    try {
      args = {
        TAG: /* Args */0,
        _0: make_scanf(ib, fmt, readers)
      };
    }
    catch (raw_exc){
      var exc = Caml_js_exceptions.internalToOCamlException(raw_exc);
      if (exc.RE_EXN_ID === Scan_failure || exc.RE_EXN_ID === "Failure" || exc.RE_EXN_ID === "End_of_file") {
        args = {
          TAG: /* Exc */1,
          _0: exc
        };
      } else {
        if (exc.RE_EXN_ID === "Invalid_argument") {
          var s = exc._1 + (" in format \"" + ($$String.escaped(str) + "\""));
          throw {
                RE_EXN_ID: "Invalid_argument",
                _1: s,
                Error: new Error()
              };
        }
        throw exc;
      }
    }
    if (args.TAG === /* Args */0) {
      var _f = f;
      var _args = args._0;
      while(true) {
        var args$1 = _args;
        var f$1 = _f;
        if (!args$1) {
          return f$1;
        }
        _args = args$1._1;
        _f = Curry._1(f$1, args$1._0);
        continue ;
      };
    } else {
      return Curry._2(ef, ib, args._0);
    }
  };
  return take_format_readers(k, fmt);
}

function bscanf(ib, fmt) {
  return kscanf(ib, scanf_bad_input, fmt);
}

function ksscanf(s, ef, fmt) {
  return kscanf(from_string(s), ef, fmt);
}

function sscanf(s, fmt) {
  return kscanf(from_string(s), scanf_bad_input, fmt);
}

function scanf(fmt) {
  return kscanf(stdin, scanf_bad_input, fmt);
}

function bscanf_format(ib, format, f) {
  scan_caml_string(Pervasives.max_int, ib);
  var str = token(ib);
  var tmp;
  try {
    tmp = CamlinternalFormat.format_of_string_format(str, format);
  }
  catch (raw_msg){
    var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
    if (msg.RE_EXN_ID === "Failure") {
      throw {
            RE_EXN_ID: Scan_failure,
            _1: msg._1,
            Error: new Error()
          };
    }
    throw msg;
  }
  return Curry._1(f, tmp);
}

function sscanf_format(s, format, f) {
  return bscanf_format(from_string(s), format, f);
}

function string_to_String(s) {
  var l = s.length;
  var b = $$Buffer.create(l + 2 | 0);
  $$Buffer.add_char(b, /* '"' */34);
  for(var i = 0; i < l; ++i){
    var c = Caml_string.get(s, i);
    if (c === /* '"' */34) {
      $$Buffer.add_char(b, /* '\\' */92);
    }
    $$Buffer.add_char(b, c);
  }
  $$Buffer.add_char(b, /* '"' */34);
  return $$Buffer.contents(b);
}

function format_from_string(s, fmt) {
  return sscanf_format(string_to_String(s), fmt, (function (x) {
                return x;
              }));
}

function unescaped(s) {
  return Curry._1(sscanf("\"" + (s + "\""), /* Format */{
                  _0: {
                    TAG: /* Caml_string */3,
                    _0: /* No_padding */0,
                    _1: {
                      TAG: /* Flush */10,
                      _0: /* End_of_format */0
                    }
                  },
                  _1: "%S%!"
                }), (function (x) {
                return x;
              }));
}

function kfscanf(ic, ef, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), ef, fmt);
}

function fscanf(ic, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), scanf_bad_input, fmt);
}

var Scanning = {
  stdin: stdin,
  open_in: open_in,
  open_in_bin: open_in_bin,
  close_in: close_in,
  from_file: open_in,
  from_file_bin: open_in_bin,
  from_string: from_string,
  from_function: from_function,
  from_channel: from_channel,
  end_of_input: end_of_input,
  beginning_of_input: beginning_of_input,
  name_of_input: name_of_input,
  stdib: stdin
};

exports.Scanning = Scanning;
exports.Scan_failure = Scan_failure;
exports.bscanf = bscanf;
exports.sscanf = sscanf;
exports.scanf = scanf;
exports.kscanf = kscanf;
exports.ksscanf = ksscanf;
exports.bscanf_format = bscanf_format;
exports.sscanf_format = sscanf_format;
exports.format_from_string = format_from_string;
exports.unescaped = unescaped;
exports.fscanf = fscanf;
exports.kfscanf = kfscanf;
/* stdin Not a pure module */
