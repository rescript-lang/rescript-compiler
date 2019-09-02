'use strict';

var List = require("./list.js");
var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var $$Buffer = require("./buffer.js");
var Printf = require("./printf.js");
var $$String = require("./string.js");
var Caml_bytes = require("./caml_bytes.js");
var Caml_int32 = require("./caml_int32.js");
var Pervasives = require("./pervasives.js");
var Caml_format = require("./caml_format.js");
var Caml_string = require("./caml_string.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");
var CamlinternalFormat = require("./camlinternalFormat.js");
var Caml_external_polyfill = require("./caml_external_polyfill.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");
var CamlinternalFormatBasics = require("./camlinternalFormatBasics.js");

function next_char(ib) {
  try {
    var c = Curry._1(ib[/* get_next_char */6], /* () */0);
    ib[/* current_char */1] = c;
    ib[/* current_char_is_valid */2] = true;
    ib[/* char_count */3] = ib[/* char_count */3] + 1 | 0;
    if (c === /* "\n" */10) {
      ib[/* line_count */4] = ib[/* line_count */4] + 1 | 0;
    }
    return c;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.end_of_file) {
      ib[/* current_char */1] = /* "\000" */0;
      ib[/* current_char_is_valid */2] = false;
      ib[/* eof */0] = true;
      return /* "\000" */0;
    } else {
      throw exn;
    }
  }
}

function peek_char(ib) {
  if (ib[/* current_char_is_valid */2]) {
    return ib[/* current_char */1];
  } else {
    return next_char(ib);
  }
}

function checked_peek_char(ib) {
  var c = peek_char(ib);
  if (ib[/* eof */0]) {
    throw Caml_builtin_exceptions.end_of_file;
  }
  return c;
}

function end_of_input(ib) {
  peek_char(ib);
  return ib[/* eof */0];
}

function beginning_of_input(ib) {
  return ib[/* char_count */3] === 0;
}

function name_of_input(ib) {
  var match = ib[/* input_name */8];
  if (typeof match === "string") {
    if (match === "From_string") {
      return "unnamed character string";
    } else {
      return "unnamed function";
    }
  } else if (/* XXX */match.tag === "From_file") {
    return match.Arg0;
  } else {
    return "unnamed pervasives input channel";
  }
}

function char_count(ib) {
  if (ib[/* current_char_is_valid */2]) {
    return ib[/* char_count */3] - 1 | 0;
  } else {
    return ib[/* char_count */3];
  }
}

function token(ib) {
  var tokbuf = ib[/* tokbuf */7];
  var tok = $$Buffer.contents(tokbuf);
  tokbuf[/* position */1] = 0;
  ib[/* token_count */5] = ib[/* token_count */5] + 1 | 0;
  return tok;
}

function ignore_char(width, ib) {
  var width$1 = width - 1 | 0;
  ib[/* current_char_is_valid */2] = false;
  return width$1;
}

function store_char(width, ib, c) {
  $$Buffer.add_char(ib[/* tokbuf */7], c);
  return ignore_char(width, ib);
}

function create(iname, next) {
  return /* record */[
          /* eof */false,
          /* current_char : "\000" */0,
          /* current_char_is_valid */false,
          /* char_count */0,
          /* line_count */0,
          /* token_count */0,
          /* get_next_char */next,
          /* tokbuf */$$Buffer.create(1024),
          /* input_name */iname
        ];
}

function from_string(s) {
  var i = /* record */[/* contents */0];
  var len = s.length;
  var next = function (param) {
    if (i[0] >= len) {
      throw Caml_builtin_exceptions.end_of_file;
    }
    var c = Caml_string.get(s, i[0]);
    i[0] = i[0] + 1 | 0;
    return c;
  };
  return create("From_string", next);
}

function from_function(param) {
  return create("From_function", param);
}

var file_buffer_size = /* record */[/* contents */1024];

function scan_close_at_end(ic) {
  Caml_external_polyfill.resolve("caml_ml_close_channel")(ic);
  throw Caml_builtin_exceptions.end_of_file;
}

function scan_raise_at_end(_ic) {
  throw Caml_builtin_exceptions.end_of_file;
}

function from_ic(scan_close_ic, iname, ic) {
  var len = file_buffer_size[0];
  var buf = Caml_bytes.caml_create_bytes(len);
  var i = /* record */[/* contents */0];
  var lim = /* record */[/* contents */0];
  var eof = /* record */[/* contents */false];
  var next = function (param) {
    if (i[0] < lim[0]) {
      var c = Caml_bytes.get(buf, i[0]);
      i[0] = i[0] + 1 | 0;
      return c;
    } else {
      if (eof[0]) {
        throw Caml_builtin_exceptions.end_of_file;
      }
      lim[0] = Pervasives.input(ic, buf, 0, len);
      if (lim[0] === 0) {
        eof[0] = true;
        return Curry._1(scan_close_ic, ic);
      } else {
        i[0] = 1;
        return Caml_bytes.get(buf, 0);
      }
    }
  };
  return create(iname, next);
}

var stdin = from_ic(scan_raise_at_end, /* constructor */{
      tag: "From_file",
      Arg0: "-",
      Arg1: Pervasives.stdin
    }, Pervasives.stdin);

function open_in(fname) {
  if (fname === "-") {
    return stdin;
  } else {
    var ic = Pervasives.open_in(fname);
    return from_ic(scan_close_at_end, /* constructor */{
                tag: "From_file",
                Arg0: fname,
                Arg1: ic
              }, ic);
  }
}

function open_in_bin(fname) {
  if (fname === "-") {
    return stdin;
  } else {
    var ic = Pervasives.open_in_bin(fname);
    return from_ic(scan_close_at_end, /* constructor */{
                tag: "From_file",
                Arg0: fname,
                Arg1: ic
              }, ic);
  }
}

var memo = /* record */[/* contents */"[]"];

function memo_from_ic(scan_close_ic, ic) {
  try {
    return List.assq(ic, memo[0]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var ib = from_ic(scan_close_ic, /* constructor */{
            tag: "From_channel",
            Arg0: ic
          }, ic);
      memo[0] = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          ic,
          ib
        ],
        Arg1: memo[0]
      };
      return ib;
    } else {
      throw exn;
    }
  }
}

function from_channel(param) {
  return memo_from_ic(scan_raise_at_end, param);
}

function close_in(ib) {
  var match = ib[/* input_name */8];
  if (typeof match === "string") {
    return /* () */0;
  } else if (/* XXX */match.tag === "From_file") {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(match.Arg1);
  } else {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(match.Arg0);
  }
}

var Scan_failure = Caml_exceptions.create("Scanf.Scan_failure");

function bad_input_escape(c) {
  var s = Curry._1(Printf.sprintf(/* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "illegal escape character ",
              Arg1: /* constructor */{
                tag: "Caml_char",
                Arg0: "End_of_format"
              }
            },
            Arg1: "illegal escape character %C"
          }), c);
  throw [
        Scan_failure,
        s
      ];
}

function bad_token_length(message) {
  var s = Curry._1(Printf.sprintf(/* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "scanning of ",
              Arg1: /* constructor */{
                tag: "String",
                Arg0: "No_padding",
                Arg1: /* constructor */{
                  tag: "String_literal",
                  Arg0: " failed: the specified length was too short for token",
                  Arg1: "End_of_format"
                }
              }
            },
            Arg1: "scanning of %s failed: the specified length was too short for token"
          }), message);
  throw [
        Scan_failure,
        s
      ];
}

function character_mismatch_err(c, ci) {
  return Curry._2(Printf.sprintf(/* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "String_literal",
                    Arg0: "looking for ",
                    Arg1: /* constructor */{
                      tag: "Caml_char",
                      Arg0: /* constructor */{
                        tag: "String_literal",
                        Arg0: ", found ",
                        Arg1: /* constructor */{
                          tag: "Caml_char",
                          Arg0: "End_of_format"
                        }
                      }
                    }
                  },
                  Arg1: "looking for %C, found %C"
                }), c, ci);
}

function check_char(ib, _c) {
  while(true) {
    var c = _c;
    if (c === /* " " */32) {
      var ib$1 = ib;
      while(true) {
        var c$1 = peek_char(ib$1);
        if (ib$1[/* eof */0]) {
          return 0;
        } else {
          var switcher = c$1 - 9 | 0;
          if (switcher > 4 || switcher < 0) {
            if (switcher !== 23) {
              return /* () */0;
            } else {
              ib$1[/* current_char_is_valid */2] = false;
              continue ;
            }
          } else if (switcher === 3 || switcher === 2) {
            return /* () */0;
          } else {
            ib$1[/* current_char_is_valid */2] = false;
            continue ;
          }
        }
      };
    } else {
      var ci = checked_peek_char(ib);
      if (ci === c) {
        ib[/* current_char_is_valid */2] = false;
        return /* () */0;
      } else if (ci !== 13) {
        var s = character_mismatch_err(c, ci);
        throw [
              Scan_failure,
              s
            ];
      } else if (c === /* "\n" */10) {
        ib[/* current_char_is_valid */2] = false;
        _c = /* "\n" */10;
        continue ;
      } else {
        var s$1 = character_mismatch_err(c, ci);
        throw [
              Scan_failure,
              s$1
            ];
      }
    }
  };
}

function token_bool(ib) {
  var s = token(ib);
  switch (s) {
    case "false" :
        return false;
    case "true" :
        return true;
    default:
      var s$1 = Curry._1(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "invalid boolean ",
                  Arg1: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  }
                },
                Arg1: "invalid boolean %S"
              }), s);
      throw [
            Scan_failure,
            s$1
          ];
  }
}

function token_int_literal(conv, ib) {
  var tok;
  var exit = 0;
  switch (conv) {
    case 98 :
        tok = "0b" + token(ib);
        break;
    case 111 :
        tok = "0o" + token(ib);
        break;
    case 100 :
    case 105 :
    case 117 :
        tok = token(ib);
        break;
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
        exit = 1;
        break;
    case 88 :
    case 120 :
        tok = "0x" + token(ib);
        break;
    default:
      exit = 1;
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "scanf.ml",
            507,
            11
          ]
        ];
  }
  var l = tok.length;
  if (l === 0 || Caml_string.get(tok, 0) !== /* "+" */43) {
    return tok;
  } else {
    return $$String.sub(tok, 1, l - 1 | 0);
  }
}

function token_float(ib) {
  return Caml_format.caml_float_of_string(token(ib));
}

function scan_decimal_digits(_width, ib) {
  while(true) {
    var width = _width;
    if (width === 0) {
      return width;
    } else {
      var c = peek_char(ib);
      if (ib[/* eof */0]) {
        return width;
      } else if (c >= 58) {
        if (c !== 95) {
          return width;
        } else {
          var width$1 = ignore_char(width, ib);
          _width = width$1;
          continue ;
        }
      } else if (c >= 48) {
        var width$2 = store_char(width, ib, c);
        _width = width$2;
        continue ;
      } else {
        return width;
      }
    }
  };
}

function scan_decimal_digits_plus(width, ib) {
  if (width === 0) {
    return bad_token_length("decimal digits");
  } else {
    var c = checked_peek_char(ib);
    if (c > 57 || c < 48) {
      var s = Curry._1(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "character ",
                  Arg1: /* constructor */{
                    tag: "Caml_char",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: " is not a decimal digit",
                      Arg1: "End_of_format"
                    }
                  }
                },
                Arg1: "character %C is not a decimal digit"
              }), c);
      throw [
            Scan_failure,
            s
          ];
    } else {
      var width$1 = store_char(width, ib, c);
      return scan_decimal_digits(width$1, ib);
    }
  }
}

function scan_digits_plus(basis, digitp, width, ib) {
  if (width === 0) {
    return bad_token_length("digits");
  } else {
    var c = checked_peek_char(ib);
    if (Curry._1(digitp, c)) {
      var _width = store_char(width, ib, c);
      while(true) {
        var width$1 = _width;
        if (width$1 === 0) {
          return width$1;
        } else {
          var c$1 = peek_char(ib);
          if (ib[/* eof */0]) {
            return width$1;
          } else if (Curry._1(digitp, c$1)) {
            _width = store_char(width$1, ib, c$1);
            continue ;
          } else if (c$1 !== 95) {
            return width$1;
          } else {
            _width = ignore_char(width$1, ib);
            continue ;
          }
        }
      };
    } else {
      var s = Curry._2(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "character ",
                  Arg1: /* constructor */{
                    tag: "Caml_char",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: " is not a valid ",
                      Arg1: /* constructor */{
                        tag: "String",
                        Arg0: "No_padding",
                        Arg1: /* constructor */{
                          tag: "String_literal",
                          Arg0: " digit",
                          Arg1: "End_of_format"
                        }
                      }
                    }
                  }
                },
                Arg1: "character %C is not a valid %s digit"
              }), c, basis);
      throw [
            Scan_failure,
            s
          ];
    }
  }
}

function is_binary_digit(param) {
  return param === 49 || param === 48;
}

function scan_binary_int(param, param$1) {
  return scan_digits_plus("binary", is_binary_digit, param, param$1);
}

function is_octal_digit(param) {
  return !(param > 55 || param < 48);
}

function scan_octal_int(param, param$1) {
  return scan_digits_plus("octal", is_octal_digit, param, param$1);
}

function is_hexa_digit(param) {
  var switcher = param - 48 | 0;
  if (switcher > 22 || switcher < 0) {
    return !(switcher > 54 || switcher < 49);
  } else {
    return switcher > 16 || switcher < 10;
  }
}

function scan_hexadecimal_int(param, param$1) {
  return scan_digits_plus("hexadecimal", is_hexa_digit, param, param$1);
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
  return scan_decimal_digits_plus(width$1, ib);
}

function scan_int_conv(conv, width, ib) {
  switch (conv) {
    case 98 :
        return scan_binary_int(width, ib);
    case 100 :
        return scan_optionally_signed_decimal_int(width, ib);
    case 105 :
        var width$1 = width;
        var ib$1 = ib;
        var width$2 = scan_sign(width$1, ib$1);
        var width$3 = width$2;
        var ib$2 = ib$1;
        var c = checked_peek_char(ib$2);
        if (c !== 48) {
          return scan_decimal_digits_plus(width$3, ib$2);
        } else {
          var width$4 = store_char(width$3, ib$2, c);
          if (width$4 === 0) {
            return width$4;
          } else {
            var c$1 = peek_char(ib$2);
            if (ib$2[/* eof */0]) {
              return width$4;
            } else if (c$1 >= 99) {
              if (c$1 !== 111) {
                if (c$1 !== 120) {
                  return scan_decimal_digits(width$4, ib$2);
                } else {
                  return scan_hexadecimal_int(store_char(width$4, ib$2, c$1), ib$2);
                }
              } else {
                return scan_octal_int(store_char(width$4, ib$2, c$1), ib$2);
              }
            } else if (c$1 !== 88) {
              if (c$1 >= 98) {
                return scan_binary_int(store_char(width$4, ib$2, c$1), ib$2);
              } else {
                return scan_decimal_digits(width$4, ib$2);
              }
            } else {
              return scan_hexadecimal_int(store_char(width$4, ib$2, c$1), ib$2);
            }
          }
        }
    case 111 :
        return scan_octal_int(width, ib);
    case 117 :
        return scan_decimal_digits_plus(width, ib);
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
        return scan_hexadecimal_int(width, ib);
    default:
      
  }
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "scanf.ml",
          674,
          9
        ]
      ];
}

function scan_frac_part(width, ib) {
  if (width === 0) {
    return width;
  } else {
    var c = peek_char(ib);
    if (ib[/* eof */0] || c > 57 || c < 48) {
      return width;
    } else {
      return scan_decimal_digits(store_char(width, ib, c), ib);
    }
  }
}

function scan_exp_part(width, ib) {
  if (width === 0) {
    return width;
  } else {
    var c = peek_char(ib);
    if (ib[/* eof */0] || c !== 69 && c !== 101) {
      return width;
    } else {
      return scan_optionally_signed_decimal_int(store_char(width, ib, c), ib);
    }
  }
}

function scan_int_part(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digits(width$1, ib);
}

function scan_float(width, precision, ib) {
  var width$1 = scan_int_part(width, ib);
  if (width$1 === 0) {
    return /* tuple */[
            width$1,
            precision
          ];
  } else {
    var c = peek_char(ib);
    if (ib[/* eof */0]) {
      return /* tuple */[
              width$1,
              precision
            ];
    } else if (c !== 46) {
      return /* tuple */[
              scan_exp_part(width$1, ib),
              precision
            ];
    } else {
      var width$2 = store_char(width$1, ib, c);
      var precision$1 = width$2 < precision ? width$2 : precision;
      var width$3 = width$2 - (precision$1 - scan_frac_part(precision$1, ib) | 0) | 0;
      return /* tuple */[
              scan_exp_part(width$3, ib),
              precision$1
            ];
    }
  }
}

function scan_caml_float(width, precision, ib) {
  var width$1 = scan_optionally_signed_decimal_int(width, ib);
  if (width$1 === 0) {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  } else {
    var c = peek_char(ib);
    if (ib[/* eof */0]) {
      throw [
            Scan_failure,
            "no dot or exponent part found in float token"
          ];
    } else {
      var switcher = c - 69 | 0;
      if (switcher > 32 || switcher < 0) {
        if (switcher !== -23) {
          throw [
                Scan_failure,
                "no dot or exponent part found in float token"
              ];
        } else {
          var width$2 = store_char(width$1, ib, c);
          var precision$1 = width$2 < precision ? width$2 : precision;
          var width$3 = width$2 - (precision$1 - scan_frac_part(precision$1, ib) | 0) | 0;
          return scan_exp_part(width$3, ib);
        }
      } else if (switcher > 31 || switcher < 1) {
        return scan_exp_part(width$1, ib);
      } else {
        throw [
              Scan_failure,
              "no dot or exponent part found in float token"
            ];
      }
    }
  }
}

function scan_string(stp, width, ib) {
  var _width = width;
  while(true) {
    var width$1 = _width;
    if (width$1 === 0) {
      return width$1;
    } else {
      var c = peek_char(ib);
      if (ib[/* eof */0]) {
        return width$1;
      } else if (stp !== undefined) {
        if (c === stp) {
          ib[/* current_char_is_valid */2] = false;
          return width$1;
        } else {
          _width = store_char(width$1, ib, c);
          continue ;
        }
      } else {
        var switcher = c - 9 | 0;
        if (switcher > 4 || switcher < 0) {
          if (switcher !== 23) {
            _width = store_char(width$1, ib, c);
            continue ;
          } else {
            return width$1;
          }
        } else if (switcher === 3 || switcher === 2) {
          _width = store_char(width$1, ib, c);
          continue ;
        } else {
          return width$1;
        }
      }
    }
  };
}

function scan_char(width, ib) {
  return store_char(width, ib, checked_peek_char(ib));
}

function char_for_backslash(c) {
  if (c >= 110) {
    if (c >= 117) {
      return c;
    } else {
      switch (c - 110 | 0) {
        case 0 :
            return /* "\n" */10;
        case 4 :
            return /* "\r" */13;
        case 1 :
        case 2 :
        case 3 :
        case 5 :
            return c;
        case 6 :
            return /* "\t" */9;
        
      }
    }
  } else if (c !== 98) {
    return c;
  } else {
    return /* "\b" */8;
  }
}

function char_for_decimal_code(c0, c1, c2) {
  var c = (Caml_int32.imul(100, c0 - /* "0" */48 | 0) + Caml_int32.imul(10, c1 - /* "0" */48 | 0) | 0) + (c2 - /* "0" */48 | 0) | 0;
  if (c < 0 || c > 255) {
    var s = Curry._3(Printf.sprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "bad character decimal encoding \\",
                Arg1: /* constructor */{
                  tag: "Char",
                  Arg0: /* constructor */{
                    tag: "Char",
                    Arg0: /* constructor */{
                      tag: "Char",
                      Arg0: "End_of_format"
                    }
                  }
                }
              },
              Arg1: "bad character decimal encoding \\%c%c%c"
            }), c0, c1, c2);
    throw [
          Scan_failure,
          s
        ];
  } else {
    return Pervasives.char_of_int(c);
  }
}

function hexadecimal_value_of_char(c) {
  if (c >= /* "a" */97) {
    return c - 87 | 0;
  } else if (c >= /* "A" */65) {
    return c - 55 | 0;
  } else {
    return c - /* "0" */48 | 0;
  }
}

function char_for_hexadecimal_code(c1, c2) {
  var c = (hexadecimal_value_of_char(c1) << 4) + hexadecimal_value_of_char(c2) | 0;
  if (c < 0 || c > 255) {
    var s = Curry._2(Printf.sprintf(/* constructor */{
              tag: "Format",
              Arg0: /* constructor */{
                tag: "String_literal",
                Arg0: "bad character hexadecimal encoding \\",
                Arg1: /* constructor */{
                  tag: "Char",
                  Arg0: /* constructor */{
                    tag: "Char",
                    Arg0: "End_of_format"
                  }
                }
              },
              Arg1: "bad character hexadecimal encoding \\%c%c"
            }), c1, c2);
    throw [
          Scan_failure,
          s
        ];
  } else {
    return Pervasives.char_of_int(c);
  }
}

function check_next_char(message, width, ib) {
  if (width === 0) {
    return bad_token_length(message);
  } else {
    var c = peek_char(ib);
    if (ib[/* eof */0]) {
      var message$1 = message;
      var s = Curry._1(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "scanning of ",
                  Arg1: /* constructor */{
                    tag: "String",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "String_literal",
                      Arg0: " failed: premature end of file occurred before end of token",
                      Arg1: "End_of_format"
                    }
                  }
                },
                Arg1: "scanning of %s failed: premature end of file occurred before end of token"
              }), message$1);
      throw [
            Scan_failure,
            s
          ];
    } else {
      return c;
    }
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
              var switcher = c - 48 | 0;
              if (switcher > 22 || switcher < 0) {
                if (switcher > 54 || switcher < 49) {
                  return bad_input_escape(c);
                } else {
                  return c;
                }
              } else if (switcher > 16 || switcher < 10) {
                return c;
              } else {
                return bad_input_escape(c);
              }
            };
            var c1 = get_digit(/* () */0);
            var c2 = get_digit(/* () */0);
            return store_char(width - 2 | 0, ib, char_for_hexadecimal_code(c1, c2));
        default:
          return bad_input_escape(c);
      }
    } else if (c >= 48) {
      var get_digit$1 = function (param) {
        var c = next_char(ib);
        if (c > 57 || c < 48) {
          return bad_input_escape(c);
        } else {
          return c;
        }
      };
      var c1$1 = get_digit$1(/* () */0);
      var c2$1 = get_digit$1(/* () */0);
      return store_char(width - 2 | 0, ib, char_for_decimal_code(c, c1$1, c2$1));
    } else {
      return bad_input_escape(c);
    }
  } else if (c !== 34 && c < 39) {
    return bad_input_escape(c);
  }
  return store_char(width, ib, char_for_backslash(c));
}

function scan_caml_char(width, ib) {
  var find_stop = function (width) {
    var c = check_next_char("a Char", width, ib);
    if (c !== 39) {
      var s = character_mismatch_err(/* "'" */39, c);
      throw [
            Scan_failure,
            s
          ];
    } else {
      return ignore_char(width, ib);
    }
  };
  var width$1 = width;
  var c = checked_peek_char(ib);
  if (c !== 39) {
    var s = character_mismatch_err(/* "'" */39, c);
    throw [
          Scan_failure,
          s
        ];
  } else {
    var width$2 = ignore_char(width$1, ib);
    var c$1 = check_next_char("a Char", width$2, ib);
    if (c$1 !== 92) {
      return find_stop(store_char(width$2, ib, c$1));
    } else {
      return find_stop(scan_backslash_char(ignore_char(width$2, ib), ib));
    }
  }
}

function scan_caml_string(width, ib) {
  var find_stop = function (_width) {
    while(true) {
      var width = _width;
      var c = check_next_char("a String", width, ib);
      if (c !== 34) {
        if (c !== 92) {
          _width = store_char(width, ib, c);
          continue ;
        } else {
          var width$1 = ignore_char(width, ib);
          var match = check_next_char("a String", width$1, ib);
          if (match !== 10) {
            if (match !== 13) {
              return find_stop(scan_backslash_char(width$1, ib));
            } else {
              var width$2 = ignore_char(width$1, ib);
              var match$1 = check_next_char("a String", width$2, ib);
              if (match$1 !== 10) {
                return find_stop(store_char(width$2, ib, /* "\r" */13));
              } else {
                return skip_spaces(ignore_char(width$2, ib));
              }
            }
          } else {
            return skip_spaces(ignore_char(width$1, ib));
          }
        }
      } else {
        return ignore_char(width, ib);
      }
    };
  };
  var skip_spaces = function (_width) {
    while(true) {
      var width = _width;
      var match = check_next_char("a String", width, ib);
      if (match !== 32) {
        return find_stop(width);
      } else {
        _width = ignore_char(width, ib);
        continue ;
      }
    };
  };
  var width$1 = width;
  var c = checked_peek_char(ib);
  if (c !== 34) {
    var s = character_mismatch_err(/* "\"" */34, c);
    throw [
          Scan_failure,
          s
        ];
  } else {
    return find_stop(ignore_char(width$1, ib));
  }
}

function scan_bool(ib) {
  var c = checked_peek_char(ib);
  var m;
  if (c !== 102) {
    if (c !== 116) {
      var s = Curry._1(Printf.sprintf(/* constructor */{
                tag: "Format",
                Arg0: /* constructor */{
                  tag: "String_literal",
                  Arg0: "the character ",
                  Arg1: /* constructor */{
                    tag: "Caml_char",
                    Arg0: /* constructor */{
                      tag: "String_literal",
                      Arg0: " cannot start a boolean",
                      Arg1: "End_of_format"
                    }
                  }
                },
                Arg1: "the character %C cannot start a boolean"
              }), c);
      throw [
            Scan_failure,
            s
          ];
    } else {
      m = 4;
    }
  } else {
    m = 5;
  }
  return scan_string(undefined, m, ib);
}

function scan_chars_in_char_set(char_set, scan_indic, width, ib) {
  var scan_chars = function (_i, stp) {
    while(true) {
      var i = _i;
      var c = peek_char(ib);
      if (i > 0 && !ib[/* eof */0] && CamlinternalFormat.is_in_char_set(char_set, c) && c !== stp) {
        store_char(Pervasives.max_int, ib, c);
        _i = i - 1 | 0;
        continue ;
      } else {
        return 0;
      }
    };
  };
  if (scan_indic !== undefined) {
    var c = scan_indic;
    scan_chars(width, c);
    if (ib[/* eof */0]) {
      return 0;
    } else {
      var ci = peek_char(ib);
      if (c === ci) {
        ib[/* current_char_is_valid */2] = false;
        return /* () */0;
      } else {
        var s = character_mismatch_err(c, ci);
        throw [
              Scan_failure,
              s
            ];
      }
    }
  } else {
    return scan_chars(width, -1);
  }
}

function scanf_bad_input(ib, x) {
  var s;
  if (x[0] === Scan_failure || x[0] === Caml_builtin_exceptions.failure) {
    s = x[1];
  } else {
    throw x;
  }
  var i = char_count(ib);
  var s$1 = Curry._2(Printf.sprintf(/* constructor */{
            tag: "Format",
            Arg0: /* constructor */{
              tag: "String_literal",
              Arg0: "scanf: bad input at char number ",
              Arg1: /* constructor */{
                tag: "Int",
                Arg0: "Int_i",
                Arg1: "No_padding",
                Arg2: "No_precision",
                Arg3: /* constructor */{
                  tag: "String_literal",
                  Arg0: ": ",
                  Arg1: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: "End_of_format"
                  }
                }
              }
            },
            Arg1: "scanf: bad input at char number %i: %S"
          }), i, s);
  throw [
        Scan_failure,
        s$1
      ];
}

function get_counter(ib, counter) {
  switch (counter) {
    case "Line_counter" :
        return ib[/* line_count */4];
    case "Char_counter" :
        return char_count(ib);
    case "Token_counter" :
        return ib[/* token_count */5];
    
  }
}

function width_of_pad_opt(pad_opt) {
  if (pad_opt !== undefined) {
    return pad_opt;
  } else {
    return Pervasives.max_int;
  }
}

function stopper_of_formatting_lit(fmting) {
  if (fmting === "Escaped_percent") {
    return /* tuple */[
            /* "%" */37,
            ""
          ];
  } else {
    var str = CamlinternalFormat.string_of_formatting_lit(fmting);
    var stp = Caml_string.get(str, 1);
    var sub_str = $$String.sub(str, 2, str.length - 2 | 0);
    return /* tuple */[
            stp,
            sub_str
          ];
  }
}

function take_format_readers(k, _fmt) {
  while(true) {
    var fmt = _fmt;
    if (typeof fmt === "string") {
      return Curry._1(k, "Nil");
    } else {
      switch (/* XXX */fmt.tag) {
        case "Int" :
        case "Int32" :
        case "Nativeint" :
        case "Int64" :
        case "Float" :
            _fmt = fmt.Arg3;
            continue ;
        case "Format_subst" :
            return take_fmtty_format_readers(k, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmt.Arg1)), fmt.Arg2);
        case "Formatting_gen" :
            _fmt = CamlinternalFormatBasics.concat_fmt(fmt.Arg0.Arg0.Arg0, fmt.Arg1);
            continue ;
        case "Reader" :
            var fmt_rest = fmt.Arg0;
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* constructor */{
                            tag: "Cons",
                            Arg0: reader,
                            Arg1: readers_rest
                          });
              };
              return take_format_readers(new_k, fmt_rest);
            }
            }(fmt_rest));
        case "String" :
        case "Caml_string" :
        case "String_literal" :
        case "Char_literal" :
        case "Formatting_lit" :
        case "Scan_get_counter" :
            _fmt = fmt.Arg1;
            continue ;
        case "Ignored_param" :
            var k$1 = k;
            var ign = fmt.Arg0;
            var fmt$1 = fmt.Arg1;
            if (typeof ign === "string") {
              if (ign === "Ignored_reader") {
                return (function(k$1,fmt$1){
                return function (reader) {
                  var new_k = function (readers_rest) {
                    return Curry._1(k$1, /* constructor */{
                                tag: "Cons",
                                Arg0: reader,
                                Arg1: readers_rest
                              });
                  };
                  return take_format_readers(new_k, fmt$1);
                }
                }(k$1,fmt$1));
              } else {
                return take_format_readers(k$1, fmt$1);
              }
            } else if (/* XXX */ign.tag === "Ignored_format_subst") {
              return take_fmtty_format_readers(k$1, ign.Arg1, fmt$1);
            } else {
              return take_format_readers(k$1, fmt$1);
            }
        case "Format_arg" :
        case "Scan_char_set" :
        case "Custom" :
            _fmt = fmt.Arg2;
            continue ;
        default:
          _fmt = fmt.Arg0;
          continue ;
      }
    }
  };
}

function take_fmtty_format_readers(k, _fmtty, fmt) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "string") {
      return take_format_readers(k, fmt);
    } else {
      switch (/* XXX */fmtty.tag) {
        case "Format_arg_ty" :
            _fmtty = fmtty.Arg1;
            continue ;
        case "Format_subst_ty" :
            var ty = CamlinternalFormat.trans(CamlinternalFormat.symm(fmtty.Arg0), fmtty.Arg1);
            _fmtty = CamlinternalFormatBasics.concat_fmtty(ty, fmtty.Arg2);
            continue ;
        case "Reader_ty" :
            var fmt_rest = fmtty.Arg0;
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* constructor */{
                            tag: "Cons",
                            Arg0: reader,
                            Arg1: readers_rest
                          });
              };
              return take_fmtty_format_readers(new_k, fmt_rest, fmt);
            }
            }(fmt_rest));
        case "Ignored_reader_ty" :
            var fmt_rest$1 = fmtty.Arg0;
            return (function(fmt_rest$1){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* constructor */{
                            tag: "Cons",
                            Arg0: reader,
                            Arg1: readers_rest
                          });
              };
              return take_fmtty_format_readers(new_k, fmt_rest$1, fmt);
            }
            }(fmt_rest$1));
        default:
          _fmtty = fmtty.Arg0;
          continue ;
      }
    }
  };
}

function make_scanf(ib, _fmt, readers) {
  while(true) {
    var fmt = _fmt;
    if (typeof fmt === "string") {
      return "Nil";
    } else {
      switch (/* XXX */fmt.tag) {
        case "Char" :
            scan_char(0, ib);
            var c = Caml_string.get(token(ib), 0);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: c,
                    Arg1: make_scanf(ib, fmt.Arg0, readers)
                  };
        case "Caml_char" :
            scan_caml_char(0, ib);
            var c$1 = Caml_string.get(token(ib), 0);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: c$1,
                    Arg1: make_scanf(ib, fmt.Arg0, readers)
                  };
        case "String" :
            var rest = fmt.Arg1;
            var pad = fmt.Arg0;
            if (typeof rest !== "string") {
              switch (/* XXX */rest.tag) {
                case "Formatting_lit" :
                    var match = stopper_of_formatting_lit(rest.Arg0);
                    var stp = match[0];
                    var scan = (function(stp){
                    return function scan(width, param, ib) {
                      return scan_string(stp, width, ib);
                    }
                    }(stp));
                    var str_rest = /* constructor */{
                      tag: "String_literal",
                      Arg0: match[1],
                      Arg1: rest.Arg1
                    };
                    return pad_prec_scanf(ib, str_rest, readers, pad, "No_precision", scan, token);
                case "Formatting_gen" :
                    var match$1 = rest.Arg0;
                    if (/* XXX */match$1.tag === "Open_tag") {
                      var scan$1 = function (width, param, ib) {
                        return scan_string(/* "{" */123, width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1.Arg0.Arg0, rest.Arg1), readers, pad, "No_precision", scan$1, token);
                    } else {
                      var scan$2 = function (width, param, ib) {
                        return scan_string(/* "[" */91, width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1.Arg0.Arg0, rest.Arg1), readers, pad, "No_precision", scan$2, token);
                    }
                default:
                  
              }
            }
            var scan$3 = function (width, param, ib) {
              return scan_string(undefined, width, ib);
            };
            return pad_prec_scanf(ib, rest, readers, pad, "No_precision", scan$3, token);
        case "Caml_string" :
            var scan$4 = function (width, param, ib) {
              return scan_caml_string(width, ib);
            };
            return pad_prec_scanf(ib, fmt.Arg1, readers, fmt.Arg0, "No_precision", scan$4, token);
        case "Int" :
            var c$2 = CamlinternalFormat.char_of_iconv(fmt.Arg0);
            var scan$5 = (function(c$2){
            return function scan$5(width, param, ib) {
              return scan_int_conv(c$2, width, ib);
            }
            }(c$2));
            return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan$5, (function(c$2){
                      return function (param) {
                        return Caml_format.caml_int_of_string(token_int_literal(c$2, param));
                      }
                      }(c$2)));
        case "Int32" :
            var c$3 = CamlinternalFormat.char_of_iconv(fmt.Arg0);
            var scan$6 = (function(c$3){
            return function scan$6(width, param, ib) {
              return scan_int_conv(c$3, width, ib);
            }
            }(c$3));
            return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan$6, (function(c$3){
                      return function (param) {
                        return Caml_format.caml_int32_of_string(token_int_literal(c$3, param));
                      }
                      }(c$3)));
        case "Nativeint" :
            var c$4 = CamlinternalFormat.char_of_iconv(fmt.Arg0);
            var scan$7 = (function(c$4){
            return function scan$7(width, param, ib) {
              return scan_int_conv(c$4, width, ib);
            }
            }(c$4));
            return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan$7, (function(c$4){
                      return function (param) {
                        return Caml_format.caml_nativeint_of_string(token_int_literal(c$4, param));
                      }
                      }(c$4)));
        case "Int64" :
            var c$5 = CamlinternalFormat.char_of_iconv(fmt.Arg0);
            var scan$8 = (function(c$5){
            return function scan$8(width, param, ib) {
              return scan_int_conv(c$5, width, ib);
            }
            }(c$5));
            return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan$8, (function(c$5){
                      return function (param) {
                        return Caml_format.caml_int64_of_string(token_int_literal(c$5, param));
                      }
                      }(c$5)));
        case "Float" :
            if (fmt.Arg0 === "Float_F") {
              return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan_caml_float, token_float);
            } else {
              return pad_prec_scanf(ib, fmt.Arg3, readers, fmt.Arg1, fmt.Arg2, scan_float, token_float);
            }
        case "Bool" :
            scan_bool(ib);
            var b = token_bool(ib);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: b,
                    Arg1: make_scanf(ib, fmt.Arg0, readers)
                  };
        case "Flush" :
            if (end_of_input(ib)) {
              _fmt = fmt.Arg0;
              continue ;
            } else {
              throw [
                    Scan_failure,
                    "end of input not found"
                  ];
            }
        case "String_literal" :
            var f = function (param) {
              return check_char(ib, param);
            };
            Bytes.iter(f, Caml_bytes.bytes_of_string(fmt.Arg0));
            _fmt = fmt.Arg1;
            continue ;
        case "Char_literal" :
            check_char(ib, fmt.Arg0);
            _fmt = fmt.Arg1;
            continue ;
        case "Format_arg" :
            scan_caml_string(width_of_pad_opt(fmt.Arg0), ib);
            var s = token(ib);
            var fmt$1;
            try {
              fmt$1 = CamlinternalFormat.format_of_string_fmtty(s, fmt.Arg1);
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn[0] === Caml_builtin_exceptions.failure) {
                throw [
                      Scan_failure,
                      exn[1]
                    ];
              }
              throw exn;
            }
            return /* constructor */{
                    tag: "Cons",
                    Arg0: fmt$1,
                    Arg1: make_scanf(ib, fmt.Arg2, readers)
                  };
        case "Format_subst" :
            var fmtty = fmt.Arg1;
            scan_caml_string(width_of_pad_opt(fmt.Arg0), ib);
            var s$1 = token(ib);
            var match$2;
            try {
              var match$3 = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
              var match$4 = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
              match$2 = /* tuple */[
                CamlinternalFormat.type_format(match$3.Arg0, CamlinternalFormatBasics.erase_rel(fmtty)),
                CamlinternalFormat.type_format(match$4.Arg0, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmtty)))
              ];
            }
            catch (raw_exn$1){
              var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
              if (exn$1[0] === Caml_builtin_exceptions.failure) {
                throw [
                      Scan_failure,
                      exn$1[1]
                    ];
              }
              throw exn$1;
            }
            return /* constructor */{
                    tag: "Cons",
                    Arg0: /* constructor */{
                      tag: "Format",
                      Arg0: match$2[0],
                      Arg1: s$1
                    },
                    Arg1: make_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$2[1], fmt.Arg2), readers)
                  };
        case "Alpha" :
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%a\""
                ];
        case "Theta" :
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%t\""
                ];
        case "Formatting_lit" :
            var s$2 = CamlinternalFormat.string_of_formatting_lit(fmt.Arg0);
            var f$1 = function (param) {
              return check_char(ib, param);
            };
            Bytes.iter(f$1, Caml_bytes.bytes_of_string(s$2));
            _fmt = fmt.Arg1;
            continue ;
        case "Formatting_gen" :
            var match$5 = fmt.Arg0;
            check_char(ib, /* "@" */64);
            if (/* XXX */match$5.tag === "Open_tag") {
              check_char(ib, /* "{" */123);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$5.Arg0.Arg0, fmt.Arg1);
              continue ;
            } else {
              check_char(ib, /* "[" */91);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$5.Arg0.Arg0, fmt.Arg1);
              continue ;
            }
        case "Reader" :
            var x = Curry._1(readers.Arg0, ib);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: x,
                    Arg1: make_scanf(ib, fmt.Arg0, readers.Arg1)
                  };
        case "Scan_char_set" :
            var rest$1 = fmt.Arg2;
            var char_set = fmt.Arg1;
            var width_opt = fmt.Arg0;
            if (typeof rest$1 !== "string" && /* XXX */rest$1.tag === "Formatting_lit") {
              var match$6 = stopper_of_formatting_lit(rest$1.Arg0);
              var width = width_of_pad_opt(width_opt);
              scan_chars_in_char_set(char_set, match$6[0], width, ib);
              var s$3 = token(ib);
              var str_rest$1 = /* constructor */{
                tag: "String_literal",
                Arg0: match$6[1],
                Arg1: rest$1.Arg1
              };
              return /* constructor */{
                      tag: "Cons",
                      Arg0: s$3,
                      Arg1: make_scanf(ib, str_rest$1, readers)
                    };
            }
            var width$1 = width_of_pad_opt(width_opt);
            scan_chars_in_char_set(char_set, undefined, width$1, ib);
            var s$4 = token(ib);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: s$4,
                    Arg1: make_scanf(ib, rest$1, readers)
                  };
        case "Scan_get_counter" :
            var count = get_counter(ib, fmt.Arg0);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: count,
                    Arg1: make_scanf(ib, fmt.Arg1, readers)
                  };
        case "Scan_next_char" :
            var c$6 = checked_peek_char(ib);
            return /* constructor */{
                    tag: "Cons",
                    Arg0: c$6,
                    Arg1: make_scanf(ib, fmt.Arg0, readers)
                  };
        case "Ignored_param" :
            var match$7 = CamlinternalFormat.param_format_of_ignored_format(fmt.Arg0, fmt.Arg1);
            var match$8 = make_scanf(ib, match$7.Arg0, readers);
            if (match$8 !== "Nil") {
              return match$8.Arg1;
            } else {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "scanf.ml",
                      1258,
                      13
                    ]
                  ];
            }
        case "Custom" :
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%?\" (custom converter)"
                ];
        
      }
    }
  };
}

function pad_prec_scanf(ib, fmt, readers, pad, prec, scan, token) {
  if (typeof pad === "string") {
    if (typeof prec === "string") {
      if (prec === "No_precision") {
        Curry._3(scan, Pervasives.max_int, Pervasives.max_int, ib);
        var x = Curry._1(token, ib);
        return /* constructor */{
                tag: "Cons",
                Arg0: x,
                Arg1: make_scanf(ib, fmt, readers)
              };
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "scanf: bad conversion \"%*\""
            ];
      }
    } else {
      Curry._3(scan, Pervasives.max_int, prec.Arg0, ib);
      var x$1 = Curry._1(token, ib);
      return /* constructor */{
              tag: "Cons",
              Arg0: x$1,
              Arg1: make_scanf(ib, fmt, readers)
            };
    }
  } else if (/* XXX */pad.tag === "Lit_padding") {
    switch (pad.Arg0) {
      case "Left" :
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "scanf: bad conversion \"%-\""
              ];
      case "Right" :
      case "Zeros" :
          break;
      
    }
    var w = pad.Arg1;
    if (typeof prec === "string") {
      if (prec === "No_precision") {
        Curry._3(scan, w, Pervasives.max_int, ib);
        var x$2 = Curry._1(token, ib);
        return /* constructor */{
                tag: "Cons",
                Arg0: x$2,
                Arg1: make_scanf(ib, fmt, readers)
              };
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "scanf: bad conversion \"%*\""
            ];
      }
    } else {
      Curry._3(scan, w, prec.Arg0, ib);
      var x$3 = Curry._1(token, ib);
      return /* constructor */{
              tag: "Cons",
              Arg0: x$3,
              Arg1: make_scanf(ib, fmt, readers)
            };
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "scanf: bad conversion \"%*\""
        ];
  }
}

function kscanf(ib, ef, param) {
  var str = param.Arg1;
  var fmt = param.Arg0;
  var k = function (readers, f) {
    $$Buffer.reset(ib[/* tokbuf */7]);
    var match;
    try {
      match = /* constructor */{
        tag: "Args",
        Arg0: make_scanf(ib, fmt, readers)
      };
    }
    catch (raw_exc){
      var exc = Caml_js_exceptions.internalToOCamlException(raw_exc);
      if (exc[0] === Scan_failure) {
        match = /* constructor */{
          tag: "Exc",
          Arg0: exc
        };
      } else if (exc[0] === Caml_builtin_exceptions.failure) {
        match = /* constructor */{
          tag: "Exc",
          Arg0: exc
        };
      } else if (exc === Caml_builtin_exceptions.end_of_file) {
        match = /* constructor */{
          tag: "Exc",
          Arg0: exc
        };
      } else if (exc[0] === Caml_builtin_exceptions.invalid_argument) {
        var s = exc[1] + (" in format \"" + ($$String.escaped(str) + "\""));
        throw [
              Caml_builtin_exceptions.invalid_argument,
              s
            ];
      } else {
        throw exc;
      }
    }
    if (/* XXX */match.tag === "Args") {
      var _f = f;
      var _args = match.Arg0;
      while(true) {
        var args = _args;
        var f$1 = _f;
        if (args !== "Nil") {
          _args = args.Arg1;
          _f = Curry._1(f$1, args.Arg0);
          continue ;
        } else {
          return f$1;
        }
      };
    } else {
      return Curry._2(ef, ib, match.Arg0);
    }
  };
  return take_format_readers(k, fmt);
}

function ksscanf(s, ef, fmt) {
  return kscanf(from_string(s), ef, fmt);
}

function kfscanf(ic, ef, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), ef, fmt);
}

function bscanf(ib, fmt) {
  return kscanf(ib, scanf_bad_input, fmt);
}

function fscanf(ic, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), scanf_bad_input, fmt);
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
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      throw [
            Scan_failure,
            exn[1]
          ];
    }
    throw exn;
  }
  return Curry._1(f, tmp);
}

function sscanf_format(s, format, f) {
  return bscanf_format(from_string(s), format, f);
}

function string_to_String(s) {
  var l = s.length;
  var b = $$Buffer.create(l + 2 | 0);
  $$Buffer.add_char(b, /* "\"" */34);
  for(var i = 0 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    var c = Caml_string.get(s, i);
    if (c === /* "\"" */34) {
      $$Buffer.add_char(b, /* "\\" */92);
    }
    $$Buffer.add_char(b, c);
  }
  $$Buffer.add_char(b, /* "\"" */34);
  return $$Buffer.contents(b);
}

function format_from_string(s, fmt) {
  return sscanf_format(string_to_String(s), fmt, (function (x) {
                return x;
              }));
}

function unescaped(s) {
  return Curry._1(sscanf("\"" + (s + "\""), /* constructor */{
                  tag: "Format",
                  Arg0: /* constructor */{
                    tag: "Caml_string",
                    Arg0: "No_padding",
                    Arg1: /* constructor */{
                      tag: "Flush",
                      Arg0: "End_of_format"
                    }
                  },
                  Arg1: "%S%!"
                }), (function (x) {
                return x;
              }));
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
exports.fscanf = fscanf;
exports.sscanf = sscanf;
exports.scanf = scanf;
exports.kscanf = kscanf;
exports.ksscanf = ksscanf;
exports.kfscanf = kfscanf;
exports.bscanf_format = bscanf_format;
exports.sscanf_format = sscanf_format;
exports.format_from_string = format_from_string;
exports.unescaped = unescaped;
/* stdin Not a pure module */
