'use strict';

var List = require("./list.js");
var Block = require("./block.js");
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
    var c = Curry._1(ib[/* ic_get_next_char */6], /* () */0);
    ib[/* ic_current_char */1] = c;
    ib[/* ic_current_char_is_valid */2] = true;
    ib[/* ic_char_count */3] = ib[/* ic_char_count */3] + 1 | 0;
    if (c === /* "\n" */10) {
      ib[/* ic_line_count */4] = ib[/* ic_line_count */4] + 1 | 0;
    }
    return c;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.end_of_file) {
      ib[/* ic_current_char */1] = /* "\000" */0;
      ib[/* ic_current_char_is_valid */2] = false;
      ib[/* ic_eof */0] = true;
      return /* "\000" */0;
    } else {
      throw exn;
    }
  }
}

function peek_char(ib) {
  if (ib[/* ic_current_char_is_valid */2]) {
    return ib[/* ic_current_char */1];
  } else {
    return next_char(ib);
  }
}

function checked_peek_char(ib) {
  var c = peek_char(ib);
  if (ib[/* ic_eof */0]) {
    throw Caml_builtin_exceptions.end_of_file;
  }
  return c;
}

function end_of_input(ib) {
  peek_char(ib);
  return ib[/* ic_eof */0];
}

function beginning_of_input(ib) {
  return ib[/* ic_char_count */3] === 0;
}

function name_of_input(ib) {
  var match = ib[/* ic_input_name */8];
  if (typeof match === "number") {
    if (match === 0) {
      return "unnamed function";
    } else {
      return "unnamed character string";
    }
  } else if (match.tag) {
    return match[0];
  } else {
    return "unnamed Pervasives input channel";
  }
}

function char_count(ib) {
  if (ib[/* ic_current_char_is_valid */2]) {
    return ib[/* ic_char_count */3] - 1 | 0;
  } else {
    return ib[/* ic_char_count */3];
  }
}

function token(ib) {
  var token_buffer = ib[/* ic_token_buffer */7];
  var tok = $$Buffer.contents(token_buffer);
  token_buffer[/* position */1] = 0;
  ib[/* ic_token_count */5] = ib[/* ic_token_count */5] + 1 | 0;
  return tok;
}

function ignore_char(width, ib) {
  var width$1 = width - 1 | 0;
  ib[/* ic_current_char_is_valid */2] = false;
  return width$1;
}

function store_char(width, ib, c) {
  $$Buffer.add_char(ib[/* ic_token_buffer */7], c);
  return ignore_char(width, ib);
}

function create(iname, next) {
  return /* record */[
          /* ic_eof */false,
          /* ic_current_char : "\000" */0,
          /* ic_current_char_is_valid */false,
          /* ic_char_count */0,
          /* ic_line_count */0,
          /* ic_token_count */0,
          /* ic_get_next_char */next,
          /* ic_token_buffer */$$Buffer.create(1024),
          /* ic_input_name */iname
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
  return create(/* From_string */1, next);
}

function from_function(param) {
  return create(/* From_function */0, param);
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

var stdin = from_ic(scan_raise_at_end, /* From_file */Block.__(1, [
        "-",
        Pervasives.stdin
      ]), Pervasives.stdin);

function open_in_file(open_in, fname) {
  if (fname === "-") {
    return stdin;
  } else {
    var ic = Curry._1(open_in, fname);
    return from_ic(scan_close_at_end, /* From_file */Block.__(1, [
                  fname,
                  ic
                ]), ic);
  }
}

function open_in(param) {
  return open_in_file(Pervasives.open_in, param);
}

function open_in_bin(param) {
  return open_in_file(Pervasives.open_in_bin, param);
}

function from_channel(ic) {
  return from_ic(scan_raise_at_end, /* From_channel */Block.__(0, [ic]), ic);
}

function close_in(ib) {
  var match = ib[/* ic_input_name */8];
  if (typeof match === "number") {
    return /* () */0;
  } else if (match.tag) {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(match[1]);
  } else {
    return Caml_external_polyfill.resolve("caml_ml_close_channel")(match[0]);
  }
}

var memo = /* record */[/* contents : [] */0];

function memo_from_ic(scan_close_ic, ic) {
  try {
    return List.assq(ic, memo[0]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      var ib = from_ic(scan_close_ic, /* From_channel */Block.__(0, [ic]), ic);
      memo[0] = /* :: */[
        /* tuple */[
          ic,
          ib
        ],
        memo[0]
      ];
      return ib;
    } else {
      throw exn;
    }
  }
}

var Scan_failure = Caml_exceptions.create("Scanf.Scan_failure");

function bad_input_escape(c) {
  var s = Curry._1(Printf.sprintf(/* Format */[
            /* String_literal */Block.__(11, [
                "illegal escape character ",
                /* Caml_char */Block.__(1, [/* End_of_format */0])
              ]),
            "illegal escape character %C"
          ]), c);
  throw [
        Scan_failure,
        s
      ];
}

function bad_token_length(message) {
  var s = Curry._1(Printf.sprintf(/* Format */[
            /* String_literal */Block.__(11, [
                "scanning of ",
                /* String */Block.__(2, [
                    /* No_padding */0,
                    /* String_literal */Block.__(11, [
                        " failed: the specified length was too short for token",
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "scanning of %s failed: the specified length was too short for token"
          ]), message);
  throw [
        Scan_failure,
        s
      ];
}

function bad_hex_float(param) {
  throw [
        Scan_failure,
        "not a valid float in hexadecimal notation"
      ];
}

function character_mismatch_err(c, ci) {
  return Curry._2(Printf.sprintf(/* Format */[
                  /* String_literal */Block.__(11, [
                      "looking for ",
                      /* Caml_char */Block.__(1, [/* String_literal */Block.__(11, [
                              ", found ",
                              /* Caml_char */Block.__(1, [/* End_of_format */0])
                            ])])
                    ]),
                  "looking for %C, found %C"
                ]), c, ci);
}

function check_this_char(ib, c) {
  var ci = checked_peek_char(ib);
  if (ci === c) {
    ib[/* ic_current_char_is_valid */2] = false;
    return /* () */0;
  } else {
    var s = character_mismatch_err(c, ci);
    throw [
          Scan_failure,
          s
        ];
  }
}

function check_char(ib, c) {
  if (c !== 10) {
    if (c !== 32) {
      return check_this_char(ib, c);
    } else {
      var ib$1 = ib;
      while(true) {
        var c$1 = peek_char(ib$1);
        if (ib$1[/* ic_eof */0]) {
          return 0;
        } else {
          var switcher = c$1 - 9 | 0;
          if (switcher > 4 || switcher < 0) {
            if (switcher !== 23) {
              return /* () */0;
            } else {
              ib$1[/* ic_current_char_is_valid */2] = false;
              continue ;
            }
          } else if (switcher === 3 || switcher === 2) {
            return /* () */0;
          } else {
            ib$1[/* ic_current_char_is_valid */2] = false;
            continue ;
          }
        }
      };
    }
  } else {
    var ib$2 = ib;
    var ci = checked_peek_char(ib$2);
    if (ci !== 10) {
      if (ci !== 13) {
        var s = character_mismatch_err(/* "\n" */10, ci);
        throw [
              Scan_failure,
              s
            ];
      } else {
        ib$2[/* ic_current_char_is_valid */2] = false;
        return check_this_char(ib$2, /* "\n" */10);
      }
    } else {
      ib$2[/* ic_current_char_is_valid */2] = false;
      return /* () */0;
    }
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
      var s$1 = Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "invalid boolean '",
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* Char_literal */Block.__(12, [
                            /* "'" */39,
                            /* End_of_format */0
                          ])
                      ])
                  ]),
                "invalid boolean '%s'"
              ]), s);
      throw [
            Scan_failure,
            s$1
          ];
  }
}

function integer_conversion_of_char(param) {
  var exit = 0;
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
        exit = 1;
        break;
    case 88 : 
    case 120 : 
        return /* X_conversion */5;
    default:
      exit = 1;
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "scanf.ml",
            555,
            9
          ]
        ];
  }
  
}

function token_int_literal(conv, ib) {
  var tok;
  switch (conv) {
    case 0 : 
        tok = "0b" + token(ib);
        break;
    case 1 : 
    case 2 : 
        tok = token(ib);
        break;
    case 3 : 
        tok = "0o" + token(ib);
        break;
    case 4 : 
        tok = "0u" + token(ib);
        break;
    case 5 : 
        tok = "0x" + token(ib);
        break;
    
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

function scan_decimal_digit_star(_width, ib) {
  while(true) {
    var width = _width;
    if (width === 0) {
      return width;
    } else {
      var c = peek_char(ib);
      if (ib[/* ic_eof */0]) {
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

function scan_decimal_digit_plus(width, ib) {
  if (width === 0) {
    return bad_token_length("decimal digits");
  } else {
    var c = checked_peek_char(ib);
    if (c > 57 || c < 48) {
      var s = Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "character ",
                    /* Caml_char */Block.__(1, [/* String_literal */Block.__(11, [
                            " is not a decimal digit",
                            /* End_of_format */0
                          ])])
                  ]),
                "character %C is not a decimal digit"
              ]), c);
      throw [
            Scan_failure,
            s
          ];
    } else {
      var width$1 = store_char(width, ib, c);
      return scan_decimal_digit_star(width$1, ib);
    }
  }
}

function scan_digit_plus(basis, digitp, width, ib) {
  if (width === 0) {
    return bad_token_length("digits");
  } else {
    var c = checked_peek_char(ib);
    if (Curry._1(digitp, c)) {
      var width$1 = store_char(width, ib, c);
      var digitp$1 = digitp;
      var width$2 = width$1;
      var ib$1 = ib;
      var _width = width$2;
      var ib$2 = ib$1;
      while(true) {
        var width$3 = _width;
        if (width$3 === 0) {
          return width$3;
        } else {
          var c$1 = peek_char(ib$2);
          if (ib$2[/* ic_eof */0]) {
            return width$3;
          } else if (Curry._1(digitp$1, c$1)) {
            var width$4 = store_char(width$3, ib$2, c$1);
            _width = width$4;
            continue ;
          } else if (c$1 !== 95) {
            return width$3;
          } else {
            var width$5 = ignore_char(width$3, ib$2);
            _width = width$5;
            continue ;
          }
        }
      };
    } else {
      var s = Curry._2(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "character ",
                    /* Caml_char */Block.__(1, [/* String_literal */Block.__(11, [
                            " is not a valid ",
                            /* String */Block.__(2, [
                                /* No_padding */0,
                                /* String_literal */Block.__(11, [
                                    " digit",
                                    /* End_of_format */0
                                  ])
                              ])
                          ])])
                  ]),
                "character %C is not a valid %s digit"
              ]), c, basis);
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
  return scan_digit_plus("binary", is_binary_digit, param, param$1);
}

function is_octal_digit(param) {
  return !(param > 55 || param < 48);
}

function scan_octal_int(param, param$1) {
  return scan_digit_plus("octal", is_octal_digit, param, param$1);
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
    case 0 : 
        return scan_binary_int(width, ib);
    case 1 : 
        return scan_optionally_signed_decimal_int(width, ib);
    case 2 : 
        var width$1 = width;
        var ib$1 = ib;
        var width$2 = scan_sign(width$1, ib$1);
        var width$3 = width$2;
        var ib$2 = ib$1;
        var c = checked_peek_char(ib$2);
        if (c !== 48) {
          return scan_decimal_digit_plus(width$3, ib$2);
        } else {
          var width$4 = store_char(width$3, ib$2, c);
          if (width$4 === 0) {
            return width$4;
          } else {
            var c$1 = peek_char(ib$2);
            if (ib$2[/* ic_eof */0]) {
              return width$4;
            } else if (c$1 >= 99) {
              if (c$1 !== 111) {
                if (c$1 !== 120) {
                  return scan_decimal_digit_star(width$4, ib$2);
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
                return scan_decimal_digit_star(width$4, ib$2);
              }
            } else {
              return scan_hexadecimal_int(store_char(width$4, ib$2, c$1), ib$2);
            }
          }
        }
    case 3 : 
        return scan_octal_int(width, ib);
    case 4 : 
        return scan_decimal_digit_plus(width, ib);
    case 5 : 
        return scan_hexadecimal_int(width, ib);
    
  }
}

function scan_fractional_part(width, ib) {
  if (width === 0) {
    return width;
  } else {
    var c = peek_char(ib);
    if (ib[/* ic_eof */0] || c > 57 || c < 48) {
      return width;
    } else {
      return scan_decimal_digit_star(store_char(width, ib, c), ib);
    }
  }
}

function scan_exponent_part(width, ib) {
  if (width === 0) {
    return width;
  } else {
    var c = peek_char(ib);
    if (ib[/* ic_eof */0] || c !== 69 && c !== 101) {
      return width;
    } else {
      return scan_optionally_signed_decimal_int(store_char(width, ib, c), ib);
    }
  }
}

function scan_integer_part(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digit_star(width$1, ib);
}

function scan_float(width, precision, ib) {
  var width$1 = scan_integer_part(width, ib);
  if (width$1 === 0) {
    return /* tuple */[
            width$1,
            precision
          ];
  } else {
    var c = peek_char(ib);
    if (ib[/* ic_eof */0]) {
      return /* tuple */[
              width$1,
              precision
            ];
    } else if (c !== 46) {
      return /* tuple */[
              scan_exponent_part(width$1, ib),
              precision
            ];
    } else {
      var width$2 = store_char(width$1, ib, c);
      var precision$1 = width$2 < precision ? width$2 : precision;
      var width$3 = width$2 - (precision$1 - scan_fractional_part(precision$1, ib) | 0) | 0;
      return /* tuple */[
              scan_exponent_part(width$3, ib),
              precision$1
            ];
    }
  }
}

function check_case_insensitive_string(width, ib, error, str) {
  var lowercase = function (c) {
    if (c > 90 || c < 65) {
      return c;
    } else {
      return Pervasives.char_of_int((c - /* "A" */65 | 0) + /* "a" */97 | 0);
    }
  };
  var len = str.length;
  var width$1 = width;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var c = peek_char(ib);
    if (lowercase(c) !== lowercase(Caml_string.get(str, i))) {
      Curry._1(error, /* () */0);
    }
    if (width$1 === 0) {
      Curry._1(error, /* () */0);
    }
    width$1 = store_char(width$1, ib, c);
  }
  return width$1;
}

function scan_hex_float(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "not a valid float in hexadecimal notation"
        ];
  }
  var width$1 = scan_sign(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "not a valid float in hexadecimal notation"
        ];
  }
  var c = peek_char(ib);
  var exit = 0;
  if (c >= 78) {
    var switcher = c - 79 | 0;
    if (switcher > 30 || switcher < 0) {
      if (switcher >= 32) {
        throw [
              Scan_failure,
              "not a valid float in hexadecimal notation"
            ];
      } else {
        var width$2 = store_char(width$1, ib, c);
        if (width$2 === 0 || end_of_input(ib)) {
          throw [
                Scan_failure,
                "not a valid float in hexadecimal notation"
              ];
        }
        return check_case_insensitive_string(width$2, ib, bad_hex_float, "an");
      }
    } else if (switcher !== 26) {
      throw [
            Scan_failure,
            "not a valid float in hexadecimal notation"
          ];
    } else {
      exit = 1;
    }
  } else if (c !== 48) {
    if (c !== 73) {
      throw [
            Scan_failure,
            "not a valid float in hexadecimal notation"
          ];
    } else {
      exit = 1;
    }
  } else {
    var width$3 = store_char(width$1, ib, c);
    if (width$3 === 0 || end_of_input(ib)) {
      throw [
            Scan_failure,
            "not a valid float in hexadecimal notation"
          ];
    }
    var width$4 = check_case_insensitive_string(width$3, ib, bad_hex_float, "x");
    if (width$4 === 0 || end_of_input(ib)) {
      return width$4;
    } else {
      var match = peek_char(ib);
      var switcher$1 = match - 46 | 0;
      var width$5 = switcher$1 > 34 || switcher$1 < 0 ? (
          switcher$1 !== 66 ? scan_hexadecimal_int(width$4, ib) : width$4
        ) : (
          switcher$1 > 33 || switcher$1 < 1 ? width$4 : scan_hexadecimal_int(width$4, ib)
        );
      if (width$5 === 0 || end_of_input(ib)) {
        return width$5;
      } else {
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
        } else {
          var c$2 = peek_char(ib);
          var exit$1 = 0;
          if (c$2 !== 80 && c$2 !== 112) {
            return width$6;
          } else {
            exit$1 = 2;
          }
          if (exit$1 === 2) {
            var width$8 = store_char(width$6, ib, c$2);
            if (width$8 === 0 || end_of_input(ib)) {
              throw [
                    Scan_failure,
                    "not a valid float in hexadecimal notation"
                  ];
            }
            return scan_optionally_signed_decimal_int(width$8, ib);
          }
          
        }
      }
    }
  }
  if (exit === 1) {
    var width$9 = store_char(width$1, ib, c);
    if (width$9 === 0 || end_of_input(ib)) {
      throw [
            Scan_failure,
            "not a valid float in hexadecimal notation"
          ];
    }
    return check_case_insensitive_string(width$9, ib, bad_hex_float, "nfinity");
  }
  
}

function scan_caml_float_rest(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  }
  var width$1 = scan_decimal_digit_star(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  }
  var c = peek_char(ib);
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
      var width_precision = scan_fractional_part(precision$1, ib);
      var frac_width = precision$1 - width_precision | 0;
      var width$3 = width$2 - frac_width | 0;
      return scan_exponent_part(width$3, ib);
    }
  } else if (switcher > 31 || switcher < 1) {
    return scan_exponent_part(width$1, ib);
  } else {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  }
}

function scan_caml_float(width, precision, ib) {
  if (width === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  }
  var width$1 = scan_sign(width, ib);
  if (width$1 === 0 || end_of_input(ib)) {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
  }
  var c = peek_char(ib);
  if (c >= 49) {
    if (c >= 58) {
      throw [
            Scan_failure,
            "no dot or exponent part found in float token"
          ];
    } else {
      var width$2 = store_char(width$1, ib, c);
      if (width$2 === 0 || end_of_input(ib)) {
        throw [
              Scan_failure,
              "no dot or exponent part found in float token"
            ];
      }
      return scan_caml_float_rest(width$2, precision, ib);
    }
  } else if (c >= 48) {
    var width$3 = store_char(width$1, ib, c);
    if (width$3 === 0 || end_of_input(ib)) {
      throw [
            Scan_failure,
            "no dot or exponent part found in float token"
          ];
    }
    var c$1 = peek_char(ib);
    var exit = 0;
    if (c$1 !== 88 && c$1 !== 120) {
      return scan_caml_float_rest(width$3, precision, ib);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var width$4 = store_char(width$3, ib, c$1);
      if (width$4 === 0 || end_of_input(ib)) {
        throw [
              Scan_failure,
              "no dot or exponent part found in float token"
            ];
      }
      var width$5 = scan_hexadecimal_int(width$4, ib);
      if (width$5 === 0 || end_of_input(ib)) {
        throw [
              Scan_failure,
              "no dot or exponent part found in float token"
            ];
      }
      var c$2 = peek_char(ib);
      var switcher = c$2 - 80 | 0;
      var width$6;
      if (switcher > 32 || switcher < 0) {
        if (switcher !== -34) {
          throw [
                Scan_failure,
                "no dot or exponent part found in float token"
              ];
        } else {
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
        }
      } else if (switcher > 31 || switcher < 1) {
        width$6 = width$5;
      } else {
        throw [
              Scan_failure,
              "no dot or exponent part found in float token"
            ];
      }
      if (width$6 === 0 || end_of_input(ib)) {
        return width$6;
      } else {
        var c$3 = peek_char(ib);
        var exit$1 = 0;
        if (c$3 !== 80 && c$3 !== 112) {
          return width$6;
        } else {
          exit$1 = 2;
        }
        if (exit$1 === 2) {
          var width$8 = store_char(width$6, ib, c$3);
          if (width$8 === 0 || end_of_input(ib)) {
            throw [
                  Scan_failure,
                  "not a valid float in hexadecimal notation"
                ];
          }
          return scan_optionally_signed_decimal_int(width$8, ib);
        }
        
      }
    }
    
  } else {
    throw [
          Scan_failure,
          "no dot or exponent part found in float token"
        ];
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
      if (ib[/* ic_eof */0]) {
        return width$1;
      } else if (stp !== undefined) {
        if (c === stp) {
          ib[/* ic_current_char_is_valid */2] = false;
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
    var s = Curry._3(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "bad character decimal encoding \\",
                  /* Char */Block.__(0, [/* Char */Block.__(0, [/* Char */Block.__(0, [/* End_of_format */0])])])
                ]),
              "bad character decimal encoding \\%c%c%c"
            ]), c0, c1, c2);
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
    var s = Curry._2(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "bad character hexadecimal encoding \\",
                  /* Char */Block.__(0, [/* Char */Block.__(0, [/* End_of_format */0])])
                ]),
              "bad character hexadecimal encoding \\%c%c"
            ]), c1, c2);
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
    if (ib[/* ic_eof */0]) {
      var message$1 = message;
      var s = Curry._1(Printf.sprintf(/* Format */[
                /* String_literal */Block.__(11, [
                    "scanning of ",
                    /* String */Block.__(2, [
                        /* No_padding */0,
                        /* String_literal */Block.__(11, [
                            " failed: premature end of file occurred before end of token",
                            /* End_of_format */0
                          ])
                      ])
                  ]),
                "scanning of %s failed: premature end of file occurred before end of token"
              ]), message$1);
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
  var exit = 0;
  if (c >= 40) {
    if (c >= 58) {
      switch (c) {
        case 92 : 
        case 98 : 
        case 110 : 
        case 114 : 
        case 116 : 
            exit = 1;
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
  } else {
    exit = 1;
  }
  if (exit === 1) {
    return store_char(width, ib, char_for_backslash(c));
  }
  
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

function scan_chars_in_char_set(char_set, scan_indic, width, ib) {
  var scan_chars = function (_i, stp) {
    while(true) {
      var i = _i;
      var c = peek_char(ib);
      if (i > 0 && !ib[/* ic_eof */0] && CamlinternalFormat.is_in_char_set(char_set, c) && c !== stp) {
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
    if (ib[/* ic_eof */0]) {
      return 0;
    } else {
      var ci = peek_char(ib);
      if (c === ci) {
        ib[/* ic_current_char_is_valid */2] = false;
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
  var exit = 0;
  var s;
  if (x[0] === Scan_failure || x[0] === Caml_builtin_exceptions.failure) {
    s = x[1];
    exit = 1;
  } else {
    throw x;
  }
  if (exit === 1) {
    var i = char_count(ib);
    var s$1 = Curry._2(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "scanf: bad input at char number ",
                  /* Int */Block.__(4, [
                      /* Int_i */3,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* String_literal */Block.__(11, [
                          ": ",
                          /* String */Block.__(2, [
                              /* No_padding */0,
                              /* End_of_format */0
                            ])
                        ])
                    ])
                ]),
              "scanf: bad input at char number %i: %s"
            ]), i, s);
    throw [
          Scan_failure,
          s$1
        ];
  }
  
}

function get_counter(ib, counter) {
  switch (counter) {
    case 0 : 
        return ib[/* ic_line_count */4];
    case 1 : 
        return char_count(ib);
    case 2 : 
        return ib[/* ic_token_count */5];
    
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
  if (fmting === /* Escaped_percent */6) {
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
    if (typeof fmt === "number") {
      return Curry._1(k, /* Nil */0);
    } else {
      switch (fmt.tag | 0) {
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 8 : 
            _fmt = fmt[3];
            continue ;
        case 14 : 
            return take_fmtty_format_readers(k, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmt[1])), fmt[2]);
        case 18 : 
            _fmt = CamlinternalFormatBasics.concat_fmt(fmt[0][0][0], fmt[1]);
            continue ;
        case 19 : 
            var fmt_rest = fmt[0];
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* Cons */[
                            reader,
                            readers_rest
                          ]);
              };
              return take_format_readers(new_k, fmt_rest);
            }
            }(fmt_rest));
        case 0 : 
        case 1 : 
        case 10 : 
        case 15 : 
        case 16 : 
        case 22 : 
            _fmt = fmt[0];
            continue ;
        case 23 : 
            var k$1 = k;
            var ign = fmt[0];
            var fmt$1 = fmt[1];
            if (typeof ign === "number") {
              if (ign === 2) {
                return (function(k$1,fmt$1){
                return function (reader) {
                  var new_k = function (readers_rest) {
                    return Curry._1(k$1, /* Cons */[
                                reader,
                                readers_rest
                              ]);
                  };
                  return take_format_readers(new_k, fmt$1);
                }
                }(k$1,fmt$1));
              } else {
                return take_format_readers(k$1, fmt$1);
              }
            } else if (ign.tag === 9) {
              return take_fmtty_format_readers(k$1, ign[1], fmt$1);
            } else {
              return take_format_readers(k$1, fmt$1);
            }
        case 13 : 
        case 20 : 
        case 24 : 
            _fmt = fmt[2];
            continue ;
        default:
          _fmt = fmt[1];
          continue ;
      }
    }
  };
}

function take_fmtty_format_readers(k, _fmtty, fmt) {
  while(true) {
    var fmtty = _fmtty;
    if (typeof fmtty === "number") {
      return take_format_readers(k, fmt);
    } else {
      switch (fmtty.tag | 0) {
        case 8 : 
            _fmtty = fmtty[1];
            continue ;
        case 9 : 
            var ty = CamlinternalFormat.trans(CamlinternalFormat.symm(fmtty[0]), fmtty[1]);
            _fmtty = CamlinternalFormatBasics.concat_fmtty(ty, fmtty[2]);
            continue ;
        case 13 : 
            var fmt_rest = fmtty[0];
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* Cons */[
                            reader,
                            readers_rest
                          ]);
              };
              return take_fmtty_format_readers(new_k, fmt_rest, fmt);
            }
            }(fmt_rest));
        case 14 : 
            var fmt_rest$1 = fmtty[0];
            return (function(fmt_rest$1){
            return function (reader) {
              var new_k = function (readers_rest) {
                return Curry._1(k, /* Cons */[
                            reader,
                            readers_rest
                          ]);
              };
              return take_fmtty_format_readers(new_k, fmt_rest$1, fmt);
            }
            }(fmt_rest$1));
        default:
          _fmtty = fmtty[0];
          continue ;
      }
    }
  };
}

function make_scanf(ib, _fmt, readers) {
  while(true) {
    var fmt = _fmt;
    if (typeof fmt === "number") {
      return /* Nil */0;
    } else {
      switch (fmt.tag | 0) {
        case 0 : 
            scan_char(0, ib);
            var c = Caml_string.get(token(ib), 0);
            return /* Cons */[
                    c,
                    make_scanf(ib, fmt[0], readers)
                  ];
        case 1 : 
            scan_caml_char(0, ib);
            var c$1 = Caml_string.get(token(ib), 0);
            return /* Cons */[
                    c$1,
                    make_scanf(ib, fmt[0], readers)
                  ];
        case 2 : 
            var rest = fmt[1];
            var pad = fmt[0];
            var exit = 0;
            if (typeof rest === "number") {
              exit = 1;
            } else {
              switch (rest.tag | 0) {
                case 17 : 
                    var match = stopper_of_formatting_lit(rest[0]);
                    var stp = match[0];
                    var scan = (function(stp){
                    return function scan(width, param, ib) {
                      return scan_string(stp, width, ib);
                    }
                    }(stp));
                    var str_rest_000 = match[1];
                    var str_rest_001 = rest[1];
                    var str_rest = /* String_literal */Block.__(11, [
                        str_rest_000,
                        str_rest_001
                      ]);
                    return pad_prec_scanf(ib, str_rest, readers, pad, /* No_precision */0, scan, token);
                case 18 : 
                    var match$1 = rest[0];
                    if (match$1.tag) {
                      var scan$1 = function (width, param, ib) {
                        return scan_string(/* "[" */91, width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1[0][0], rest[1]), readers, pad, /* No_precision */0, scan$1, token);
                    } else {
                      var scan$2 = function (width, param, ib) {
                        return scan_string(/* "{" */123, width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1[0][0], rest[1]), readers, pad, /* No_precision */0, scan$2, token);
                    }
                default:
                  exit = 1;
              }
            }
            if (exit === 1) {
              var scan$3 = function (width, param, ib) {
                return scan_string(undefined, width, ib);
              };
              return pad_prec_scanf(ib, rest, readers, pad, /* No_precision */0, scan$3, token);
            }
            break;
        case 3 : 
            var scan$4 = function (width, param, ib) {
              return scan_caml_string(width, ib);
            };
            return pad_prec_scanf(ib, fmt[1], readers, fmt[0], /* No_precision */0, scan$4, token);
        case 4 : 
            var c$2 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt[0]));
            var scan$5 = (function(c$2){
            return function scan$5(width, param, ib) {
              return scan_int_conversion(c$2, width, ib);
            }
            }(c$2));
            return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan$5, (function(c$2){
                      return function (param) {
                        return Caml_format.caml_int_of_string(token_int_literal(c$2, param));
                      }
                      }(c$2)));
        case 5 : 
            var c$3 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt[0]));
            var scan$6 = (function(c$3){
            return function scan$6(width, param, ib) {
              return scan_int_conversion(c$3, width, ib);
            }
            }(c$3));
            return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan$6, (function(c$3){
                      return function (param) {
                        return Caml_format.caml_int32_of_string(token_int_literal(c$3, param));
                      }
                      }(c$3)));
        case 6 : 
            var c$4 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt[0]));
            var scan$7 = (function(c$4){
            return function scan$7(width, param, ib) {
              return scan_int_conversion(c$4, width, ib);
            }
            }(c$4));
            return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan$7, (function(c$4){
                      return function (param) {
                        return Caml_format.caml_nativeint_of_string(token_int_literal(c$4, param));
                      }
                      }(c$4)));
        case 7 : 
            var c$5 = integer_conversion_of_char(CamlinternalFormat.char_of_iconv(fmt[0]));
            var scan$8 = (function(c$5){
            return function scan$8(width, param, ib) {
              return scan_int_conversion(c$5, width, ib);
            }
            }(c$5));
            return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan$8, (function(c$5){
                      return function (param) {
                        return Caml_format.caml_int64_of_string(token_int_literal(c$5, param));
                      }
                      }(c$5)));
        case 8 : 
            var match$2 = fmt[0];
            if (match$2 !== 15) {
              if (match$2 >= 16) {
                return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan_hex_float, token_float);
              } else {
                return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan_float, token_float);
              }
            } else {
              return pad_prec_scanf(ib, fmt[3], readers, fmt[1], fmt[2], scan_caml_float, token_float);
            }
        case 9 : 
            var scan$9 = function (param, param$1, ib) {
              var ib$1 = ib;
              var c = checked_peek_char(ib$1);
              var m;
              if (c !== 102) {
                if (c !== 116) {
                  var s = Curry._1(Printf.sprintf(/* Format */[
                            /* String_literal */Block.__(11, [
                                "the character ",
                                /* Caml_char */Block.__(1, [/* String_literal */Block.__(11, [
                                        " cannot start a boolean",
                                        /* End_of_format */0
                                      ])])
                              ]),
                            "the character %C cannot start a boolean"
                          ]), c);
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
              return scan_string(undefined, m, ib$1);
            };
            return pad_prec_scanf(ib, fmt[1], readers, fmt[0], /* No_precision */0, scan$9, token_bool);
        case 10 : 
            if (end_of_input(ib)) {
              _fmt = fmt[0];
              continue ;
            } else {
              throw [
                    Scan_failure,
                    "end of input not found"
                  ];
            }
        case 11 : 
            $$String.iter((function (param) {
                    return check_char(ib, param);
                  }), fmt[0]);
            _fmt = fmt[1];
            continue ;
        case 12 : 
            check_char(ib, fmt[0]);
            _fmt = fmt[1];
            continue ;
        case 13 : 
            scan_caml_string(width_of_pad_opt(fmt[0]), ib);
            var s = token(ib);
            var fmt$1;
            try {
              fmt$1 = CamlinternalFormat.format_of_string_fmtty(s, fmt[1]);
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
            return /* Cons */[
                    fmt$1,
                    make_scanf(ib, fmt[2], readers)
                  ];
        case 14 : 
            var fmtty = fmt[1];
            scan_caml_string(width_of_pad_opt(fmt[0]), ib);
            var s$1 = token(ib);
            var match$3;
            try {
              var match$4 = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
              var match$5 = CamlinternalFormat.fmt_ebb_of_string(undefined, s$1);
              match$3 = /* tuple */[
                CamlinternalFormat.type_format(match$4[0], CamlinternalFormatBasics.erase_rel(fmtty)),
                CamlinternalFormat.type_format(match$5[0], CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmtty)))
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
            return /* Cons */[
                    /* Format */[
                      match$3[0],
                      s$1
                    ],
                    make_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$3[1], fmt[2]), readers)
                  ];
        case 15 : 
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%a\""
                ];
        case 16 : 
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%t\""
                ];
        case 17 : 
            $$String.iter((function (param) {
                    return check_char(ib, param);
                  }), CamlinternalFormat.string_of_formatting_lit(fmt[0]));
            _fmt = fmt[1];
            continue ;
        case 18 : 
            var match$6 = fmt[0];
            check_char(ib, /* "@" */64);
            if (match$6.tag) {
              check_char(ib, /* "[" */91);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$6[0][0], fmt[1]);
              continue ;
            } else {
              check_char(ib, /* "{" */123);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$6[0][0], fmt[1]);
              continue ;
            }
        case 19 : 
            if (readers) {
              var x = Curry._1(readers[0], ib);
              return /* Cons */[
                      x,
                      make_scanf(ib, fmt[0], readers[1])
                    ];
            } else {
              throw [
                    Caml_builtin_exceptions.invalid_argument,
                    "scanf: missing reader"
                  ];
            }
        case 20 : 
            var rest$1 = fmt[2];
            var char_set = fmt[1];
            var width_opt = fmt[0];
            var exit$1 = 0;
            if (typeof rest$1 === "number" || rest$1.tag !== 17) {
              exit$1 = 1;
            } else {
              var match$7 = stopper_of_formatting_lit(rest$1[0]);
              var width = width_of_pad_opt(width_opt);
              scan_chars_in_char_set(char_set, match$7[0], width, ib);
              var s$2 = token(ib);
              var str_rest_000$1 = match$7[1];
              var str_rest_001$1 = rest$1[1];
              var str_rest$1 = /* String_literal */Block.__(11, [
                  str_rest_000$1,
                  str_rest_001$1
                ]);
              return /* Cons */[
                      s$2,
                      make_scanf(ib, str_rest$1, readers)
                    ];
            }
            if (exit$1 === 1) {
              var width$1 = width_of_pad_opt(width_opt);
              scan_chars_in_char_set(char_set, undefined, width$1, ib);
              var s$3 = token(ib);
              return /* Cons */[
                      s$3,
                      make_scanf(ib, rest$1, readers)
                    ];
            }
            break;
        case 21 : 
            var count = get_counter(ib, fmt[0]);
            return /* Cons */[
                    count,
                    make_scanf(ib, fmt[1], readers)
                  ];
        case 22 : 
            var c$6 = checked_peek_char(ib);
            return /* Cons */[
                    c$6,
                    make_scanf(ib, fmt[0], readers)
                  ];
        case 23 : 
            var match$8 = CamlinternalFormat.param_format_of_ignored_format(fmt[0], fmt[1]);
            var match$9 = make_scanf(ib, match$8[0], readers);
            if (match$9) {
              return match$9[1];
            } else {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "scanf.ml",
                      1455,
                      13
                    ]
                  ];
            }
        case 24 : 
            throw [
                  Caml_builtin_exceptions.invalid_argument,
                  "scanf: bad conversion \"%?\" (custom converter)"
                ];
        
      }
    }
  };
}

function pad_prec_scanf(ib, fmt, readers, pad, prec, scan, token) {
  if (typeof pad === "number") {
    if (typeof prec === "number") {
      if (prec !== 0) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "scanf: bad conversion \"%*\""
            ];
      }
      Curry._3(scan, Pervasives.max_int, Pervasives.max_int, ib);
      var x = Curry._1(token, ib);
      return /* Cons */[
              x,
              make_scanf(ib, fmt, readers)
            ];
    } else {
      Curry._3(scan, Pervasives.max_int, prec[0], ib);
      var x$1 = Curry._1(token, ib);
      return /* Cons */[
              x$1,
              make_scanf(ib, fmt, readers)
            ];
    }
  } else if (pad.tag) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "scanf: bad conversion \"%*\""
        ];
  } else if (pad[0] !== 0) {
    var w = pad[1];
    if (typeof prec === "number") {
      if (prec !== 0) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "scanf: bad conversion \"%*\""
            ];
      }
      Curry._3(scan, w, Pervasives.max_int, ib);
      var x$2 = Curry._1(token, ib);
      return /* Cons */[
              x$2,
              make_scanf(ib, fmt, readers)
            ];
    } else {
      Curry._3(scan, w, prec[0], ib);
      var x$3 = Curry._1(token, ib);
      return /* Cons */[
              x$3,
              make_scanf(ib, fmt, readers)
            ];
    }
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "scanf: bad conversion \"%-\""
        ];
  }
}

function kscanf(ib, ef, param) {
  var str = param[1];
  var fmt = param[0];
  var k = function (readers, f) {
    $$Buffer.reset(ib[/* ic_token_buffer */7]);
    var match;
    try {
      match = /* Args */Block.__(0, [make_scanf(ib, fmt, readers)]);
    }
    catch (raw_exc){
      var exc = Caml_js_exceptions.internalToOCamlException(raw_exc);
      if (exc[0] === Scan_failure || exc[0] === Caml_builtin_exceptions.failure || exc === Caml_builtin_exceptions.end_of_file) {
        match = /* Exc */Block.__(1, [exc]);
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
    if (match.tag) {
      return Curry._2(ef, ib, match[0]);
    } else {
      var _f = f;
      var _args = match[0];
      while(true) {
        var args = _args;
        var f$1 = _f;
        if (args) {
          _args = args[1];
          _f = Curry._1(f$1, args[0]);
          continue ;
        } else {
          return f$1;
        }
      };
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
  return Curry._1(sscanf("\"" + (s + "\""), /* Format */[
                  /* Caml_string */Block.__(3, [
                      /* No_padding */0,
                      /* Flush */Block.__(10, [/* End_of_format */0])
                    ]),
                  "%S%!"
                ]), (function (x) {
                return x;
              }));
}

function kfscanf(ic, ef, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), ef, fmt);
}

function fscanf(ic, fmt) {
  return kscanf(memo_from_ic(scan_raise_at_end, ic), scanf_bad_input, fmt);
}

var Scanning = [
  stdin,
  open_in,
  open_in_bin,
  close_in,
  open_in,
  open_in_bin,
  from_string,
  from_function,
  from_channel,
  end_of_input,
  beginning_of_input,
  name_of_input,
  stdin
];

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
