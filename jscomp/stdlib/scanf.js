// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Pervasives = require("./pervasives");
var Caml_format = require("../runtime/caml_format");
var Printf = require("./printf");
var Caml_primitive = require("../runtime/caml_primitive");
var CamlinternalFormatBasics = require("./camlinternalFormatBasics");
var Buffer = require("./buffer");
var $$String = require("./string");
var Caml_string = require("../runtime/caml_string");
var List = require("./list");
var CamlinternalFormat = require("./camlinternalFormat");

var null_char = /* "\000" */0;

function next_char(ib) {
  try {
    var c = ib[7](/* () */0);
    ib[2] = c;
    ib[3] = /* true */1;
    ++ ib[4];
    if (c === /* "\n" */10) {
      ++ ib[5];
    }
    return c;
  }
  catch (exn){
    if (exn === Caml_exceptions.End_of_file) {
      ib[2] = null_char;
      ib[3] = /* false */0;
      ib[1] = /* true */1;
      return null_char;
    }
    else {
      throw exn;
    }
  }
}

function peek_char(ib) {
  if (ib[3]) {
    return ib[2];
  }
  else {
    return next_char(ib);
  }
}

function checked_peek_char(ib) {
  var c = peek_char(ib);
  if (ib[1]) {
    throw Caml_exceptions.End_of_file;
  }
  return c;
}

function end_of_input(ib) {
  peek_char(ib);
  return ib[1];
}

function eof(ib) {
  return ib[1];
}

function beginning_of_input(ib) {
  return +(ib[4] === 0);
}

function name_of_input(ib) {
  var match = ib[9];
  if (typeof match === "number") {
    if (match) {
      return "unnamed function";
    }
    else {
      return "unnamed character string";
    }
  }
  else {
    if (match[0]) {
      return "unnamed pervasives input channel";
    }
    else {
      return match[1];
    }
  }
}

function char_count(ib) {
  if (ib[3]) {
    return ib[4] - 1;
  }
  else {
    return ib[4];
  }
}

function reset_token(ib) {
  return Buffer.reset(ib[8]);
}

function invalidate_current_char(ib) {
  ib[3] = /* false */0;
  return /* () */0;
}

function token(ib) {
  var tokbuf = ib[8];
  var tok = Buffer.contents(tokbuf);
  Buffer.clear(tokbuf);
  ++ ib[6];
  return tok;
}

function skip_char(width, ib) {
  invalidate_current_char(ib);
  return width;
}

function ignore_char(width, ib) {
  return skip_char(width - 1, ib);
}

function store_char(width, ib, c) {
  Buffer.add_char(ib[8], c);
  return ignore_char(width, ib);
}

function create(iname, next) {
  return [
          /* record */0,
          /* false */0,
          null_char,
          /* false */0,
          0,
          0,
          0,
          next,
          Buffer.create(1024),
          iname
        ];
}

function from_string(s) {
  var i = [
    0,
    0
  ];
  var len = s.length;
  var next = function () {
    if (i[1] >= len) {
      throw Caml_exceptions.End_of_file;
    }
    else {
      var c = s.charCodeAt(i[1]);
      ++ i[1];
      return c;
    }
  };
  return create(/* From_string */0, next);
}

function from_function(param) {
  return create(/* From_function */1, param);
}

var file_buffer_size = [
  0,
  1024
];

function scan_close_at_end(ic) {
  Pervasives.close_in(ic);
  throw Caml_exceptions.End_of_file;
}

function scan_raise_at_end() {
  throw Caml_exceptions.End_of_file;
}

function from_ic(scan_close_ic, iname, ic) {
  var len = file_buffer_size[1];
  var buf = Caml_string.caml_create_string(len);
  var i = [
    0,
    0
  ];
  var lim = [
    0,
    0
  ];
  var eof = [
    0,
    /* false */0
  ];
  var next = function () {
    if (i[1] < lim[1]) {
      var c = buf[i[1]];
      ++ i[1];
      return c;
    }
    else {
      if (eof[1]) {
        throw Caml_exceptions.End_of_file;
      }
      else {
        lim[1] = Pervasives.input(ic, buf, 0, len);
        if (lim[1]) {
          i[1] = 1;
          return buf[0];
        }
        else {
          eof[1] = /* true */1;
          return scan_close_ic(ic);
        }
      }
    }
  };
  return create(iname, next);
}

function from_ic_close_at_end(param, param$1) {
  return from_ic(scan_close_at_end, param, param$1);
}

var stdin = from_ic(scan_raise_at_end, [
      /* From_file */0,
      "-",
      Pervasives.stdin
    ], Pervasives.stdin);

function open_in(fname) {
  if (fname === "-") {
    return stdin;
  }
  else {
    var ic = Pervasives.open_in(fname);
    return from_ic_close_at_end([
                /* From_file */0,
                fname,
                ic
              ], ic);
  }
}

function open_in_bin(fname) {
  if (fname === "-") {
    return stdin;
  }
  else {
    var ic = Pervasives.open_in_bin(fname);
    return from_ic_close_at_end([
                /* From_file */0,
                fname,
                ic
              ], ic);
  }
}

var memo = [
  0,
  /* [] */0
];

function from_channel(param) {
  var scan_close_ic = scan_raise_at_end;
  var ic = param;
  try {
    return List.assq(ic, memo[1]);
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      var ib = from_ic(scan_close_ic, [
            /* From_channel */1,
            ic
          ], ic);
      memo[1] = [
        /* :: */0,
        [
          /* tuple */0,
          ic,
          ib
        ],
        memo[1]
      ];
      return ib;
    }
    else {
      throw exn;
    }
  }
}

function close_in(ib) {
  var match = ib[9];
  if (typeof match === "number") {
    return /* () */0;
  }
  else {
    if (match[0]) {
      return Pervasives.close_in(match[1]);
    }
    else {
      return Pervasives.close_in(match[2]);
    }
  }
}

var Scan_failure = [
  248,
  "Scanf.Scan_failure",
  ++ Caml_exceptions.caml_oo_last_id
];

function bad_input(s) {
  throw [
        0,
        Scan_failure,
        s
      ];
}

function bad_input_escape(c) {
  return bad_input(Printf.sprintf([
                    /* Format */0,
                    [
                      /* String_literal */11,
                      "illegal escape character ",
                      [
                        /* Caml_char */1,
                        /* End_of_format */0
                      ]
                    ],
                    "illegal escape character %C"
                  ])(c));
}

function bad_token_length(message) {
  return bad_input(Printf.sprintf([
                    /* Format */0,
                    [
                      /* String_literal */11,
                      "scanning of ",
                      [
                        /* String */2,
                        /* No_padding */0,
                        [
                          /* String_literal */11,
                          " failed: the specified length was too short for token",
                          /* End_of_format */0
                        ]
                      ]
                    ],
                    "scanning of %s failed: the specified length was too short for token"
                  ])(message));
}

function bad_float() {
  return bad_input("no dot or exponent part found in float token");
}

function character_mismatch_err(c, ci) {
  return Printf.sprintf([
                /* Format */0,
                [
                  /* String_literal */11,
                  "looking for ",
                  [
                    /* Caml_char */1,
                    [
                      /* String_literal */11,
                      ", found ",
                      [
                        /* Caml_char */1,
                        /* End_of_format */0
                      ]
                    ]
                  ]
                ],
                "looking for %C, found %C"
              ])(c, ci);
}

function character_mismatch(c, ci) {
  return bad_input(character_mismatch_err(c, ci));
}

function check_char(ib, _c) {
  while(/* true */1) {
    var c = _c;
    if (c === /* " " */32) {
      var ib$1 = ib;
      while(/* true */1) {
        var c$1 = peek_char(ib$1);
        if (!eof(ib$1)) {
          var switcher = -9 + c$1;
          if (!(4 < (switcher >>> 0))) {
            if (1 < (-2 + switcher >>> 0)) {
              return invalidate_current_char(ib$1);
            }
            else {
              return /* () */0;
            }
          }
          else {
            if (switcher !== 23) {
              return /* () */0;
            }
            else {
              return invalidate_current_char(ib$1);
            }
          }
        }
        else {
          return 0;
        }
      };
    }
    else {
      var ci = checked_peek_char(ib);
      if (ci === c) {
        return invalidate_current_char(ib);
      }
      else {
        if (ci !== 13) {
          return character_mismatch(c, ci);
        }
        else {
          if (c === /* "\n" */10) {
            invalidate_current_char(ib);
            _c = /* "\n" */10;
          }
          else {
            return character_mismatch(c, ci);
          }
        }
      }
    }
  };
}

function token_char(ib) {
  return token(ib).charCodeAt(0);
}

function token_bool(ib) {
  var s = token(ib);
  switch (s) {
    case "false" : 
        return /* false */0;
    case "true" : 
        return /* true */1;
    default:
      return bad_input(Printf.sprintf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "invalid boolean ",
                          [
                            /* Caml_string */3,
                            /* No_padding */0,
                            /* End_of_format */0
                          ]
                        ],
                        "invalid boolean %S"
                      ])(s));
  }
}

function token_int_literal(conv, ib) {
  var tok;
  var exit = 0;
  var switcher = -88 + conv;
  if (32 < (switcher >>> 0)) {
    exit = 1;
  }
  else {
    switch (switcher) {
      case 10 : 
          tok = "0b" + token(ib);
          break;
      case 23 : 
          tok = "0o" + token(ib);
          break;
      case 12 : 
      case 17 : 
      case 29 : 
          exit = 2;
          break;
      case 1 : 
      case 2 : 
      case 3 : 
      case 4 : 
      case 5 : 
      case 6 : 
      case 7 : 
      case 8 : 
      case 9 : 
      case 11 : 
      case 13 : 
      case 14 : 
      case 15 : 
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
      case 0 : 
      case 32 : 
          exit = 3;
          break;
      
    }
  }
  switch (exit) {
    case 1 : 
        throw [
              0,
              Caml_exceptions.Assert_failure,
              [
                0,
                "scanf.ml",
                507,
                11
              ]
            ];
    case 2 : 
        tok = token(ib);
        break;
    case 3 : 
        tok = "0x" + token(ib);
        break;
    
  }
  var l = tok.length;
  if (l === 0 || tok.charCodeAt(0) !== /* "+" */43) {
    return tok;
  }
  else {
    return $$String.sub(tok, 1, l - 1);
  }
}

function token_float(ib) {
  return Caml_format.caml_float_of_string(token(ib));
}

function scan_decimal_digits(_width, ib) {
  while(/* true */1) {
    var width = _width;
    if (width) {
      var c = peek_char(ib);
      if (eof(ib)) {
        return width;
      }
      else {
        if (c >= 58) {
          if (c !== 95) {
            return width;
          }
          else {
            var width$1 = ignore_char(width, ib);
            _width = width$1;
          }
        }
        else {
          if (c >= 48) {
            var width$2 = store_char(width, ib, c);
            _width = width$2;
          }
          else {
            return width;
          }
        }
      }
    }
    else {
      return width;
    }
  };
}

function scan_decimal_digits_plus(width, ib) {
  if (width) {
    var c = checked_peek_char(ib);
    if (9 < (-48 + c >>> 0)) {
      return bad_input(Printf.sprintf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "character ",
                          [
                            /* Caml_char */1,
                            [
                              /* String_literal */11,
                              " is not a decimal digit",
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "character %C is not a decimal digit"
                      ])(c));
    }
    else {
      var width$1 = store_char(width, ib, c);
      return scan_decimal_digits(width$1, ib);
    }
  }
  else {
    return bad_token_length("decimal digits");
  }
}

function scan_digits_plus(basis, digitp, width, ib) {
  if (width) {
    var c = checked_peek_char(ib);
    if (digitp(c)) {
      var _width = store_char(width, ib, c);
      while(/* true */1) {
        var width$1 = _width;
        if (width$1) {
          var c$1 = peek_char(ib);
          if (eof(ib)) {
            return width$1;
          }
          else {
            if (digitp(c$1)) {
              _width = store_char(width$1, ib, c$1);
            }
            else {
              if (c$1 !== 95) {
                return width$1;
              }
              else {
                _width = ignore_char(width$1, ib);
              }
            }
          }
        }
        else {
          return width$1;
        }
      };
    }
    else {
      return bad_input(Printf.sprintf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "character ",
                          [
                            /* Caml_char */1,
                            [
                              /* String_literal */11,
                              " is not a valid ",
                              [
                                /* String */2,
                                /* No_padding */0,
                                [
                                  /* String_literal */11,
                                  " digit",
                                  /* End_of_format */0
                                ]
                              ]
                            ]
                          ]
                        ],
                        "character %C is not a valid %s digit"
                      ])(c, basis));
    }
  }
  else {
    return bad_token_length("digits");
  }
}

function is_binary_digit(param) {
  if (1 < (-48 + param >>> 0)) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function scan_binary_int(param, param$1) {
  return scan_digits_plus("binary", is_binary_digit, param, param$1);
}

function is_octal_digit(param) {
  if (7 < (-48 + param >>> 0)) {
    return /* false */0;
  }
  else {
    return /* true */1;
  }
}

function scan_octal_int(param, param$1) {
  return scan_digits_plus("octal", is_octal_digit, param, param$1);
}

function is_hexa_digit(param) {
  var switcher = -48 + param;
  if (22 < (switcher >>> 0)) {
    if (5 < (-49 + switcher >>> 0)) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  }
  else {
    if (6 < (-10 + switcher >>> 0)) {
      return /* true */1;
    }
    else {
      return /* false */0;
    }
  }
}

function scan_hexadecimal_int(param, param$1) {
  return scan_digits_plus("hexadecimal", is_hexa_digit, param, param$1);
}

function scan_sign(width, ib) {
  var c = checked_peek_char(ib);
  var switcher = -43 + c;
  if (2 < (switcher >>> 0)) {
    return width;
  }
  else {
    switch (switcher) {
      case 1 : 
          return width;
      case 0 : 
      case 2 : 
          return store_char(width, ib, c);
      
    }
  }
}

function scan_optionally_signed_decimal_int(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digits_plus(width$1, ib);
}

function scan_int_conv(conv, width, ib) {
  var exit = 0;
  var switcher = -88 + conv;
  if (32 < (switcher >>> 0)) {
    exit = 1;
  }
  else {
    switch (switcher) {
      case 10 : 
          return scan_binary_int(width, ib);
      case 12 : 
          return scan_optionally_signed_decimal_int(width, ib);
      case 17 : 
          var width$1 = width;
          var ib$1 = ib;
          var width$2 = scan_sign(width$1, ib$1);
          var width$3 = width$2;
          var ib$2 = ib$1;
          var c = checked_peek_char(ib$2);
          if (c !== 48) {
            return scan_decimal_digits_plus(width$3, ib$2);
          }
          else {
            var width$4 = store_char(width$3, ib$2, c);
            if (width$4) {
              var c$1 = peek_char(ib$2);
              if (eof(ib$2)) {
                return width$4;
              }
              else {
                var exit$1 = 0;
                if (c$1 >= 99) {
                  if (c$1 !== 111) {
                    if (c$1 !== 120) {
                      return scan_decimal_digits(width$4, ib$2);
                    }
                    else {
                      exit$1 = 1;
                    }
                  }
                  else {
                    return scan_octal_int(store_char(width$4, ib$2, c$1), ib$2);
                  }
                }
                else {
                  if (c$1 !== 88) {
                    if (c$1 >= 98) {
                      return scan_binary_int(store_char(width$4, ib$2, c$1), ib$2);
                    }
                    else {
                      return scan_decimal_digits(width$4, ib$2);
                    }
                  }
                  else {
                    exit$1 = 1;
                  }
                }
                if (exit$1 === 1) {
                  return scan_hexadecimal_int(store_char(width$4, ib$2, c$1), ib$2);
                }
                
              }
            }
            else {
              return width$4;
            }
          }
      case 23 : 
          return scan_octal_int(width, ib);
      case 29 : 
          return scan_decimal_digits_plus(width, ib);
      case 1 : 
      case 2 : 
      case 3 : 
      case 4 : 
      case 5 : 
      case 6 : 
      case 7 : 
      case 8 : 
      case 9 : 
      case 11 : 
      case 13 : 
      case 14 : 
      case 15 : 
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
      case 0 : 
      case 32 : 
          return scan_hexadecimal_int(width, ib);
      
    }
  }
  if (exit === 1) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "scanf.ml",
            674,
            9
          ]
        ];
  }
  
}

function scan_frac_part(width, ib) {
  if (width) {
    var c = peek_char(ib);
    if (eof(ib)) {
      return width;
    }
    else {
      if (9 < (-48 + c >>> 0)) {
        return width;
      }
      else {
        return scan_decimal_digits(store_char(width, ib, c), ib);
      }
    }
  }
  else {
    return width;
  }
}

function scan_exp_part(width, ib) {
  if (width) {
    var c = peek_char(ib);
    if (eof(ib)) {
      return width;
    }
    else {
      var exit = 0;
      if (c !== 69) {
        if (c !== 101) {
          return width;
        }
        else {
          exit = 1;
        }
      }
      else {
        exit = 1;
      }
      if (exit === 1) {
        return scan_optionally_signed_decimal_int(store_char(width, ib, c), ib);
      }
      
    }
  }
  else {
    return width;
  }
}

function scan_int_part(width, ib) {
  var width$1 = scan_sign(width, ib);
  return scan_decimal_digits(width$1, ib);
}

function scan_float(width, precision, ib) {
  var width$1 = scan_int_part(width, ib);
  if (width$1) {
    var c = peek_char(ib);
    if (eof(ib)) {
      return [
              /* tuple */0,
              width$1,
              precision
            ];
    }
    else {
      if (c !== 46) {
        return [
                /* tuple */0,
                scan_exp_part(width$1, ib),
                precision
              ];
      }
      else {
        var width$2 = store_char(width$1, ib, c);
        var precision$1 = Pervasives.min(width$2, precision);
        var width$3 = width$2 - (precision$1 - scan_frac_part(precision$1, ib));
        return [
                /* tuple */0,
                scan_exp_part(width$3, ib),
                precision$1
              ];
      }
    }
  }
  else {
    return [
            /* tuple */0,
            width$1,
            precision
          ];
  }
}

function scan_caml_float(width, precision, ib) {
  var width$1 = scan_optionally_signed_decimal_int(width, ib);
  if (width$1) {
    var c = peek_char(ib);
    if (eof(ib)) {
      return bad_float(/* () */0);
    }
    else {
      var switcher = -69 + c;
      if (!(32 < (switcher >>> 0))) {
        if (30 < (-1 + switcher >>> 0)) {
          return scan_exp_part(width$1, ib);
        }
        else {
          return bad_float(/* () */0);
        }
      }
      else {
        if (switcher !== -23) {
          return bad_float(/* () */0);
        }
        else {
          var width$2 = store_char(width$1, ib, c);
          var precision$1 = Pervasives.min(width$2, precision);
          var width$3 = width$2 - (precision$1 - scan_frac_part(precision$1, ib));
          return scan_exp_part(width$3, ib);
        }
      }
    }
  }
  else {
    return bad_float(/* () */0);
  }
}

function scan_string(stp, width, ib) {
  var _width = width;
  while(/* true */1) {
    var width$1 = _width;
    if (width$1) {
      var c = peek_char(ib);
      if (eof(ib)) {
        return width$1;
      }
      else {
        if (stp) {
          if (c === stp[1]) {
            return skip_char(width$1, ib);
          }
          else {
            _width = store_char(width$1, ib, c);
          }
        }
        else {
          var exit = 0;
          var switcher = -9 + c;
          if (!(4 < (switcher >>> 0))) {
            if (1 < (-2 + switcher >>> 0)) {
              return width$1;
            }
            else {
              exit = 1;
            }
          }
          else {
            if (switcher !== 23) {
              exit = 1;
            }
            else {
              return width$1;
            }
          }
          if (exit === 1) {
            _width = store_char(width$1, ib, c);
          }
          
        }
      }
    }
    else {
      return width$1;
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
    }
    else {
      switch (-110 + c) {
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
  }
  else {
    if (c !== 98) {
      return c;
    }
    else {
      return /* "\b" */8;
    }
  }
}

function decimal_value_of_char(c) {
  return c - /* "0" */48;
}

function char_for_decimal_code(c0, c1, c2) {
  var c = 100 * decimal_value_of_char(c0) + 10 * decimal_value_of_char(c1) + decimal_value_of_char(c2);
  if (c < 0 || c > 255) {
    return bad_input(Printf.sprintf([
                      /* Format */0,
                      [
                        /* String_literal */11,
                        "bad character decimal encoding \\",
                        [
                          /* Char */0,
                          [
                            /* Char */0,
                            [
                              /* Char */0,
                              /* End_of_format */0
                            ]
                          ]
                        ]
                      ],
                      "bad character decimal encoding \\%c%c%c"
                    ])(c0, c1, c2));
  }
  else {
    return Pervasives.char_of_int(c);
  }
}

function hexadecimal_value_of_char(c) {
  if (c >= /* "a" */97) {
    return c - 87;
  }
  else {
    if (c >= /* "A" */65) {
      return c - 55;
    }
    else {
      return c - /* "0" */48;
    }
  }
}

function char_for_hexadecimal_code(c1, c2) {
  var c = 16 * hexadecimal_value_of_char(c1) + hexadecimal_value_of_char(c2);
  if (c < 0 || c > 255) {
    return bad_input(Printf.sprintf([
                      /* Format */0,
                      [
                        /* String_literal */11,
                        "bad character hexadecimal encoding \\",
                        [
                          /* Char */0,
                          [
                            /* Char */0,
                            /* End_of_format */0
                          ]
                        ]
                      ],
                      "bad character hexadecimal encoding \\%c%c"
                    ])(c1, c2));
  }
  else {
    return Pervasives.char_of_int(c);
  }
}

function check_next_char(message, width, ib) {
  if (width) {
    var c = peek_char(ib);
    if (eof(ib)) {
      var message$1 = message;
      return bad_input(Printf.sprintf([
                        /* Format */0,
                        [
                          /* String_literal */11,
                          "scanning of ",
                          [
                            /* String */2,
                            /* No_padding */0,
                            [
                              /* String_literal */11,
                              " failed: premature end of file occurred before end of token",
                              /* End_of_format */0
                            ]
                          ]
                        ],
                        "scanning of %s failed: premature end of file occurred before end of token"
                      ])(message$1));
    }
    else {
      return c;
    }
  }
  else {
    return bad_token_length(message);
  }
}

function check_next_char_for_char(param, param$1) {
  return check_next_char("a Char", param, param$1);
}

function check_next_char_for_string(param, param$1) {
  return check_next_char("a String", param, param$1);
}

function scan_backslash_char(width, ib) {
  var c = check_next_char_for_char(width, ib);
  var exit = 0;
  if (c >= 40) {
    if (c >= 58) {
      var switcher = -92 + c;
      if (28 < (switcher >>> 0)) {
        exit = 1;
      }
      else {
        switch (switcher) {
          case 0 : 
          case 6 : 
          case 18 : 
          case 22 : 
          case 24 : 
              exit = 2;
              break;
          case 1 : 
          case 2 : 
          case 3 : 
          case 4 : 
          case 5 : 
          case 7 : 
          case 8 : 
          case 9 : 
          case 10 : 
          case 11 : 
          case 12 : 
          case 13 : 
          case 14 : 
          case 15 : 
          case 16 : 
          case 17 : 
          case 19 : 
          case 20 : 
          case 21 : 
          case 23 : 
          case 25 : 
          case 26 : 
          case 27 : 
              exit = 1;
              break;
          case 28 : 
              var get_digit = function () {
                var c = next_char(ib);
                var switcher = -48 + c;
                if (22 < (switcher >>> 0)) {
                  if (5 < (-49 + switcher >>> 0)) {
                    return bad_input_escape(c);
                  }
                  else {
                    return c;
                  }
                }
                else {
                  if (6 < (-10 + switcher >>> 0)) {
                    return c;
                  }
                  else {
                    return bad_input_escape(c);
                  }
                }
              };
              var c1 = get_digit(/* () */0);
              var c2 = get_digit(/* () */0);
              return store_char(width - 2, ib, char_for_hexadecimal_code(c1, c2));
          
        }
      }
    }
    else {
      if (c >= 48) {
        var get_digit$1 = function () {
          var c = next_char(ib);
          if (9 < (-48 + c >>> 0)) {
            return bad_input_escape(c);
          }
          else {
            return c;
          }
        };
        var c1$1 = get_digit$1(/* () */0);
        var c2$1 = get_digit$1(/* () */0);
        return store_char(width - 2, ib, char_for_decimal_code(c, c1$1, c2$1));
      }
      else {
        exit = 1;
      }
    }
  }
  else {
    exit = c !== 34 ? (
        c >= 39 ? 2 : 1
      ) : 2;
  }
  switch (exit) {
    case 1 : 
        return bad_input_escape(c);
    case 2 : 
        return store_char(width, ib, char_for_backslash(c));
    
  }
}

function scan_caml_char(width, ib) {
  var find_stop = function (width) {
    var c = check_next_char_for_char(width, ib);
    if (c !== 39) {
      return character_mismatch(/* "'" */39, c);
    }
    else {
      return ignore_char(width, ib);
    }
  };
  var width$1 = width;
  var c = checked_peek_char(ib);
  if (c !== 39) {
    return character_mismatch(/* "'" */39, c);
  }
  else {
    var width$2 = ignore_char(width$1, ib);
    var c$1 = check_next_char_for_char(width$2, ib);
    if (c$1 !== 92) {
      return find_stop(store_char(width$2, ib, c$1));
    }
    else {
      return find_stop(scan_backslash_char(ignore_char(width$2, ib), ib));
    }
  }
}

function scan_caml_string(width, ib) {
  var find_stop = function (_width) {
    while(/* true */1) {
      var width = _width;
      var c = check_next_char_for_string(width, ib);
      if (c !== 34) {
        if (c !== 92) {
          _width = store_char(width, ib, c);
        }
        else {
          var width$1 = ignore_char(width, ib);
          var match = check_next_char_for_string(width$1, ib);
          if (match !== 10) {
            if (match !== 13) {
              return find_stop(scan_backslash_char(width$1, ib));
            }
            else {
              return skip_newline(ignore_char(width$1, ib));
            }
          }
          else {
            return skip_spaces(ignore_char(width$1, ib));
          }
        }
      }
      else {
        return ignore_char(width, ib);
      }
    };
  };
  var skip_spaces = function (_width) {
    while(/* true */1) {
      var width = _width;
      var match = check_next_char_for_string(width, ib);
      if (match !== 32) {
        return find_stop(width);
      }
      else {
        _width = ignore_char(width, ib);
      }
    };
  };
  var width$1 = width;
  var c = checked_peek_char(ib);
  if (c !== 34) {
    return character_mismatch(/* "\"" */34, c);
  }
  else {
    return find_stop(ignore_char(width$1, ib));
  }
}

function scan_bool(ib) {
  var c = checked_peek_char(ib);
  var m = c !== 102 ? (
      c !== 116 ? bad_input(Printf.sprintf([
                    /* Format */0,
                    [
                      /* String_literal */11,
                      "the character ",
                      [
                        /* Caml_char */1,
                        [
                          /* String_literal */11,
                          " cannot start a boolean",
                          /* End_of_format */0
                        ]
                      ]
                    ],
                    "the character %C cannot start a boolean"
                  ])(c)) : 4
    ) : 5;
  return scan_string(/* None */0, m, ib);
}

function scan_chars_in_char_set(char_set, scan_indic, width, ib) {
  var scan_chars = function (_i, stp) {
    while(/* true */1) {
      var i = _i;
      var c = peek_char(ib);
      if (i > 0 && !eof(ib) && CamlinternalFormat.is_in_char_set(char_set, c) && c !== stp) {
        store_char(Pervasives.max_int, ib, c);
        _i = i - 1;
      }
      else {
        return 0;
      }
    };
  };
  if (scan_indic) {
    var c = scan_indic[1];
    scan_chars(width, c);
    if (!eof(ib)) {
      var ci = peek_char(ib);
      if (c === ci) {
        return invalidate_current_char(ib);
      }
      else {
        return character_mismatch(c, ci);
      }
    }
    else {
      return 0;
    }
  }
  else {
    return scan_chars(width, -1);
  }
}

function scanf_bad_input(ib, x) {
  var exit = 0;
  var s;
  if (x[1] === Scan_failure) {
    s = x[2];
    exit = 1;
  }
  else {
    if (x[1] === Caml_exceptions.Failure) {
      s = x[2];
      exit = 1;
    }
    else {
      throw x;
    }
  }
  if (exit === 1) {
    var i = char_count(ib);
    return bad_input(Printf.sprintf([
                      /* Format */0,
                      [
                        /* String_literal */11,
                        "scanf: bad input at char number ",
                        [
                          /* Int */4,
                          /* Int_i */3,
                          /* No_padding */0,
                          /* No_precision */0,
                          [
                            /* String_literal */11,
                            ": ",
                            [
                              /* Caml_string */3,
                              /* No_padding */0,
                              /* End_of_format */0
                            ]
                          ]
                        ]
                      ],
                      "scanf: bad input at char number %i: %S"
                    ])(i, s));
  }
  
}

function get_counter(ib, counter) {
  switch (counter) {
    case 0 : 
        var ib$1 = ib;
        return ib$1[5];
    case 1 : 
        return char_count(ib);
    case 2 : 
        var ib$2 = ib;
        return ib$2[6];
    
  }
}

function width_of_pad_opt(pad_opt) {
  if (pad_opt) {
    return pad_opt[1];
  }
  else {
    return Pervasives.max_int;
  }
}

function stopper_of_formatting_lit(fmting) {
  if (fmting === /* Escaped_percent */6) {
    return [
            /* tuple */0,
            /* "%" */37,
            ""
          ];
  }
  else {
    var str = CamlinternalFormat.string_of_formatting_lit(fmting);
    var stp = str.charCodeAt(1);
    var sub_str = $$String.sub(str, 2, str.length - 2);
    return [
            /* tuple */0,
            stp,
            sub_str
          ];
  }
}

function take_format_readers(k, _fmt) {
  while(/* true */1) {
    var fmt = _fmt;
    if (typeof fmt === "number") {
      return k(/* Nil */0);
    }
    else {
      switch (fmt[0]) {
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 8 : 
            _fmt = fmt[4];
            break;
        case 14 : 
            return take_fmtty_format_readers(k, CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmt[2])), fmt[3]);
        case 18 : 
            var match = fmt[1];
            _fmt = match[0] ? CamlinternalFormatBasics.concat_fmt(match[1][1], fmt[2]) : CamlinternalFormatBasics.concat_fmt(match[1][1], fmt[2]);
            break;
        case 19 : 
            var fmt_rest = fmt[1];
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return k([
                            /* Cons */0,
                            reader,
                            readers_rest
                          ]);
              };
              return take_format_readers(new_k, fmt_rest);
            }
            }(fmt_rest));
        case 2 : 
        case 3 : 
        case 11 : 
        case 12 : 
        case 17 : 
        case 21 : 
            _fmt = fmt[2];
            break;
        case 0 : 
        case 1 : 
        case 9 : 
        case 10 : 
        case 15 : 
        case 16 : 
        case 22 : 
            _fmt = fmt[1];
            break;
        case 23 : 
            var k$1 = k;
            var ign = fmt[1];
            var fmt$1 = fmt[2];
            if (typeof ign === "number") {
              switch (ign) {
                case 3 : 
                    return (function(k$1,fmt$1){
                    return function (reader) {
                      var new_k = function (readers_rest) {
                        return k$1([
                                    /* Cons */0,
                                    reader,
                                    readers_rest
                                  ]);
                      };
                      return take_format_readers(new_k, fmt$1);
                    }
                    }(k$1,fmt$1));
                case 0 : 
                case 1 : 
                case 2 : 
                case 4 : 
                    return take_format_readers(k$1, fmt$1);
                
              }
            }
            else {
              switch (ign[0]) {
                case 8 : 
                    return take_fmtty_format_readers(k$1, ign[2], fmt$1);
                case 0 : 
                case 1 : 
                case 2 : 
                case 3 : 
                case 4 : 
                case 5 : 
                case 6 : 
                case 7 : 
                case 9 : 
                case 10 : 
                    return take_format_readers(k$1, fmt$1);
                
              }
            }
        case 13 : 
        case 20 : 
        case 24 : 
            _fmt = fmt[3];
            break;
        
      }
    }
  };
}

function take_fmtty_format_readers(k, _fmtty, fmt) {
  while(/* true */1) {
    var fmtty = _fmtty;
    if (typeof fmtty === "number") {
      return take_format_readers(k, fmt);
    }
    else {
      switch (fmtty[0]) {
        case 8 : 
            _fmtty = fmtty[2];
            break;
        case 9 : 
            var ty = CamlinternalFormat.trans(CamlinternalFormat.symm(fmtty[1]), fmtty[2]);
            _fmtty = CamlinternalFormatBasics.concat_fmtty(ty, fmtty[3]);
            break;
        case 0 : 
        case 1 : 
        case 2 : 
        case 3 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 10 : 
        case 11 : 
        case 12 : 
            _fmtty = fmtty[1];
            break;
        case 13 : 
            var fmt_rest = fmtty[1];
            return (function(fmt_rest){
            return function (reader) {
              var new_k = function (readers_rest) {
                return k([
                            /* Cons */0,
                            reader,
                            readers_rest
                          ]);
              };
              return take_fmtty_format_readers(new_k, fmt_rest, fmt);
            }
            }(fmt_rest));
        case 14 : 
            var fmt_rest$1 = fmtty[1];
            return (function(fmt_rest$1){
            return function (reader) {
              var new_k = function (readers_rest) {
                return k([
                            /* Cons */0,
                            reader,
                            readers_rest
                          ]);
              };
              return take_fmtty_format_readers(new_k, fmt_rest$1, fmt);
            }
            }(fmt_rest$1));
        
      }
    }
  };
}

function make_scanf(ib, _fmt, readers) {
  while(/* true */1) {
    var fmt = _fmt;
    if (typeof fmt === "number") {
      return /* Nil */0;
    }
    else {
      switch (fmt[0]) {
        case 0 : 
            scan_char(0, ib);
            var c = token_char(ib);
            return [
                    /* Cons */0,
                    c,
                    make_scanf(ib, fmt[1], readers)
                  ];
        case 1 : 
            scan_caml_char(0, ib);
            var c$1 = token_char(ib);
            return [
                    /* Cons */0,
                    c$1,
                    make_scanf(ib, fmt[1], readers)
                  ];
        case 2 : 
            var rest = fmt[2];
            var pad = fmt[1];
            var exit = 0;
            if (typeof rest === "number") {
              exit = 1;
            }
            else {
              switch (rest[0]) {
                case 17 : 
                    var match = stopper_of_formatting_lit(rest[1]);
                    var stp = match[1];
                    var scan = (function(stp){
                    return function (width, _, ib) {
                      return scan_string([
                                  /* Some */0,
                                  stp
                                ], width, ib);
                    }
                    }(stp));
                    var str_rest_001 = match[2];
                    var str_rest_002 = rest[2];
                    var str_rest = [
                      /* String_literal */11,
                      str_rest_001,
                      str_rest_002
                    ];
                    return pad_prec_scanf(ib, str_rest, readers, pad, /* No_precision */0, scan, token);
                case 18 : 
                    var match$1 = rest[1];
                    if (match$1[0]) {
                      var scan$1 = function (width, _, ib) {
                        return scan_string([
                                    /* Some */0,
                                    /* "[" */91
                                  ], width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1[1][1], rest[2]), readers, pad, /* No_precision */0, scan$1, token);
                    }
                    else {
                      var scan$2 = function (width, _, ib) {
                        return scan_string([
                                    /* Some */0,
                                    /* "{" */123
                                  ], width, ib);
                      };
                      return pad_prec_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$1[1][1], rest[2]), readers, pad, /* No_precision */0, scan$2, token);
                    }
                    break;
                default:
                  exit = 1;
              }
            }
            if (exit === 1) {
              var scan$3 = function (width, _, ib) {
                return scan_string(/* None */0, width, ib);
              };
              return pad_prec_scanf(ib, rest, readers, pad, /* No_precision */0, scan$3, token);
            }
            break;
        case 3 : 
            var scan$4 = function (width, _, ib) {
              return scan_caml_string(width, ib);
            };
            return pad_prec_scanf(ib, fmt[2], readers, fmt[1], /* No_precision */0, scan$4, token);
        case 4 : 
            var c$2 = CamlinternalFormat.char_of_iconv(fmt[1]);
            var scan$5 = (function(c$2){
            return function (width, _, ib) {
              return scan_int_conv(c$2, width, ib);
            }
            }(c$2));
            return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan$5, (function(c$2){
                      return function (param) {
                        var conv = c$2;
                        var ib = param;
                        return Caml_format.caml_int_of_string(token_int_literal(conv, ib));
                      }
                      }(c$2)));
        case 5 : 
            var c$3 = CamlinternalFormat.char_of_iconv(fmt[1]);
            var scan$6 = (function(c$3){
            return function (width, _, ib) {
              return scan_int_conv(c$3, width, ib);
            }
            }(c$3));
            return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan$6, (function(c$3){
                      return function (param) {
                        var conv = c$3;
                        var ib = param;
                        return Caml_format.caml_int32_of_string(token_int_literal(conv, ib));
                      }
                      }(c$3)));
        case 6 : 
            var c$4 = CamlinternalFormat.char_of_iconv(fmt[1]);
            var scan$7 = (function(c$4){
            return function (width, _, ib) {
              return scan_int_conv(c$4, width, ib);
            }
            }(c$4));
            return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan$7, (function(c$4){
                      return function (param) {
                        var conv = c$4;
                        var ib = param;
                        return Caml_format.caml_nativeint_of_string(token_int_literal(conv, ib));
                      }
                      }(c$4)));
        case 7 : 
            var c$5 = CamlinternalFormat.char_of_iconv(fmt[1]);
            var scan$8 = (function(c$5){
            return function (width, _, ib) {
              return scan_int_conv(c$5, width, ib);
            }
            }(c$5));
            return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan$8, (function(c$5){
                      return function (param) {
                        var conv = c$5;
                        var ib = param;
                        return Caml_primitive.caml_int64_of_string(token_int_literal(conv, ib));
                      }
                      }(c$5)));
        case 8 : 
            if (fmt[1] >= 15) {
              return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan_caml_float, token_float);
            }
            else {
              return pad_prec_scanf(ib, fmt[4], readers, fmt[2], fmt[3], scan_float, token_float);
            }
        case 9 : 
            scan_bool(ib);
            var b = token_bool(ib);
            return [
                    /* Cons */0,
                    b,
                    make_scanf(ib, fmt[1], readers)
                  ];
        case 10 : 
            if (end_of_input(ib)) {
              _fmt = fmt[1];
            }
            else {
              return bad_input("end of input not found");
            }
            break;
        case 11 : 
            $$String.iter(function (param) {
                  return check_char(ib, param);
                }, fmt[1]);
            _fmt = fmt[2];
            break;
        case 12 : 
            check_char(ib, fmt[1]);
            _fmt = fmt[2];
            break;
        case 13 : 
            scan_caml_string(width_of_pad_opt(fmt[1]), ib);
            var s = token(ib);
            var fmt$1;
            try {
              fmt$1 = CamlinternalFormat.format_of_string_fmtty(s, fmt[2]);
            }
            catch (exn){
              if (exn[1] === Caml_exceptions.Failure) {
                fmt$1 = bad_input(exn[2]);
              }
              else {
                throw exn;
              }
            }
            return [
                    /* Cons */0,
                    fmt$1,
                    make_scanf(ib, fmt[3], readers)
                  ];
        case 14 : 
            var fmtty = fmt[2];
            scan_caml_string(width_of_pad_opt(fmt[1]), ib);
            var s$1 = token(ib);
            var match$2;
            try {
              var match$3 = CamlinternalFormat.fmt_ebb_of_string(/* None */0, s$1);
              var match$4 = CamlinternalFormat.fmt_ebb_of_string(/* None */0, s$1);
              match$2 = [
                /* tuple */0,
                CamlinternalFormat.type_format(match$3[1], CamlinternalFormatBasics.erase_rel(fmtty)),
                CamlinternalFormat.type_format(match$4[1], CamlinternalFormatBasics.erase_rel(CamlinternalFormat.symm(fmtty)))
              ];
            }
            catch (exn$1){
              if (exn$1[1] === Caml_exceptions.Failure) {
                match$2 = bad_input(exn$1[2]);
              }
              else {
                throw exn$1;
              }
            }
            return [
                    /* Cons */0,
                    [
                      /* Format */0,
                      match$2[1],
                      s$1
                    ],
                    make_scanf(ib, CamlinternalFormatBasics.concat_fmt(match$2[2], fmt[3]), readers)
                  ];
        case 15 : 
            return Pervasives.invalid_arg('scanf: bad conversion "%a"');
        case 16 : 
            return Pervasives.invalid_arg('scanf: bad conversion "%t"');
        case 17 : 
            $$String.iter(function (param) {
                  return check_char(ib, param);
                }, CamlinternalFormat.string_of_formatting_lit(fmt[1]));
            _fmt = fmt[2];
            break;
        case 18 : 
            var match$5 = fmt[1];
            check_char(ib, /* "@" */64);
            if (match$5[0]) {
              check_char(ib, /* "[" */91);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$5[1][1], fmt[2]);
            }
            else {
              check_char(ib, /* "{" */123);
              _fmt = CamlinternalFormatBasics.concat_fmt(match$5[1][1], fmt[2]);
            }
            break;
        case 19 : 
            var x = readers[1](ib);
            return [
                    /* Cons */0,
                    x,
                    make_scanf(ib, fmt[1], readers[2])
                  ];
        case 20 : 
            var rest$1 = fmt[3];
            var char_set = fmt[2];
            var width_opt = fmt[1];
            var exit$1 = 0;
            if (typeof rest$1 === "number") {
              exit$1 = 1;
            }
            else {
              if (rest$1[0] === 17) {
                var match$6 = stopper_of_formatting_lit(rest$1[1]);
                var width = width_of_pad_opt(width_opt);
                scan_chars_in_char_set(char_set, [
                      /* Some */0,
                      match$6[1]
                    ], width, ib);
                var s$2 = token(ib);
                var str_rest_001$1 = match$6[2];
                var str_rest_002$1 = rest$1[2];
                var str_rest$1 = [
                  /* String_literal */11,
                  str_rest_001$1,
                  str_rest_002$1
                ];
                return [
                        /* Cons */0,
                        s$2,
                        make_scanf(ib, str_rest$1, readers)
                      ];
              }
              else {
                exit$1 = 1;
              }
            }
            if (exit$1 === 1) {
              var width$1 = width_of_pad_opt(width_opt);
              scan_chars_in_char_set(char_set, /* None */0, width$1, ib);
              var s$3 = token(ib);
              return [
                      /* Cons */0,
                      s$3,
                      make_scanf(ib, rest$1, readers)
                    ];
            }
            break;
        case 21 : 
            var count = get_counter(ib, fmt[1]);
            return [
                    /* Cons */0,
                    count,
                    make_scanf(ib, fmt[2], readers)
                  ];
        case 22 : 
            var c$6 = checked_peek_char(ib);
            return [
                    /* Cons */0,
                    c$6,
                    make_scanf(ib, fmt[1], readers)
                  ];
        case 23 : 
            var match$7 = CamlinternalFormat.param_format_of_ignored_format(fmt[1], fmt[2]);
            var match$8 = make_scanf(ib, match$7[1], readers);
            if (match$8) {
              return match$8[2];
            }
            else {
              throw [
                    0,
                    Caml_exceptions.Assert_failure,
                    [
                      0,
                      "scanf.ml",
                      1258,
                      13
                    ]
                  ];
            }
            break;
        case 24 : 
            return Pervasives.invalid_arg('scanf: bad conversion "%?" (custom converter)');
        
      }
    }
  };
}

function pad_prec_scanf(ib, fmt, readers, pad, prec, scan, token) {
  if (typeof pad === "number") {
    if (typeof prec === "number") {
      if (prec !== 0) {
        return Pervasives.invalid_arg('scanf: bad conversion "%*"');
      }
      else {
        scan(Pervasives.max_int, Pervasives.max_int, ib);
        var x = token(ib);
        return [
                /* Cons */0,
                x,
                make_scanf(ib, fmt, readers)
              ];
      }
    }
    else {
      scan(Pervasives.max_int, prec[1], ib);
      var x$1 = token(ib);
      return [
              /* Cons */0,
              x$1,
              make_scanf(ib, fmt, readers)
            ];
    }
  }
  else {
    if (pad[0]) {
      return Pervasives.invalid_arg('scanf: bad conversion "%*"');
    }
    else {
      if (pad[1] !== 0) {
        var w = pad[2];
        if (typeof prec === "number") {
          if (prec !== 0) {
            return Pervasives.invalid_arg('scanf: bad conversion "%*"');
          }
          else {
            scan(w, Pervasives.max_int, ib);
            var x$2 = token(ib);
            return [
                    /* Cons */0,
                    x$2,
                    make_scanf(ib, fmt, readers)
                  ];
          }
        }
        else {
          scan(w, prec[1], ib);
          var x$3 = token(ib);
          return [
                  /* Cons */0,
                  x$3,
                  make_scanf(ib, fmt, readers)
                ];
        }
      }
      else {
        return Pervasives.invalid_arg('scanf: bad conversion "%-"');
      }
    }
  }
}

function kscanf(ib, ef, param) {
  var str = param[2];
  var fmt = param[1];
  var k = function (readers, f) {
    reset_token(ib);
    var match;
    try {
      match = [
        /* Args */0,
        make_scanf(ib, fmt, readers)
      ];
    }
    catch (exc){
      var exit = 0;
      if (exc[1] === Scan_failure) {
        exit = 1;
      }
      else {
        if (exc[1] === Caml_exceptions.Failure) {
          exit = 1;
        }
        else {
          if (exc === Caml_exceptions.End_of_file) {
            exit = 1;
          }
          else {
            if (exc[1] === Caml_exceptions.Invalid_argument) {
              match = Pervasives.invalid_arg(exc[2] + (' in format "' + ($$String.escaped(str) + '"')));
            }
            else {
              throw exc;
            }
          }
        }
      }
      if (exit === 1) {
        match = [
          /* Exc */1,
          exc
        ];
      }
      
    }
    if (match[0]) {
      return ef(ib, match[1]);
    }
    else {
      var _f = f;
      var _args = match[1];
      while(/* true */1) {
        var args = _args;
        var f$1 = _f;
        if (args) {
          _args = args[2];
          _f = f$1(args[1]);
        }
        else {
          return f$1;
        }
      };
    }
  };
  return take_format_readers(k, fmt);
}

function ksscanf(s, ef, fmt) {
  return kscanf(from_string(s), ef, fmt);
}

function kfscanf(ic, ef, fmt) {
  return kscanf(from_channel(ic), ef, fmt);
}

function bscanf(ib, fmt) {
  return kscanf(ib, scanf_bad_input, fmt);
}

function fscanf(ic, fmt) {
  return kscanf(from_channel(ic), scanf_bad_input, fmt);
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
  var $js;
  try {
    $js = CamlinternalFormat.format_of_string_format(str, format);
  }
  catch (exn){
    if (exn[1] === Caml_exceptions.Failure) {
      $js = bad_input(exn[2]);
    }
    else {
      throw exn;
    }
  }
  return f($js);
}

function sscanf_format(s, format, f) {
  return bscanf_format(from_string(s), format, f);
}

function string_to_String(s) {
  var l = s.length;
  var b = Buffer.create(l + 2);
  Buffer.add_char(b, /* "\"" */34);
  for(var i = 0 ,i_finish = l - 1; i<= i_finish; ++i){
    var c = s.charCodeAt(i);
    if (c === /* "\"" */34) {
      Buffer.add_char(b, /* "\\" */92);
    }
    Buffer.add_char(b, c);
  }
  Buffer.add_char(b, /* "\"" */34);
  return Buffer.contents(b);
}

function format_from_string(s, fmt) {
  return sscanf_format(string_to_String(s), fmt, function (x) {
              return x;
            });
}

function unescaped(s) {
  return sscanf('"' + (s + '"'), [
                /* Format */0,
                [
                  /* Caml_string */3,
                  /* No_padding */0,
                  [
                    /* Flush */10,
                    /* End_of_format */0
                  ]
                ],
                "%S%!"
              ])(function (x) {
              return x;
            });
}

var Scanning = [
  0,
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
