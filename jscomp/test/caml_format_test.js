'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Int64 = require("../../lib/js/int64.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Format = require("../../lib/js/format.js");
var Printf = require("../../lib/js/printf.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var CamlinternalFormatBasics = require("../../lib/js/camlinternalFormatBasics.js");

var of_string = [
  [
    0,
    "0"
  ],
  [
    3,
    "03"
  ],
  [
    -3,
    "-03"
  ],
  [
    -63,
    "-0x3f"
  ],
  [
    -31,
    "-0x1f"
  ],
  [
    47,
    "0X2f"
  ],
  [
    11,
    "0O13"
  ],
  [
    8,
    "0o10"
  ],
  [
    3,
    "0b11"
  ],
  [
    1,
    "0b01"
  ],
  [
    0,
    "0b00"
  ],
  [
    -3,
    "-0b11"
  ],
  [
    -5,
    "-0B101"
  ],
  [
    332,
    "0332"
  ],
  [
    -32,
    "-32"
  ],
  [
    1,
    "-0xffff_ffff"
  ],
  [
    -1,
    "0xffff_ffff"
  ]
];

function from_float_of_string(xs) {
  return $$Array.mapi((function (i, param) {
                return Pervasives.string_of_float;
              }), xs);
}

function from_of_string(xs) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return [
                            Curry._1(Printf.sprintf(/* Format */{
                                      _0: {
                                        TAG: /* String_literal */11,
                                        _0: "of_string ",
                                        _1: {
                                          TAG: /* Scan_get_counter */21,
                                          _0: /* Token_counter */2,
                                          _1: /* End_of_format */0
                                        }
                                      },
                                      _1: "of_string %L"
                                    }), i),
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: Caml_format.caml_int_of_string(b),
                                        _1: a
                                      };
                              })
                          ];
                  }), of_string));
}

function u(v) {
  return Curry._1(Printf.sprintf(/* Format */{
                  _0: {
                    TAG: /* Int */4,
                    _0: /* Int_d */0,
                    _1: {
                      TAG: /* Lit_padding */0,
                      _0: /* Right */1,
                      _1: 33
                    },
                    _2: /* No_precision */0,
                    _3: /* End_of_format */0
                  },
                  _1: "%33d"
                }), v);
}

var to_str = Caml_format.caml_int_of_string;

var v = Curry._1(Printf.sprintf(/* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: {
              TAG: /* Lit_padding */0,
              _0: /* Right */1,
              _1: 3
            },
            _2: /* No_precision */0,
            _3: /* End_of_format */0
          },
          _1: "%3d"
        }), 3333);

var pairs = [
  [
    /* FP_infinite */3,
    "infinity"
  ],
  [
    /* FP_infinite */3,
    "+infinity"
  ],
  [
    /* FP_infinite */3,
    "-infinity"
  ],
  [
    /* FP_zero */2,
    "0"
  ],
  [
    /* FP_zero */2,
    "0."
  ]
];

var pairs$1 = [
  [
    3232,
    "32_32.0"
  ],
  [
    1.000,
    "1.000"
  ],
  [
    12.000,
    "12.000"
  ]
];

var suites = Pervasives.$at(from_of_string(of_string), Pervasives.$at({
          hd: [
            "isnan_of_string",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: true,
                        _1: Pervasives.classify_float(Caml_format.caml_float_of_string("nan")) === /* FP_nan */4
                      };
              })
          ],
          tl: /* [] */0
        }, Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                        var b = param[1];
                        var a = param[0];
                        return [
                                Curry._1(Printf.sprintf(/* Format */{
                                          _0: {
                                            TAG: /* String_literal */11,
                                            _0: "infinity_of_string ",
                                            _1: {
                                              TAG: /* Int */4,
                                              _0: /* Int_d */0,
                                              _1: /* No_padding */0,
                                              _2: /* No_precision */0,
                                              _3: /* End_of_format */0
                                            }
                                          },
                                          _1: "infinity_of_string %d"
                                        }), i),
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: a,
                                            _1: Pervasives.classify_float(Caml_format.caml_float_of_string(b))
                                          };
                                  })
                              ];
                      }), pairs)), Pervasives.$at({
                  hd: [
                    "throw",
                    (function (param) {
                        return {
                                TAG: /* ThrowAny */7,
                                _0: (function (param) {
                                    Caml_format.caml_float_of_string("");
                                    
                                  })
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "format_int",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: "                              33",
                                  _1: Caml_format.caml_format_int("%32d", 33)
                                };
                        })
                    ],
                    tl: /* [] */0
                  }
                }, $$Array.to_list($$Array.mapi((function (i, param) {
                            var b = param[1];
                            var a = param[0];
                            return [
                                    Curry._1(Printf.sprintf(/* Format */{
                                              _0: {
                                                TAG: /* String_literal */11,
                                                _0: "normal_float_of_string ",
                                                _1: {
                                                  TAG: /* Int */4,
                                                  _0: /* Int_d */0,
                                                  _1: /* No_padding */0,
                                                  _2: /* No_precision */0,
                                                  _3: /* End_of_format */0
                                                }
                                              },
                                              _1: "normal_float_of_string %d"
                                            }), i),
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: a,
                                                _1: Caml_format.caml_float_of_string(b)
                                              };
                                      })
                                  ];
                          }), pairs$1))))));

function $caret$caret(param, param$1) {
  return /* Format */{
          _0: CamlinternalFormatBasics.concat_fmt(param._0, param$1._0),
          _1: param._1 + ("%," + param$1._1)
        };
}

function ff(param) {
  return Caml_format.caml_format_int("%32d", param);
}

var formatter_suites_0 = [
  "fmt_concat",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: Curry._6(Format.asprintf($caret$caret(/* Format */{
                            _0: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: {
                                    TAG: /* Lit_padding */0,
                                    _0: /* Zeros */2,
                                    _1: 3
                                  },
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* Char_literal */12,
                                    _0: /* " " */32,
                                    _1: {
                                      TAG: /* Scan_get_counter */21,
                                      _0: /* Token_counter */2,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            },
                            _1: "%s %03d %L"
                          }, /* Format */{
                            _0: {
                              TAG: /* Caml_string */3,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: {
                                    TAG: /* Lit_padding */0,
                                    _0: /* Zeros */2,
                                    _1: 3
                                  },
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* Char_literal */12,
                                    _0: /* " " */32,
                                    _1: {
                                      TAG: /* Scan_get_counter */21,
                                      _0: /* Token_counter */2,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            },
                            _1: "%S %03d %L"
                          })), "32", 33, 33, "a", 33, 3),
              _1: "32 033 33\"a\" 033 3"
            };
    })
];

var formatter_suites_1 = {
  hd: [
    "fmt_gen",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: Curry._8(Format.asprintf($caret$caret(/* Format */{
                              _0: {
                                TAG: /* String */2,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* Char_literal */12,
                                  _0: /* " " */32,
                                  _1: {
                                    TAG: /* Int */4,
                                    _0: /* Int_d */0,
                                    _1: {
                                      TAG: /* Lit_padding */0,
                                      _0: /* Zeros */2,
                                      _1: 3
                                    },
                                    _2: /* No_precision */0,
                                    _3: {
                                      TAG: /* Char_literal */12,
                                      _0: /* " " */32,
                                      _1: {
                                        TAG: /* Scan_get_counter */21,
                                        _0: /* Token_counter */2,
                                        _1: /* End_of_format */0
                                      }
                                    }
                                  }
                                }
                              },
                              _1: "%s %03d %L"
                            }, /* Format */{
                              _0: {
                                TAG: /* Caml_string */3,
                                _0: /* No_padding */0,
                                _1: {
                                  TAG: /* Char_literal */12,
                                  _0: /* " " */32,
                                  _1: {
                                    TAG: /* Int */4,
                                    _0: /* Int_d */0,
                                    _1: {
                                      TAG: /* Lit_padding */0,
                                      _0: /* Zeros */2,
                                      _1: 3
                                    },
                                    _2: /* No_precision */0,
                                    _3: {
                                      TAG: /* Char_literal */12,
                                      _0: /* " " */32,
                                      _1: {
                                        TAG: /* Scan_get_counter */21,
                                        _0: /* Token_counter */2,
                                        _1: {
                                          TAG: /* Char_literal */12,
                                          _0: /* " " */32,
                                          _1: {
                                            TAG: /* Alpha */15,
                                            _0: /* End_of_format */0
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              },
                              _1: "%S %03d %L %a"
                            })), "32", 33, 33, "a", 33, 3, (function (param, param$1) {
                        return Format.pp_print_list(undefined, Format.pp_print_int, param, param$1);
                      }), {
                      hd: 1,
                      tl: {
                        hd: 2,
                        tl: {
                          hd: 3,
                          tl: /* [] */0
                        }
                      }
                    }),
                _1: "32 033 33\"a\" 033 3 12\n3"
              };
      })
  ],
  tl: {
    hd: [
      "long_fmt",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: Curry.app(Format.asprintf(/* Format */{
                            _0: {
                              TAG: /* Int */4,
                              _0: /* Int_d */0,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Char_literal */12,
                                _0: /* " " */32,
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_i */3,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* Char_literal */12,
                                    _0: /* " " */32,
                                    _1: {
                                      TAG: /* Int */4,
                                      _0: /* Int_u */12,
                                      _1: /* No_padding */0,
                                      _2: /* No_precision */0,
                                      _3: {
                                        TAG: /* Char_literal */12,
                                        _0: /* " " */32,
                                        _1: {
                                          TAG: /* Scan_get_counter */21,
                                          _0: /* Char_counter */1,
                                          _1: {
                                            TAG: /* Char_literal */12,
                                            _0: /* " " */32,
                                            _1: {
                                              TAG: /* Scan_get_counter */21,
                                              _0: /* Line_counter */0,
                                              _1: {
                                                TAG: /* Char_literal */12,
                                                _0: /* " " */32,
                                                _1: {
                                                  TAG: /* Scan_get_counter */21,
                                                  _0: /* Token_counter */2,
                                                  _1: {
                                                    TAG: /* Char_literal */12,
                                                    _0: /* " " */32,
                                                    _1: {
                                                      TAG: /* Scan_get_counter */21,
                                                      _0: /* Token_counter */2,
                                                      _1: {
                                                        TAG: /* Char_literal */12,
                                                        _0: /* " " */32,
                                                        _1: {
                                                          TAG: /* Int */4,
                                                          _0: /* Int_x */6,
                                                          _1: /* No_padding */0,
                                                          _2: /* No_precision */0,
                                                          _3: {
                                                            TAG: /* Char_literal */12,
                                                            _0: /* " " */32,
                                                            _1: {
                                                              TAG: /* Int */4,
                                                              _0: /* Int_X */8,
                                                              _1: /* No_padding */0,
                                                              _2: /* No_precision */0,
                                                              _3: {
                                                                TAG: /* Char_literal */12,
                                                                _0: /* " " */32,
                                                                _1: {
                                                                  TAG: /* Int */4,
                                                                  _0: /* Int_o */10,
                                                                  _1: /* No_padding */0,
                                                                  _2: /* No_precision */0,
                                                                  _3: {
                                                                    TAG: /* Char_literal */12,
                                                                    _0: /* " " */32,
                                                                    _1: {
                                                                      TAG: /* String */2,
                                                                      _0: /* No_padding */0,
                                                                      _1: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* " " */32,
                                                                        _1: {
                                                                          TAG: /* Caml_string */3,
                                                                          _0: /* No_padding */0,
                                                                          _1: {
                                                                            TAG: /* Char_literal */12,
                                                                            _0: /* " " */32,
                                                                            _1: {
                                                                              TAG: /* Char */0,
                                                                              _0: {
                                                                                TAG: /* Char_literal */12,
                                                                                _0: /* " " */32,
                                                                                _1: {
                                                                                  TAG: /* Caml_char */1,
                                                                                  _0: {
                                                                                    TAG: /* Char_literal */12,
                                                                                    _0: /* " " */32,
                                                                                    _1: {
                                                                                      TAG: /* Float */8,
                                                                                      _0: /* Float_f */0,
                                                                                      _1: /* No_padding */0,
                                                                                      _2: /* No_precision */0,
                                                                                      _3: {
                                                                                        TAG: /* Char_literal */12,
                                                                                        _0: /* " " */32,
                                                                                        _1: {
                                                                                          TAG: /* Float */8,
                                                                                          _0: /* Float_F */15,
                                                                                          _1: /* No_padding */0,
                                                                                          _2: /* No_precision */0,
                                                                                          _3: {
                                                                                            TAG: /* Char_literal */12,
                                                                                            _0: /* " " */32,
                                                                                            _1: {
                                                                                              TAG: /* Float */8,
                                                                                              _0: /* Float_e */3,
                                                                                              _1: /* No_padding */0,
                                                                                              _2: /* No_precision */0,
                                                                                              _3: {
                                                                                                TAG: /* Char_literal */12,
                                                                                                _0: /* " " */32,
                                                                                                _1: {
                                                                                                  TAG: /* Float */8,
                                                                                                  _0: /* Float_E */6,
                                                                                                  _1: /* No_padding */0,
                                                                                                  _2: /* No_precision */0,
                                                                                                  _3: {
                                                                                                    TAG: /* Char_literal */12,
                                                                                                    _0: /* " " */32,
                                                                                                    _1: {
                                                                                                      TAG: /* Float */8,
                                                                                                      _0: /* Float_g */9,
                                                                                                      _1: /* No_padding */0,
                                                                                                      _2: /* No_precision */0,
                                                                                                      _3: {
                                                                                                        TAG: /* Char_literal */12,
                                                                                                        _0: /* " " */32,
                                                                                                        _1: {
                                                                                                          TAG: /* Float */8,
                                                                                                          _0: /* Float_G */12,
                                                                                                          _1: /* No_padding */0,
                                                                                                          _2: /* No_precision */0,
                                                                                                          _3: {
                                                                                                            TAG: /* Char_literal */12,
                                                                                                            _0: /* " " */32,
                                                                                                            _1: {
                                                                                                              TAG: /* Bool */9,
                                                                                                              _0: /* No_padding */0,
                                                                                                              _1: {
                                                                                                                TAG: /* Char_literal */12,
                                                                                                                _0: /* " " */32,
                                                                                                                _1: {
                                                                                                                  TAG: /* Bool */9,
                                                                                                                  _0: /* No_padding */0,
                                                                                                                  _1: {
                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                    _0: /* " " */32,
                                                                                                                    _1: {
                                                                                                                      TAG: /* Int32 */5,
                                                                                                                      _0: /* Int_d */0,
                                                                                                                      _1: /* No_padding */0,
                                                                                                                      _2: /* No_precision */0,
                                                                                                                      _3: {
                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                        _0: /* " " */32,
                                                                                                                        _1: {
                                                                                                                          TAG: /* Int32 */5,
                                                                                                                          _0: /* Int_i */3,
                                                                                                                          _1: /* No_padding */0,
                                                                                                                          _2: /* No_precision */0,
                                                                                                                          _3: {
                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                            _0: /* " " */32,
                                                                                                                            _1: {
                                                                                                                              TAG: /* Int32 */5,
                                                                                                                              _0: /* Int_u */12,
                                                                                                                              _1: /* No_padding */0,
                                                                                                                              _2: /* No_precision */0,
                                                                                                                              _3: {
                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                _0: /* " " */32,
                                                                                                                                _1: {
                                                                                                                                  TAG: /* Int32 */5,
                                                                                                                                  _0: /* Int_x */6,
                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                  _3: {
                                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                                    _0: /* " " */32,
                                                                                                                                    _1: {
                                                                                                                                      TAG: /* Int32 */5,
                                                                                                                                      _0: /* Int_X */8,
                                                                                                                                      _1: /* No_padding */0,
                                                                                                                                      _2: /* No_precision */0,
                                                                                                                                      _3: {
                                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                                        _0: /* " " */32,
                                                                                                                                        _1: {
                                                                                                                                          TAG: /* Int32 */5,
                                                                                                                                          _0: /* Int_o */10,
                                                                                                                                          _1: /* No_padding */0,
                                                                                                                                          _2: /* No_precision */0,
                                                                                                                                          _3: {
                                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                                            _0: /* " " */32,
                                                                                                                                            _1: {
                                                                                                                                              TAG: /* Nativeint */6,
                                                                                                                                              _0: /* Int_d */0,
                                                                                                                                              _1: /* No_padding */0,
                                                                                                                                              _2: /* No_precision */0,
                                                                                                                                              _3: {
                                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                                _0: /* " " */32,
                                                                                                                                                _1: {
                                                                                                                                                  TAG: /* Nativeint */6,
                                                                                                                                                  _0: /* Int_i */3,
                                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                                  _3: {
                                                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                                                    _0: /* " " */32,
                                                                                                                                                    _1: {
                                                                                                                                                      TAG: /* Nativeint */6,
                                                                                                                                                      _0: /* Int_u */12,
                                                                                                                                                      _1: /* No_padding */0,
                                                                                                                                                      _2: /* No_precision */0,
                                                                                                                                                      _3: {
                                                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                                                        _0: /* " " */32,
                                                                                                                                                        _1: {
                                                                                                                                                          TAG: /* Nativeint */6,
                                                                                                                                                          _0: /* Int_x */6,
                                                                                                                                                          _1: /* No_padding */0,
                                                                                                                                                          _2: /* No_precision */0,
                                                                                                                                                          _3: {
                                                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                                                            _0: /* " " */32,
                                                                                                                                                            _1: {
                                                                                                                                                              TAG: /* Nativeint */6,
                                                                                                                                                              _0: /* Int_x */6,
                                                                                                                                                              _1: /* No_padding */0,
                                                                                                                                                              _2: /* No_precision */0,
                                                                                                                                                              _3: {
                                                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                                                _0: /* " " */32,
                                                                                                                                                                _1: {
                                                                                                                                                                  TAG: /* Nativeint */6,
                                                                                                                                                                  _0: /* Int_o */10,
                                                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                                                  _3: {
                                                                                                                                                                    TAG: /* String_literal */11,
                                                                                                                                                                    _0: "  ",
                                                                                                                                                                    _1: /* End_of_format */0
                                                                                                                                                                  }
                                                                                                                                                                }
                                                                                                                                                              }
                                                                                                                                                            }
                                                                                                                                                          }
                                                                                                                                                        }
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            },
                            _1: "%d %i %u %n %l %L %N %x %X %o %s %S %c %C %f %F %e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  "
                          }), [
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        "a",
                        "b",
                        /* "c" */99,
                        /* "d" */100,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        true,
                        false,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        11
                      ]),
                  _1: "1 2 3 4 5 6 7 8 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "
                };
        })
    ],
    tl: {
      hd: [
        "long_fmt_2",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: Curry.app(Format.asprintf(/* Format */{
                              _0: {
                                TAG: /* Formatting_gen */18,
                                _0: {
                                  TAG: /* Open_box */1,
                                  _0: /* Format */{
                                    _0: /* End_of_format */0,
                                    _1: ""
                                  }
                                },
                                _1: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: {
                                    TAG: /* Lit_padding */0,
                                    _0: /* Right */1,
                                    _1: 23
                                  },
                                  _2: /* No_precision */0,
                                  _3: {
                                    TAG: /* Char_literal */12,
                                    _0: /* " " */32,
                                    _1: {
                                      TAG: /* Int */4,
                                      _0: /* Int_i */3,
                                      _1: {
                                        TAG: /* Lit_padding */0,
                                        _0: /* Right */1,
                                        _1: 2
                                      },
                                      _2: /* No_precision */0,
                                      _3: {
                                        TAG: /* Char_literal */12,
                                        _0: /* " " */32,
                                        _1: {
                                          TAG: /* Int */4,
                                          _0: /* Int_u */12,
                                          _1: {
                                            TAG: /* Lit_padding */0,
                                            _0: /* Right */1,
                                            _1: 3
                                          },
                                          _2: /* No_precision */0,
                                          _3: {
                                            TAG: /* Char_literal */12,
                                            _0: /* " " */32,
                                            _1: {
                                              TAG: /* Scan_get_counter */21,
                                              _0: /* Char_counter */1,
                                              _1: {
                                                TAG: /* Char_literal */12,
                                                _0: /* " " */32,
                                                _1: {
                                                  TAG: /* Int */4,
                                                  _0: /* Int_x */6,
                                                  _1: {
                                                    TAG: /* Lit_padding */0,
                                                    _0: /* Right */1,
                                                    _1: 0
                                                  },
                                                  _2: /* No_precision */0,
                                                  _3: {
                                                    TAG: /* String_literal */11,
                                                    _0: "l ",
                                                    _1: {
                                                      TAG: /* Int */4,
                                                      _0: /* Int_x */6,
                                                      _1: {
                                                        TAG: /* Lit_padding */0,
                                                        _0: /* Right */1,
                                                        _1: 0
                                                      },
                                                      _2: /* No_precision */0,
                                                      _3: {
                                                        TAG: /* String_literal */11,
                                                        _0: "L ",
                                                        _1: {
                                                          TAG: /* Scan_get_counter */21,
                                                          _0: /* Token_counter */2,
                                                          _1: {
                                                            TAG: /* Char_literal */12,
                                                            _0: /* " " */32,
                                                            _1: {
                                                              TAG: /* Int */4,
                                                              _0: /* Int_x */6,
                                                              _1: {
                                                                TAG: /* Lit_padding */0,
                                                                _0: /* Zeros */2,
                                                                _1: 3
                                                              },
                                                              _2: /* No_precision */0,
                                                              _3: {
                                                                TAG: /* Char_literal */12,
                                                                _0: /* " " */32,
                                                                _1: {
                                                                  TAG: /* Int */4,
                                                                  _0: /* Int_X */8,
                                                                  _1: /* No_padding */0,
                                                                  _2: /* No_precision */0,
                                                                  _3: {
                                                                    TAG: /* Char_literal */12,
                                                                    _0: /* " " */32,
                                                                    _1: {
                                                                      TAG: /* Int */4,
                                                                      _0: /* Int_o */10,
                                                                      _1: /* No_padding */0,
                                                                      _2: /* No_precision */0,
                                                                      _3: {
                                                                        TAG: /* Char_literal */12,
                                                                        _0: /* " " */32,
                                                                        _1: {
                                                                          TAG: /* String */2,
                                                                          _0: /* No_padding */0,
                                                                          _1: {
                                                                            TAG: /* Char_literal */12,
                                                                            _0: /* " " */32,
                                                                            _1: {
                                                                              TAG: /* Caml_string */3,
                                                                              _0: /* No_padding */0,
                                                                              _1: {
                                                                                TAG: /* Char_literal */12,
                                                                                _0: /* " " */32,
                                                                                _1: {
                                                                                  TAG: /* Char */0,
                                                                                  _0: {
                                                                                    TAG: /* Char_literal */12,
                                                                                    _0: /* " " */32,
                                                                                    _1: {
                                                                                      TAG: /* Caml_char */1,
                                                                                      _0: {
                                                                                        TAG: /* Char_literal */12,
                                                                                        _0: /* " " */32,
                                                                                        _1: {
                                                                                          TAG: /* Float */8,
                                                                                          _0: /* Float_f */0,
                                                                                          _1: {
                                                                                            TAG: /* Lit_padding */0,
                                                                                            _0: /* Right */1,
                                                                                            _1: 3
                                                                                          },
                                                                                          _2: /* No_precision */0,
                                                                                          _3: {
                                                                                            TAG: /* Char_literal */12,
                                                                                            _0: /* " " */32,
                                                                                            _1: {
                                                                                              TAG: /* Float */8,
                                                                                              _0: /* Float_F */15,
                                                                                              _1: {
                                                                                                TAG: /* Lit_padding */0,
                                                                                                _0: /* Right */1,
                                                                                                _1: 2
                                                                                              },
                                                                                              _2: /* No_precision */0,
                                                                                              _3: {
                                                                                                TAG: /* Char_literal */12,
                                                                                                _0: /* " " */32,
                                                                                                _1: {
                                                                                                  TAG: /* Float */8,
                                                                                                  _0: /* Float_e */3,
                                                                                                  _1: {
                                                                                                    TAG: /* Lit_padding */0,
                                                                                                    _0: /* Right */1,
                                                                                                    _1: 2
                                                                                                  },
                                                                                                  _2: /* No_precision */0,
                                                                                                  _3: {
                                                                                                    TAG: /* Char_literal */12,
                                                                                                    _0: /* " " */32,
                                                                                                    _1: {
                                                                                                      TAG: /* Float */8,
                                                                                                      _0: /* Float_E */6,
                                                                                                      _1: /* No_padding */0,
                                                                                                      _2: /* No_precision */0,
                                                                                                      _3: {
                                                                                                        TAG: /* Char_literal */12,
                                                                                                        _0: /* " " */32,
                                                                                                        _1: {
                                                                                                          TAG: /* Float */8,
                                                                                                          _0: /* Float_g */9,
                                                                                                          _1: /* No_padding */0,
                                                                                                          _2: /* No_precision */0,
                                                                                                          _3: {
                                                                                                            TAG: /* Char_literal */12,
                                                                                                            _0: /* " " */32,
                                                                                                            _1: {
                                                                                                              TAG: /* Float */8,
                                                                                                              _0: /* Float_G */12,
                                                                                                              _1: /* No_padding */0,
                                                                                                              _2: /* No_precision */0,
                                                                                                              _3: {
                                                                                                                TAG: /* Char_literal */12,
                                                                                                                _0: /* " " */32,
                                                                                                                _1: {
                                                                                                                  TAG: /* Bool */9,
                                                                                                                  _0: /* No_padding */0,
                                                                                                                  _1: {
                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                    _0: /* " " */32,
                                                                                                                    _1: {
                                                                                                                      TAG: /* Bool */9,
                                                                                                                      _0: /* No_padding */0,
                                                                                                                      _1: {
                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                        _0: /* " " */32,
                                                                                                                        _1: {
                                                                                                                          TAG: /* Int32 */5,
                                                                                                                          _0: /* Int_d */0,
                                                                                                                          _1: /* No_padding */0,
                                                                                                                          _2: /* No_precision */0,
                                                                                                                          _3: {
                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                            _0: /* " " */32,
                                                                                                                            _1: {
                                                                                                                              TAG: /* Int32 */5,
                                                                                                                              _0: /* Int_i */3,
                                                                                                                              _1: /* No_padding */0,
                                                                                                                              _2: /* No_precision */0,
                                                                                                                              _3: {
                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                _0: /* " " */32,
                                                                                                                                _1: {
                                                                                                                                  TAG: /* Int32 */5,
                                                                                                                                  _0: /* Int_u */12,
                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                  _3: {
                                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                                    _0: /* " " */32,
                                                                                                                                    _1: {
                                                                                                                                      TAG: /* Int32 */5,
                                                                                                                                      _0: /* Int_x */6,
                                                                                                                                      _1: /* No_padding */0,
                                                                                                                                      _2: /* No_precision */0,
                                                                                                                                      _3: {
                                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                                        _0: /* " " */32,
                                                                                                                                        _1: {
                                                                                                                                          TAG: /* Int32 */5,
                                                                                                                                          _0: /* Int_X */8,
                                                                                                                                          _1: /* No_padding */0,
                                                                                                                                          _2: /* No_precision */0,
                                                                                                                                          _3: {
                                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                                            _0: /* " " */32,
                                                                                                                                            _1: {
                                                                                                                                              TAG: /* Int32 */5,
                                                                                                                                              _0: /* Int_o */10,
                                                                                                                                              _1: /* No_padding */0,
                                                                                                                                              _2: /* No_precision */0,
                                                                                                                                              _3: {
                                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                                _0: /* " " */32,
                                                                                                                                                _1: {
                                                                                                                                                  TAG: /* Nativeint */6,
                                                                                                                                                  _0: /* Int_d */0,
                                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                                  _3: {
                                                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                                                    _0: /* " " */32,
                                                                                                                                                    _1: {
                                                                                                                                                      TAG: /* Nativeint */6,
                                                                                                                                                      _0: /* Int_i */3,
                                                                                                                                                      _1: /* No_padding */0,
                                                                                                                                                      _2: /* No_precision */0,
                                                                                                                                                      _3: {
                                                                                                                                                        TAG: /* Char_literal */12,
                                                                                                                                                        _0: /* " " */32,
                                                                                                                                                        _1: {
                                                                                                                                                          TAG: /* Nativeint */6,
                                                                                                                                                          _0: /* Int_u */12,
                                                                                                                                                          _1: /* No_padding */0,
                                                                                                                                                          _2: /* No_precision */0,
                                                                                                                                                          _3: {
                                                                                                                                                            TAG: /* Char_literal */12,
                                                                                                                                                            _0: /* " " */32,
                                                                                                                                                            _1: {
                                                                                                                                                              TAG: /* Nativeint */6,
                                                                                                                                                              _0: /* Int_x */6,
                                                                                                                                                              _1: /* No_padding */0,
                                                                                                                                                              _2: /* No_precision */0,
                                                                                                                                                              _3: {
                                                                                                                                                                TAG: /* Char_literal */12,
                                                                                                                                                                _0: /* " " */32,
                                                                                                                                                                _1: {
                                                                                                                                                                  TAG: /* Nativeint */6,
                                                                                                                                                                  _0: /* Int_x */6,
                                                                                                                                                                  _1: /* No_padding */0,
                                                                                                                                                                  _2: /* No_precision */0,
                                                                                                                                                                  _3: {
                                                                                                                                                                    TAG: /* Char_literal */12,
                                                                                                                                                                    _0: /* " " */32,
                                                                                                                                                                    _1: {
                                                                                                                                                                      TAG: /* Nativeint */6,
                                                                                                                                                                      _0: /* Int_o */10,
                                                                                                                                                                      _1: /* No_padding */0,
                                                                                                                                                                      _2: /* No_precision */0,
                                                                                                                                                                      _3: {
                                                                                                                                                                        TAG: /* String_literal */11,
                                                                                                                                                                        _0: "  ",
                                                                                                                                                                        _1: {
                                                                                                                                                                          TAG: /* Formatting_lit */17,
                                                                                                                                                                          _0: /* Close_box */0,
                                                                                                                                                                          _1: /* End_of_format */0
                                                                                                                                                                        }
                                                                                                                                                                      }
                                                                                                                                                                    }
                                                                                                                                                                  }
                                                                                                                                                                }
                                                                                                                                                              }
                                                                                                                                                            }
                                                                                                                                                          }
                                                                                                                                                        }
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              },
                              _1: "@[%23d %2i %3u %n %0xl %0xL %N %03x %X %o %s %S %c %C %3f %2F %2e %E %g %G %B %b %ld %li %lu %lx %lX %lo %nd %ni %nu %nx %nx %no  @]"
                            }), [
                          1,
                          2,
                          3,
                          4,
                          5,
                          6,
                          7,
                          8,
                          9,
                          10,
                          "a",
                          "b",
                          /* "c" */99,
                          /* "d" */100,
                          1,
                          2,
                          3,
                          4,
                          5,
                          6,
                          true,
                          false,
                          0,
                          1,
                          2,
                          3,
                          4,
                          5,
                          6,
                          7,
                          8,
                          9,
                          10,
                          11
                        ]),
                    _1: "                      1  2   3 4 5l 6L 7 008 9 12 a \"b\" c 'd' 1.000000 2. 3.000000e+00 4.000000E+00 5 6 true false 0 1 2 3 4 5 6 7 8 9 a 13  "
                  };
          })
      ],
      tl: {
        hd: [
          "width_1",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: Curry._1(Format.asprintf(/* Format */{
                                _0: {
                                  TAG: /* Int */4,
                                  _0: /* Int_d */0,
                                  _1: {
                                    TAG: /* Lit_padding */0,
                                    _0: /* Zeros */2,
                                    _1: 14
                                  },
                                  _2: /* No_precision */0,
                                  _3: /* End_of_format */0
                                },
                                _1: "%014d"
                              }), 32),
                      _1: "00000000000032"
                    };
            })
        ],
        tl: {
          hd: [
            "width_2",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: Curry._1(Format.asprintf(/* Format */{
                                  _0: {
                                    TAG: /* Float */8,
                                    _0: /* Float_f */0,
                                    _1: {
                                      TAG: /* Lit_padding */0,
                                      _0: /* Right */1,
                                      _1: 10
                                    },
                                    _2: /* Lit_precision */{
                                      _0: 3
                                    },
                                    _3: /* End_of_format */0
                                  },
                                  _1: "%10.3f"
                                }), 32333.02),
                        _1: " 32333.020"
                      };
              })
          ],
          tl: {
            hd: [
              "alternate_1",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: Curry._1(Format.asprintf(/* Format */{
                                    _0: {
                                      TAG: /* Int */4,
                                      _0: /* Int_x */6,
                                      _1: {
                                        TAG: /* Lit_padding */0,
                                        _0: /* Right */1,
                                        _1: 0
                                      },
                                      _2: /* No_precision */0,
                                      _3: /* End_of_format */0
                                    },
                                    _1: "%0x"
                                  }), 32333),
                          _1: "7e4d"
                        };
                })
            ],
            tl: {
              hd: [
                "alternate_2",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: Curry._1(Format.asprintf(/* Format */{
                                      _0: {
                                        TAG: /* Int */4,
                                        _0: /* Int_Cx */7,
                                        _1: {
                                          TAG: /* Lit_padding */0,
                                          _0: /* Right */1,
                                          _1: 0
                                        },
                                        _2: /* No_precision */0,
                                        _3: /* End_of_format */0
                                      },
                                      _1: "%#0x"
                                    }), 32333),
                            _1: "0x7e4d"
                          };
                  })
              ],
              tl: {
                hd: [
                  "alternate_3",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: [
                                Curry._1(Format.asprintf(/* Format */{
                                          _0: {
                                            TAG: /* Int */4,
                                            _0: /* Int_Co */11,
                                            _1: /* No_padding */0,
                                            _2: /* No_precision */0,
                                            _3: /* End_of_format */0
                                          },
                                          _1: "%#o"
                                        }), 32),
                                Curry._1(Format.asprintf(/* Format */{
                                          _0: {
                                            TAG: /* Int */4,
                                            _0: /* Int_o */10,
                                            _1: /* No_padding */0,
                                            _2: /* No_precision */0,
                                            _3: /* End_of_format */0
                                          },
                                          _1: "%o"
                                        }), 32)
                              ],
                              _1: [
                                "040",
                                "40"
                              ]
                            };
                    })
                ],
                tl: {
                  hd: [
                    "justify_0",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: Caml_format.caml_format_int("%-8d", 32),
                                _1: "32      "
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "sign_p",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: Curry._1(Format.asprintf(/* Format */{
                                            _0: {
                                              TAG: /* Int */4,
                                              _0: /* Int_pd */1,
                                              _1: {
                                                TAG: /* Lit_padding */0,
                                                _0: /* Right */1,
                                                _1: 4
                                              },
                                              _2: /* No_precision */0,
                                              _3: /* End_of_format */0
                                            },
                                            _1: "%+4d"
                                          }), 32),
                                  _1: " +32"
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "sign_2p",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: Curry._1(Format.asprintf(/* Format */{
                                              _0: {
                                                TAG: /* Int */4,
                                                _0: /* Int_sd */2,
                                                _1: {
                                                  TAG: /* Lit_padding */0,
                                                  _0: /* Right */1,
                                                  _1: 4
                                                },
                                                _2: /* No_precision */0,
                                                _3: /* End_of_format */0
                                              },
                                              _1: "% 4d"
                                            }), 32),
                                    _1: "  32"
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "sign_3p",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: Curry._1(Format.asprintf(/* Format */{
                                                _0: {
                                                  TAG: /* Int32 */5,
                                                  _0: /* Int_u */12,
                                                  _1: /* No_padding */0,
                                                  _2: /* No_precision */0,
                                                  _3: /* End_of_format */0
                                                },
                                                _1: "%lu"
                                              }), -1),
                                      _1: "4294967295"
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "sign_4p",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: Curry._1(Format.asprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* Int32 */5,
                                                    _0: /* Int_d */0,
                                                    _1: /* No_padding */0,
                                                    _2: /* No_precision */0,
                                                    _3: /* End_of_format */0
                                                  },
                                                  _1: "%ld"
                                                }), -1),
                                        _1: "-1"
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "width_3",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: Caml_format.caml_format_int("%032d", 32),
                                          _1: "00000000000000000000000000000032"
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "prec_1",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: Curry._1(Format.asprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* Int */4,
                                                        _0: /* Int_d */0,
                                                        _1: /* No_padding */0,
                                                        _2: /* Lit_precision */{
                                                          _0: 10
                                                        },
                                                        _3: /* End_of_format */0
                                                      },
                                                      _1: "%.10d"
                                                    }), 32),
                                            _1: "0000000032"
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "prec_2",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: Caml_format.caml_format_int("%.10d", 32),
                                              _1: "0000000032"
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "prec_3",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: Caml_format.caml_format_int("%.d", 32),
                                                _1: "32"
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "prec_4",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: Caml_format.caml_format_int("%.d", 32),
                                                  _1: "32"
                                                };
                                        })
                                    ],
                                    tl: /* [] */0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var formatter_suites = {
  hd: formatter_suites_0,
  tl: formatter_suites_1
};

var float_data = [
  [
    "%f",
    32,
    "32.000000"
  ],
  [
    "%f",
    Number.NaN,
    "nan"
  ],
  [
    "%f",
    Pervasives.infinity,
    "inf"
  ],
  [
    "%f",
    Pervasives.neg_infinity,
    "-inf"
  ],
  [
    "%1.e",
    13000,
    "1e+04"
  ],
  [
    "%1.3e",
    2.3e-05,
    "2.300e-05"
  ],
  [
    "%3.10e",
    3e+56,
    "3.0000000000e+56"
  ],
  [
    "%3.10f",
    20000000000,
    "20000000000.0000000000"
  ],
  [
    "%3.3f",
    -3300,
    "-3300.000"
  ],
  [
    "%1.g",
    13000,
    "1e+04"
  ],
  [
    "%1.3g",
    2.3e-05,
    "2.3e-05"
  ],
  [
    "%3.10g",
    3e+56,
    "3e+56"
  ],
  [
    "%3.10g",
    20000000000,
    "2e+10"
  ],
  [
    "%3.3g",
    -3300,
    "-3.3e+03"
  ],
  [
    "%3.3g",
    -0.0033,
    "-0.0033"
  ],
  [
    "%3.10g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.0g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.g",
    30000000000,
    "3e+10"
  ],
  [
    "%3.g",
    3,
    "  3"
  ],
  [
    "%1.1g",
    2.1,
    "2"
  ],
  [
    "%1.2g",
    2.1,
    "2.1"
  ]
];

function ident(ppf, s) {
  return Curry._1(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%s"
                }), s);
}

function kwd(ppf, s) {
  return Curry._1(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* String */2,
                    _0: /* No_padding */0,
                    _1: /* End_of_format */0
                  },
                  _1: "%s"
                }), s);
}

function pr_exp0(ppf, s) {
  switch (s.TAG | 0) {
    case /* Var */1 :
        return Curry._2(Format.fprintf(ppf, /* Format */{
                        _0: {
                          TAG: /* Alpha */15,
                          _0: /* End_of_format */0
                        },
                        _1: "%a"
                      }), ident, s._0);
    case /* Lambda */0 :
    case /* Apply */2 :
        break;
    
  }
  return Curry._2(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* Formatting_gen */18,
                    _0: {
                      TAG: /* Open_box */1,
                      _0: /* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "<1>",
                          _1: /* End_of_format */0
                        },
                        _1: "<1>"
                      }
                    },
                    _1: {
                      TAG: /* Char_literal */12,
                      _0: /* "(" */40,
                      _1: {
                        TAG: /* Alpha */15,
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* ")" */41,
                          _1: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Close_box */0,
                            _1: /* End_of_format */0
                          }
                        }
                      }
                    }
                  },
                  _1: "@[<1>(%a)@]"
                }), pr_lambda, s);
}

function pr_app(ppf, e) {
  return Curry._2(Format.fprintf(ppf, /* Format */{
                  _0: {
                    TAG: /* Formatting_gen */18,
                    _0: {
                      TAG: /* Open_box */1,
                      _0: /* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "<2>",
                          _1: /* End_of_format */0
                        },
                        _1: "<2>"
                      }
                    },
                    _1: {
                      TAG: /* Alpha */15,
                      _0: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Close_box */0,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "@[<2>%a@]"
                }), pr_other_applications, e);
}

function pr_other_applications(ppf, f) {
  switch (f.TAG | 0) {
    case /* Lambda */0 :
    case /* Var */1 :
        return pr_exp0(ppf, f);
    case /* Apply */2 :
        return Curry._4(Format.fprintf(ppf, /* Format */{
                        _0: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Formatting_lit */17,
                            _0: {
                              TAG: /* Break */0,
                              _0: "@ ",
                              _1: 1,
                              _2: 0
                            },
                            _1: {
                              TAG: /* Alpha */15,
                              _0: /* End_of_format */0
                            }
                          }
                        },
                        _1: "%a@ %a"
                      }), pr_app, f._0, pr_exp0, f._1);
    
  }
}

function pr_lambda(ppf, e) {
  switch (e.TAG | 0) {
    case /* Lambda */0 :
        return Curry._8(Format.fprintf(ppf, /* Format */{
                        _0: {
                          TAG: /* Formatting_gen */18,
                          _0: {
                            TAG: /* Open_box */1,
                            _0: /* Format */{
                              _0: {
                                TAG: /* String_literal */11,
                                _0: "<1>",
                                _1: /* End_of_format */0
                              },
                              _1: "<1>"
                            }
                          },
                          _1: {
                            TAG: /* Alpha */15,
                            _0: {
                              TAG: /* Alpha */15,
                              _0: {
                                TAG: /* Alpha */15,
                                _0: {
                                  TAG: /* Formatting_lit */17,
                                  _0: {
                                    TAG: /* Break */0,
                                    _0: "@ ",
                                    _1: 1,
                                    _2: 0
                                  },
                                  _1: {
                                    TAG: /* Alpha */15,
                                    _0: {
                                      TAG: /* Formatting_lit */17,
                                      _0: /* Close_box */0,
                                      _1: /* End_of_format */0
                                    }
                                  }
                                }
                              }
                            }
                          }
                        },
                        _1: "@[<1>%a%a%a@ %a@]"
                      }), kwd, "\\", ident, e._0, kwd, ".", pr_lambda, e._1);
    case /* Var */1 :
    case /* Apply */2 :
        return pr_app(ppf, e);
    
  }
}

var string_of_lambda = Curry._1(Format.asprintf(/* Format */{
          _0: {
            TAG: /* Alpha */15,
            _0: /* End_of_format */0
          },
          _1: "%a"
        }), pr_lambda);

var Lambda_suites = {
  ident: ident,
  kwd: kwd,
  pr_exp0: pr_exp0,
  pr_app: pr_app,
  pr_other_applications: pr_other_applications,
  pr_lambda: pr_lambda,
  string_of_lambda: string_of_lambda
};

var lambda_suites = [
  [
    {
      TAG: /* Var */1,
      _0: "x"
    },
    "x"
  ],
  [
    {
      TAG: /* Apply */2,
      _0: {
        TAG: /* Var */1,
        _0: "x"
      },
      _1: {
        TAG: /* Var */1,
        _0: "y"
      }
    },
    "x y"
  ],
  [
    {
      TAG: /* Lambda */0,
      _0: "z",
      _1: {
        TAG: /* Apply */2,
        _0: {
          TAG: /* Var */1,
          _0: "x"
        },
        _1: {
          TAG: /* Var */1,
          _0: "y"
        }
      }
    },
    "\\z. x y"
  ],
  [
    {
      TAG: /* Lambda */0,
      _0: "z",
      _1: {
        TAG: /* Lambda */0,
        _0: "z",
        _1: {
          TAG: /* Apply */2,
          _0: {
            TAG: /* Var */1,
            _0: "x"
          },
          _1: {
            TAG: /* Var */1,
            _0: "y"
          }
        }
      }
    },
    "\\z. \\z. x y"
  ]
];

function from_lambda_pairs(p) {
  return $$Array.to_list($$Array.mapi((function (i, param) {
                    var b = param[1];
                    var a = param[0];
                    return [
                            Curry._1(Printf.sprintf(/* Format */{
                                      _0: {
                                        TAG: /* String_literal */11,
                                        _0: "lambda_print ",
                                        _1: {
                                          TAG: /* Int */4,
                                          _0: /* Int_d */0,
                                          _1: /* No_padding */0,
                                          _2: /* No_precision */0,
                                          _3: /* End_of_format */0
                                        }
                                      },
                                      _1: "lambda_print %d"
                                    }), i),
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: Curry._1(string_of_lambda, a),
                                        _1: b
                                      };
                              })
                          ];
                  }), lambda_suites));
}

var ksprintf_suites_0 = [
  "ksprintf",
  (function (param) {
      var f = function (fmt) {
        return Format.ksprintf((function (x) {
                      return x + x;
                    }), fmt);
      };
      return {
              TAG: /* Eq */0,
              _0: Curry._2(f(/* Format */{
                        _0: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Char_literal */12,
                            _0: /* " " */32,
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: " a ",
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        },
                        _1: "%s %s a "
                      }), "x", "xx"),
              _1: "x xx a x xx a "
            };
    })
];

var ksprintf_suites_1 = {
  hd: [
    "sprintf",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: Curry._2(Format.sprintf(/* Format */{
                          _0: {
                            TAG: /* String */2,
                            _0: /* No_padding */0,
                            _1: {
                              TAG: /* Char_literal */12,
                              _0: /* " " */32,
                              _1: {
                                TAG: /* Caml_string */3,
                                _0: /* No_padding */0,
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "%s %S"
                        }), "x", "X"),
                _1: "x \"X\""
              };
      })
  ],
  tl: /* [] */0
};

var ksprintf_suites = {
  hd: ksprintf_suites_0,
  tl: ksprintf_suites_1
};

var int64_suites_0 = [
  "i32_simple",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: Curry._1(Format.asprintf(/* Format */{
                        _0: {
                          TAG: /* Nativeint */6,
                          _0: /* Int_x */6,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: /* End_of_format */0
                        },
                        _1: "%nx"
                      }), 4294967295),
              _1: "ffffffff"
            };
    })
];

var int64_suites_1 = {
  hd: [
    "i32_simple1",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: Curry._1(Format.asprintf(/* Format */{
                          _0: {
                            TAG: /* Nativeint */6,
                            _0: /* Int_o */10,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: /* End_of_format */0
                          },
                          _1: "%no"
                        }), 4294967295),
                _1: "37777777777"
              };
      })
  ],
  tl: {
    hd: [
      "i64_simple",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: Curry._1(Format.asprintf(/* Format */{
                            _0: {
                              TAG: /* Int64 */7,
                              _0: /* Int_d */0,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: /* End_of_format */0
                            },
                            _1: "%Ld"
                          }), Caml_int64.mk(3, 0)),
                  _1: "3"
                };
        })
    ],
    tl: {
      hd: [
        "i64_simple2",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: Curry._1(Format.asprintf(/* Format */{
                              _0: {
                                TAG: /* Int64 */7,
                                _0: /* Int_x */6,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: /* End_of_format */0
                              },
                              _1: "%Lx"
                            }), Caml_int64.mk(33, 0)),
                    _1: "21"
                  };
          })
      ],
      tl: {
        hd: [
          "i64_simple3",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: Curry._1(Format.asprintf(/* Format */{
                                _0: {
                                  TAG: /* Int64 */7,
                                  _0: /* Int_i */3,
                                  _1: /* No_padding */0,
                                  _2: /* No_precision */0,
                                  _3: /* End_of_format */0
                                },
                                _1: "%Li"
                              }), Caml_int64.mk(33, 0)),
                      _1: "33"
                    };
            })
        ],
        tl: {
          hd: [
            "i64_simple4",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: Curry._1(Format.asprintf(/* Format */{
                                  _0: {
                                    TAG: /* Int64 */7,
                                    _0: /* Int_X */8,
                                    _1: /* No_padding */0,
                                    _2: /* No_precision */0,
                                    _3: /* End_of_format */0
                                  },
                                  _1: "%LX"
                                }), Caml_int64.mk(44, 0)),
                        _1: "2C"
                      };
              })
          ],
          tl: {
            hd: [
              "i64_simple5",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: Curry._1(Format.asprintf(/* Format */{
                                    _0: {
                                      TAG: /* Int64 */7,
                                      _0: /* Int_x */6,
                                      _1: /* No_padding */0,
                                      _2: /* No_precision */0,
                                      _3: /* End_of_format */0
                                    },
                                    _1: "%Lx"
                                  }), Caml_int64.mk(44, 0)),
                          _1: "2c"
                        };
                })
            ],
            tl: {
              hd: [
                "i64_simple6",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: Curry._2(Format.asprintf(/* Format */{
                                      _0: {
                                        TAG: /* Int64 */7,
                                        _0: /* Int_x */6,
                                        _1: {
                                          TAG: /* Arg_padding */1,
                                          _0: /* Right */1
                                        },
                                        _2: /* No_precision */0,
                                        _3: /* End_of_format */0
                                      },
                                      _1: "%*Lx"
                                    }), 5, Caml_int64.mk(44, 0)),
                            _1: "   2c"
                          };
                  })
              ],
              tl: {
                hd: [
                  "i64_simple7",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: Caml_int64.to_string(Caml_int64.mk(3333, 0)),
                              _1: "3333"
                            };
                    })
                ],
                tl: {
                  hd: [
                    "i64_simple8",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: Curry._2(Format.asprintf(/* Format */{
                                          _0: {
                                            TAG: /* Int64 */7,
                                            _0: /* Int_d */0,
                                            _1: /* No_padding */0,
                                            _2: /* No_precision */0,
                                            _3: {
                                              TAG: /* Int64 */7,
                                              _0: /* Int_d */0,
                                              _1: {
                                                TAG: /* Lit_padding */0,
                                                _0: /* Zeros */2,
                                                _1: 18
                                              },
                                              _2: /* No_precision */0,
                                              _3: /* End_of_format */0
                                            }
                                          },
                                          _1: "%Ld%018Ld"
                                        }), Caml_int64.mk(3, 0), Caml_int64.mk(3, 0)),
                                _1: "3000000000000000003"
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "i64_simple9",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: Curry._2(Format.asprintf(/* Format */{
                                            _0: {
                                              TAG: /* Int64 */7,
                                              _0: /* Int_d */0,
                                              _1: /* No_padding */0,
                                              _2: /* No_precision */0,
                                              _3: {
                                                TAG: /* Int64 */7,
                                                _0: /* Int_d */0,
                                                _1: {
                                                  TAG: /* Lit_padding */0,
                                                  _0: /* Zeros */2,
                                                  _1: 18
                                                },
                                                _2: /* No_precision */0,
                                                _3: /* End_of_format */0
                                              }
                                            },
                                            _1: "%Ld%018Ld"
                                          }), Caml_int64.mk(1548746752, 107288), Caml_int64.zero),
                                  _1: "460800000000000000000000000000000"
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "i64_simple10",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: Curry._1(Format.asprintf(/* Format */{
                                              _0: {
                                                TAG: /* Int64 */7,
                                                _0: /* Int_x */6,
                                                _1: /* No_padding */0,
                                                _2: /* No_precision */0,
                                                _3: /* End_of_format */0
                                              },
                                              _1: "%Lx"
                                            }), Int64.max_int),
                                    _1: "7fffffffffffffff"
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "i64_simple15",
                          (function (param) {
                              return {
                                      TAG: /* Eq */0,
                                      _0: Curry._1(Format.asprintf(/* Format */{
                                                _0: {
                                                  TAG: /* Int64 */7,
                                                  _0: /* Int_d */0,
                                                  _1: /* No_padding */0,
                                                  _2: /* No_precision */0,
                                                  _3: /* End_of_format */0
                                                },
                                                _1: "%Ld"
                                              }), Caml_int64.neg_one),
                                      _1: "-1"
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "i64_simple16",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: Curry._1(Format.asprintf(/* Format */{
                                                  _0: {
                                                    TAG: /* Int64 */7,
                                                    _0: /* Int_d */0,
                                                    _1: /* No_padding */0,
                                                    _2: /* No_precision */0,
                                                    _3: /* End_of_format */0
                                                  },
                                                  _1: "%Ld"
                                                }), Caml_int64.mk(-11111, -1)),
                                        _1: "-11111"
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "i64_simple14",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: Curry._1(Format.asprintf(/* Format */{
                                                    _0: {
                                                      TAG: /* Int64 */7,
                                                      _0: /* Int_X */8,
                                                      _1: /* No_padding */0,
                                                      _2: /* No_precision */0,
                                                      _3: /* End_of_format */0
                                                    },
                                                    _1: "%LX"
                                                  }), Caml_int64.neg_one),
                                          _1: "FFFFFFFFFFFFFFFF"
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "File \"caml_format_test.ml\", line 209, characters 4-11",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: Curry._1(Format.asprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* Int64 */7,
                                                        _0: /* Int_x */6,
                                                        _1: /* No_padding */0,
                                                        _2: /* No_precision */0,
                                                        _3: /* End_of_format */0
                                                      },
                                                      _1: "%Lx"
                                                    }), Caml_int64.neg_one),
                                            _1: "ffffffffffffffff"
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "i64_simple11",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: Curry._1(Format.asprintf(/* Format */{
                                                        _0: {
                                                          TAG: /* Int64 */7,
                                                          _0: /* Int_X */8,
                                                          _1: /* No_padding */0,
                                                          _2: /* No_precision */0,
                                                          _3: /* End_of_format */0
                                                        },
                                                        _1: "%LX"
                                                      }), Int64.max_int),
                                              _1: "7FFFFFFFFFFFFFFF"
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "File \"caml_format_test.ml\", line 217, characters 4-11",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: Curry._1(Format.asprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* Int64 */7,
                                                            _0: /* Int_X */8,
                                                            _1: /* No_padding */0,
                                                            _2: /* No_precision */0,
                                                            _3: /* End_of_format */0
                                                          },
                                                          _1: "%LX"
                                                        }), Int64.min_int),
                                                _1: "8000000000000000"
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "File \"caml_format_test.ml\", line 218, characters 4-11",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: Curry._1(Format.asprintf(/* Format */{
                                                            _0: {
                                                              TAG: /* Int64 */7,
                                                              _0: /* Int_u */12,
                                                              _1: /* No_padding */0,
                                                              _2: /* No_precision */0,
                                                              _3: /* End_of_format */0
                                                            },
                                                            _1: "%Lu"
                                                          }), Caml_int64.neg_one),
                                                  _1: "18446744073709551615"
                                                };
                                        })
                                    ],
                                    tl: {
                                      hd: [
                                        "File \"caml_format_test.ml\", line 222, characters 4-11",
                                        (function (param) {
                                            return {
                                                    TAG: /* Eq */0,
                                                    _0: Curry._1(Format.asprintf(/* Format */{
                                                              _0: {
                                                                TAG: /* Int64 */7,
                                                                _0: /* Int_u */12,
                                                                _1: /* No_padding */0,
                                                                _2: /* No_precision */0,
                                                                _3: /* End_of_format */0
                                                              },
                                                              _1: "%Lu"
                                                            }), Caml_int64.mk(-100, -1)),
                                                    _1: "18446744073709551516"
                                                  };
                                          })
                                      ],
                                      tl: {
                                        hd: [
                                          "File \"caml_format_test.ml\", line 225, characters 4-11",
                                          (function (param) {
                                              return {
                                                      TAG: /* Eq */0,
                                                      _0: Curry._1(Format.asprintf(/* Format */{
                                                                _0: {
                                                                  TAG: /* Int64 */7,
                                                                  _0: /* Int_u */12,
                                                                  _1: /* No_padding */0,
                                                                  _2: /* No_precision */0,
                                                                  _3: /* End_of_format */0
                                                                },
                                                                _1: "%Lu"
                                                              }), Caml_int64.add(Int64.min_int, Caml_int64.one)),
                                                      _1: "9223372036854775809"
                                                    };
                                            })
                                        ],
                                        tl: {
                                          hd: [
                                            "File \"caml_format_test.ml\", line 228, characters 4-11",
                                            (function (param) {
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: Curry._1(Format.asprintf(/* Format */{
                                                                  _0: {
                                                                    TAG: /* Int64 */7,
                                                                    _0: /* Int_u */12,
                                                                    _1: /* No_padding */0,
                                                                    _2: /* No_precision */0,
                                                                    _3: /* End_of_format */0
                                                                  },
                                                                  _1: "%Lu"
                                                                }), Caml_int64.mk(-10000, -1)),
                                                        _1: "18446744073709541616"
                                                      };
                                              })
                                          ],
                                          tl: {
                                            hd: [
                                              "i64_simple19",
                                              (function (param) {
                                                  return {
                                                          TAG: /* Eq */0,
                                                          _0: Curry._1(Format.asprintf(/* Format */{
                                                                    _0: {
                                                                      TAG: /* Int64 */7,
                                                                      _0: /* Int_o */10,
                                                                      _1: /* No_padding */0,
                                                                      _2: /* No_precision */0,
                                                                      _3: /* End_of_format */0
                                                                    },
                                                                    _1: "%Lo"
                                                                  }), Int64.min_int),
                                                          _1: "1000000000000000000000"
                                                        };
                                                })
                                            ],
                                            tl: {
                                              hd: [
                                                "i64_simple13",
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: Curry._1(Format.asprintf(/* Format */{
                                                                      _0: {
                                                                        TAG: /* Int64 */7,
                                                                        _0: /* Int_X */8,
                                                                        _1: /* No_padding */0,
                                                                        _2: /* No_precision */0,
                                                                        _3: /* End_of_format */0
                                                                      },
                                                                      _1: "%LX"
                                                                    }), Caml_int64.add(Int64.min_int, Caml_int64.one)),
                                                            _1: "8000000000000001"
                                                          };
                                                  })
                                              ],
                                              tl: {
                                                hd: [
                                                  "i64_simple20",
                                                  (function (param) {
                                                      return {
                                                              TAG: /* Eq */0,
                                                              _0: Curry._1(Format.asprintf(/* Format */{
                                                                        _0: {
                                                                          TAG: /* Int64 */7,
                                                                          _0: /* Int_x */6,
                                                                          _1: {
                                                                            TAG: /* Lit_padding */0,
                                                                            _0: /* Right */1,
                                                                            _1: 12
                                                                          },
                                                                          _2: /* No_precision */0,
                                                                          _3: /* End_of_format */0
                                                                        },
                                                                        _1: "%12Lx"
                                                                      }), Caml_int64.mk(3, 0)),
                                                              _1: "           3"
                                                            };
                                                    })
                                                ],
                                                tl: {
                                                  hd: [
                                                    "i64_simple21",
                                                    (function (param) {
                                                        return {
                                                                TAG: /* Eq */0,
                                                                _0: Curry._1(Format.asprintf(/* Format */{
                                                                          _0: {
                                                                            TAG: /* Int64 */7,
                                                                            _0: /* Int_X */8,
                                                                            _1: /* No_padding */0,
                                                                            _2: /* No_precision */0,
                                                                            _3: /* End_of_format */0
                                                                          },
                                                                          _1: "%LX"
                                                                        }), Caml_int64.mk(1163551168, 1859194407)),
                                                                _1: "6ED10E27455A61C0"
                                                              };
                                                      })
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "missing_neline",
                                                      (function (param) {
                                                          return {
                                                                  TAG: /* Eq */0,
                                                                  _0: Curry._1(Format.asprintf(/* Format */{
                                                                            _0: {
                                                                              TAG: /* Int64 */7,
                                                                              _0: /* Int_d */0,
                                                                              _1: /* No_padding */0,
                                                                              _2: /* No_precision */0,
                                                                              _3: {
                                                                                TAG: /* Char_literal */12,
                                                                                _0: /* "\n" */10,
                                                                                _1: /* End_of_format */0
                                                                              }
                                                                            },
                                                                            _1: "%Ld\n"
                                                                          }), Caml_int64.mk(32, 0)),
                                                                  _1: "32\n"
                                                                };
                                                        })
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "missing_newline2",
                                                        (function (param) {
                                                            var buf = $$Buffer.create(30);
                                                            return {
                                                                    TAG: /* Eq */0,
                                                                    _0: (Curry._1(Printf.bprintf(buf, /* Format */{
                                                                                _0: {
                                                                                  TAG: /* Int64 */7,
                                                                                  _0: /* Int_d */0,
                                                                                  _1: /* No_padding */0,
                                                                                  _2: /* No_precision */0,
                                                                                  _3: {
                                                                                    TAG: /* Char_literal */12,
                                                                                    _0: /* "\n" */10,
                                                                                    _1: /* End_of_format */0
                                                                                  }
                                                                                },
                                                                                _1: "%Ld\n"
                                                                              }), Caml_int64.mk(32, 0)), $$Buffer.contents(buf)),
                                                                    _1: "32\n"
                                                                  };
                                                          })
                                                      ],
                                                      tl: /* [] */0
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var int64_suites = {
  hd: int64_suites_0,
  tl: int64_suites_1
};

var of_string_data = [
  [
    Caml_int64.zero,
    "0"
  ],
  [
    Caml_int64.mk(3, 0),
    "3"
  ],
  [
    Caml_int64.mk(33, 0),
    "33"
  ],
  [
    Caml_int64.mk(333, 0),
    "33_3"
  ],
  [
    Caml_int64.mk(33333, 0),
    "33_33_3"
  ],
  [
    Caml_int64.mk(-1674115755, 77),
    "333333333333"
  ],
  [
    Caml_int64.neg_one,
    "0xffff_ffff_ffff_ffff"
  ],
  [
    Caml_int64.mk(113, 0),
    "0b01110001"
  ],
  [
    Caml_int64.one,
    "-0xffff_ffff_ffff_ffff"
  ]
];

Mt.from_pair_suites("Caml_format_test", Pervasives.$at(suites, Pervasives.$at(formatter_suites, Pervasives.$at(from_lambda_pairs(lambda_suites), Pervasives.$at(ksprintf_suites, Pervasives.$at($$Array.to_list($$Array.mapi((function (i, param) {
                                    var str_result = param[2];
                                    var f = param[1];
                                    var fmt = param[0];
                                    return [
                                            Curry._1(Printf.sprintf(/* Format */{
                                                      _0: {
                                                        TAG: /* String_literal */11,
                                                        _0: "float_format ",
                                                        _1: {
                                                          TAG: /* Int */4,
                                                          _0: /* Int_d */0,
                                                          _1: /* No_padding */0,
                                                          _2: /* No_precision */0,
                                                          _3: /* End_of_format */0
                                                        }
                                                      },
                                                      _1: "float_format %d"
                                                    }), i),
                                            (function (param) {
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: Caml_format.caml_format_float(fmt, f),
                                                        _1: str_result
                                                      };
                                              })
                                          ];
                                  }), float_data)), Pervasives.$at(int64_suites, $$Array.to_list($$Array.mapi((function (i, param) {
                                        var b = param[1];
                                        var a = param[0];
                                        return [
                                                Curry._1(Printf.sprintf(/* Format */{
                                                          _0: {
                                                            TAG: /* String_literal */11,
                                                            _0: "int64_of_string ",
                                                            _1: {
                                                              TAG: /* Int */4,
                                                              _0: /* Int_d */0,
                                                              _1: /* No_padding */0,
                                                              _2: /* No_precision */0,
                                                              _3: {
                                                                TAG: /* Char_literal */12,
                                                                _0: /* " " */32,
                                                                _1: /* End_of_format */0
                                                              }
                                                            }
                                                          },
                                                          _1: "int64_of_string %d "
                                                        }), i),
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: Caml_format.caml_int64_of_string(b),
                                                            _1: a
                                                          };
                                                  })
                                              ];
                                      }), of_string_data)))))))));

var a = Format.asprintf;

var float_suites = {
  hd: "float_nan",
  tl: /* [] */0
};

var hh = Caml_int64.mk(-858993460, 214748364);

var hhh = Caml_int64.mk(0, 268435456);

exports.of_string = of_string;
exports.from_float_of_string = from_float_of_string;
exports.from_of_string = from_of_string;
exports.u = u;
exports.to_str = to_str;
exports.v = v;
exports.suites = suites;
exports.$caret$caret = $caret$caret;
exports.ff = ff;
exports.a = a;
exports.formatter_suites = formatter_suites;
exports.float_data = float_data;
exports.float_suites = float_suites;
exports.Lambda_suites = Lambda_suites;
exports.lambda_suites = lambda_suites;
exports.from_lambda_pairs = from_lambda_pairs;
exports.ksprintf_suites = ksprintf_suites;
exports.int64_suites = int64_suites;
exports.hh = hh;
exports.hhh = hhh;
exports.of_string_data = of_string_data;
/* v Not a pure module */
