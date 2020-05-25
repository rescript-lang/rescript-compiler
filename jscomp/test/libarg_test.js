'use strict';

var Mt = require("./mt.js");
var Arg = require("../../lib/js/arg.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Printf = require("../../lib/js/printf.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var current = {
  contents: 0
};

var accum = {
  contents: /* [] */0
};

function record(fmt) {
  return Printf.kprintf((function (s) {
                accum.contents = /* :: */{
                  _0: s,
                  _1: accum.contents
                };
                
              }), fmt);
}

function f_unit(param) {
  return record(/* Format */{
              _0: {
                tag: /* String_literal */11,
                _0: "unit()",
                _1: /* End_of_format */0
              },
              _1: "unit()"
            });
}

function f_bool(b) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "bool(",
                    _1: {
                      tag: /* Bool */9,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "bool(%b)"
                }), b);
}

var r_set = {
  contents: false
};

var r_clear = {
  contents: true
};

function f_string(s) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "string(",
                    _1: {
                      tag: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "string(%s)"
                }), s);
}

var r_string = {
  contents: ""
};

function f_int(i) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "int(",
                    _1: {
                      tag: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "int(%d)"
                }), i);
}

var r_int = {
  contents: 0
};

function f_float(f) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "float(",
                    _1: {
                      tag: /* Float */8,
                      _0: /* Float_g */9,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "float(%g)"
                }), f);
}

var r_float = {
  contents: 0.0
};

function f_symbol(s) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "symbol(",
                    _1: {
                      tag: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "symbol(%s)"
                }), s);
}

function f_rest(s) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "rest(",
                    _1: {
                      tag: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "rest(%s)"
                }), s);
}

function f_anon(s) {
  return Curry._1(record(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "anon(",
                    _1: {
                      tag: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* Char_literal */12,
                        _0: /* ")" */41,
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "anon(%s)"
                }), s);
}

var spec_0 = /* tuple */[
  "-u",
  {
    tag: /* Unit */0,
    _0: f_unit
  },
  "Unit (0)"
];

var spec_1 = /* :: */{
  _0: /* tuple */[
    "-b",
    {
      tag: /* Bool */1,
      _0: f_bool
    },
    "Bool (1)"
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "-s",
      {
        tag: /* Set */2,
        _0: r_set
      },
      "Set (0)"
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "-c",
        {
          tag: /* Clear */3,
          _0: r_clear
        },
        "Clear (0)"
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "-str",
          {
            tag: /* String */4,
            _0: f_string
          },
          "String (1)"
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "-sstr",
            {
              tag: /* Set_string */5,
              _0: r_string
            },
            "Set_string (1)"
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "-i",
              {
                tag: /* Int */6,
                _0: f_int
              },
              "Int (1)"
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "-si",
                {
                  tag: /* Set_int */7,
                  _0: r_int
                },
                "Set_int (1)"
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "-f",
                  {
                    tag: /* Float */8,
                    _0: f_float
                  },
                  "Float (1)"
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "-sf",
                    {
                      tag: /* Set_float */9,
                      _0: r_float
                    },
                    "Set_float (1)"
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "-t",
                      {
                        tag: /* Tuple */10,
                        _0: /* :: */{
                          _0: {
                            tag: /* Bool */1,
                            _0: f_bool
                          },
                          _1: /* :: */{
                            _0: {
                              tag: /* String */4,
                              _0: f_string
                            },
                            _1: /* :: */{
                              _0: {
                                tag: /* Int */6,
                                _0: f_int
                              },
                              _1: /* [] */0
                            }
                          }
                        }
                      },
                      "Tuple (3)"
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "-sym",
                        {
                          tag: /* Symbol */11,
                          _0: /* :: */{
                            _0: "a",
                            _1: /* :: */{
                              _0: "b",
                              _1: /* :: */{
                                _0: "c",
                                _1: /* [] */0
                              }
                            }
                          },
                          _1: f_symbol
                        },
                        "Symbol (1)"
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "-rest",
                          {
                            tag: /* Rest */12,
                            _0: f_rest
                          },
                          "Rest (*)"
                        ],
                        _1: /* [] */0
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

var spec = /* :: */{
  _0: spec_0,
  _1: spec_1
};

var args1 = [
  "prog",
  "anon1",
  "-u",
  "-b",
  "true",
  "-s",
  "anon2",
  "-c",
  "-str",
  "foo",
  "-sstr",
  "bar",
  "-i",
  "19",
  "-si",
  "42",
  "-f",
  "3.14",
  "-sf",
  "2.72",
  "anon3",
  "-t",
  "false",
  "gee",
  "1436",
  "-sym",
  "c",
  "anon4",
  "-rest",
  "r1",
  "r2"
];

var args2 = [
  "prog",
  "anon1",
  "-u",
  "-b=true",
  "-s",
  "anon2",
  "-c",
  "-str=foo",
  "-sstr=bar",
  "-i=19",
  "-si=42",
  "-f=3.14",
  "-sf=2.72",
  "anon3",
  "-t",
  "false",
  "gee",
  "1436",
  "-sym=c",
  "anon4",
  "-rest",
  "r1",
  "r2"
];

function error(s) {
  return Curry._1(Printf.printf(/* Format */{
                  _0: {
                    tag: /* String_literal */11,
                    _0: "error (",
                    _1: {
                      tag: /* String */2,
                      _0: /* No_padding */0,
                      _1: {
                        tag: /* String_literal */11,
                        _0: ")\n",
                        _1: /* End_of_format */0
                      }
                    }
                  },
                  _1: "error (%s)\n"
                }), s);
}

function check(r, v, msg) {
  if (Caml_obj.caml_notequal(r.contents, v)) {
    return error(msg);
  }
  
}

function test(argv) {
  current.contents = 0;
  r_set.contents = false;
  r_clear.contents = true;
  r_string.contents = "";
  r_int.contents = 0;
  r_float.contents = 0.0;
  accum.contents = /* [] */0;
  Arg.parse_argv(current, argv, spec, f_anon, "usage");
  var result = List.rev(accum.contents);
  var reference = /* :: */{
    _0: "anon(anon1)",
    _1: /* :: */{
      _0: "unit()",
      _1: /* :: */{
        _0: "bool(true)",
        _1: /* :: */{
          _0: "anon(anon2)",
          _1: /* :: */{
            _0: "string(foo)",
            _1: /* :: */{
              _0: "int(19)",
              _1: /* :: */{
                _0: "float(3.14)",
                _1: /* :: */{
                  _0: "anon(anon3)",
                  _1: /* :: */{
                    _0: "bool(false)",
                    _1: /* :: */{
                      _0: "string(gee)",
                      _1: /* :: */{
                        _0: "int(1436)",
                        _1: /* :: */{
                          _0: "symbol(c)",
                          _1: /* :: */{
                            _0: "anon(anon4)",
                            _1: /* :: */{
                              _0: "rest(r1)",
                              _1: /* :: */{
                                _0: "rest(r2)",
                                _1: /* [] */0
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
  if (Caml_obj.caml_notequal(result, reference)) {
    var f = function (x, y) {
      return Curry._3(Printf.printf(/* Format */{
                      _0: {
                        tag: /* String */2,
                        _0: {
                          tag: /* Lit_padding */0,
                          _0: /* Right */1,
                          _1: 20
                        },
                        _1: {
                          tag: /* Char_literal */12,
                          _0: /* " " */32,
                          _1: {
                            tag: /* Char */0,
                            _0: {
                              tag: /* Char_literal */12,
                              _0: /* " " */32,
                              _1: {
                                tag: /* String */2,
                                _0: {
                                  tag: /* Lit_padding */0,
                                  _0: /* Left */0,
                                  _1: 20
                                },
                                _1: {
                                  tag: /* Char_literal */12,
                                  _0: /* "\n" */10,
                                  _1: {
                                    tag: /* Flush */10,
                                    _0: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          }
                        }
                      },
                      _1: "%20s %c %-20s\n%!"
                    }), x, x === y ? /* "=" */61 : /* "#" */35, y);
    };
    List.iter2(f, result, reference);
  }
  check(r_set, true, "Set");
  check(r_clear, false, "Clear");
  check(r_string, "bar", "Set_string");
  check(r_int, 42, "Set_int");
  return check(r_float, 2.72, "Set_float");
}

test(args1);

test(args2);

Mt.from_pair_suites("Libarg_test", /* [] */0);

var suites = /* [] */0;

exports.current = current;
exports.accum = accum;
exports.record = record;
exports.f_unit = f_unit;
exports.f_bool = f_bool;
exports.r_set = r_set;
exports.r_clear = r_clear;
exports.f_string = f_string;
exports.r_string = r_string;
exports.f_int = f_int;
exports.r_int = r_int;
exports.f_float = f_float;
exports.r_float = r_float;
exports.f_symbol = f_symbol;
exports.f_rest = f_rest;
exports.f_anon = f_anon;
exports.spec = spec;
exports.args1 = args1;
exports.args2 = args2;
exports.error = error;
exports.check = check;
exports.test = test;
exports.suites = suites;
/*  Not a pure module */
