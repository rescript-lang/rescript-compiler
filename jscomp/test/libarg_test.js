'use strict';

var Mt = require("./mt.js");
var Arg = require("../../lib/js/arg.js");
var List = require("../../lib/js/list.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var current = {
  contents: 0
};

var accum = {
  contents: /* [] */0
};

function record(fmt) {
  accum.contents = {
    hd: fmt,
    tl: accum.contents
  };
  
}

function f_unit(param) {
  return record("unit()");
}

function f_bool(b) {
  return record("bool(" + b + ")");
}

var r_set = {
  contents: false
};

var r_clear = {
  contents: true
};

function f_string(s) {
  return record("string(" + s + ")");
}

var r_string = {
  contents: ""
};

function f_int(i) {
  return record("int(" + i + ")");
}

var r_int = {
  contents: 0
};

function f_float(f) {
  return record("float(" + f + ")");
}

var r_float = {
  contents: 0.0
};

function f_symbol(s) {
  return record("symbol(" + s + ")");
}

function f_rest(s) {
  return record("rest(" + s + ")");
}

function f_anon(s) {
  return record("anon(" + s + ")");
}

var spec_0 = [
  "-u",
  {
    TAG: /* Unit */0,
    _0: f_unit
  },
  "Unit (0)"
];

var spec_1 = {
  hd: [
    "-b",
    {
      TAG: /* Bool */1,
      _0: f_bool
    },
    "Bool (1)"
  ],
  tl: {
    hd: [
      "-s",
      {
        TAG: /* Set */2,
        _0: r_set
      },
      "Set (0)"
    ],
    tl: {
      hd: [
        "-c",
        {
          TAG: /* Clear */3,
          _0: r_clear
        },
        "Clear (0)"
      ],
      tl: {
        hd: [
          "-str",
          {
            TAG: /* String */4,
            _0: f_string
          },
          "String (1)"
        ],
        tl: {
          hd: [
            "-sstr",
            {
              TAG: /* Set_string */5,
              _0: r_string
            },
            "Set_string (1)"
          ],
          tl: {
            hd: [
              "-i",
              {
                TAG: /* Int */6,
                _0: f_int
              },
              "Int (1)"
            ],
            tl: {
              hd: [
                "-si",
                {
                  TAG: /* Set_int */7,
                  _0: r_int
                },
                "Set_int (1)"
              ],
              tl: {
                hd: [
                  "-f",
                  {
                    TAG: /* Float */8,
                    _0: f_float
                  },
                  "Float (1)"
                ],
                tl: {
                  hd: [
                    "-sf",
                    {
                      TAG: /* Set_float */9,
                      _0: r_float
                    },
                    "Set_float (1)"
                  ],
                  tl: {
                    hd: [
                      "-t",
                      {
                        TAG: /* Tuple */10,
                        _0: {
                          hd: {
                            TAG: /* Bool */1,
                            _0: f_bool
                          },
                          tl: {
                            hd: {
                              TAG: /* String */4,
                              _0: f_string
                            },
                            tl: {
                              hd: {
                                TAG: /* Int */6,
                                _0: f_int
                              },
                              tl: /* [] */0
                            }
                          }
                        }
                      },
                      "Tuple (3)"
                    ],
                    tl: {
                      hd: [
                        "-sym",
                        {
                          TAG: /* Symbol */11,
                          _0: {
                            hd: "a",
                            tl: {
                              hd: "b",
                              tl: {
                                hd: "c",
                                tl: /* [] */0
                              }
                            }
                          },
                          _1: f_symbol
                        },
                        "Symbol (1)"
                      ],
                      tl: {
                        hd: [
                          "-rest",
                          {
                            TAG: /* Rest */12,
                            _0: f_rest
                          },
                          "Rest (*)"
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
};

var spec = {
  hd: spec_0,
  tl: spec_1
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
  console.log("error (%s)");
  
}

function check(r, v, msg) {
  if (Caml_obj.caml_notequal(r.contents, v)) {
    console.log("error (%s)");
    return ;
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
  var reference = {
    hd: "anon(anon1)",
    tl: {
      hd: "unit()",
      tl: {
        hd: "bool(true)",
        tl: {
          hd: "anon(anon2)",
          tl: {
            hd: "string(foo)",
            tl: {
              hd: "int(19)",
              tl: {
                hd: "float(3.14)",
                tl: {
                  hd: "anon(anon3)",
                  tl: {
                    hd: "bool(false)",
                    tl: {
                      hd: "string(gee)",
                      tl: {
                        hd: "int(1436)",
                        tl: {
                          hd: "symbol(c)",
                          tl: {
                            hd: "anon(anon4)",
                            tl: {
                              hd: "rest(r1)",
                              tl: {
                                hd: "rest(r2)",
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
  };
  if (Caml_obj.caml_notequal(result, reference)) {
    var f = function (x, y) {
      console.log(x, y);
      
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
