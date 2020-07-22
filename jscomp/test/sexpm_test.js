'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Sexpm = require("./sexpm.js");
var Format = require("../../lib/js/format.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function print_or_error(fmt, x) {
  if (x.HASH === "Error") {
    return Curry._1(Format.fprintf(fmt, /* Format */{
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
                        TAG: /* String_literal */11,
                        _0: "Error:",
                        _1: {
                          TAG: /* String */2,
                          _0: /* No_padding */0,
                          _1: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Close_box */0,
                            _1: {
                              TAG: /* Formatting_lit */17,
                              _0: /* Flush_newline */4,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    },
                    _1: "@[Error:%s@]@."
                  }), x.VAL);
  } else {
    return Curry._2(Format.fprintf(fmt, /* Format */{
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
                        TAG: /* String_literal */11,
                        _0: "Ok:",
                        _1: {
                          TAG: /* Alpha */15,
                          _0: {
                            TAG: /* Formatting_lit */17,
                            _0: /* Close_box */0,
                            _1: {
                              TAG: /* Formatting_lit */17,
                              _0: /* Flush_newline */4,
                              _1: /* End_of_format */0
                            }
                          }
                        }
                      }
                    },
                    _1: "@[Ok:%a@]@."
                  }), Sexpm.print, x.VAL);
  }
}

var a = Sexpm.parse_string("(x x gh 3 3)");

eq("File \"sexpm_test.ml\", line 17, characters 7-14", [
      {
        HASH: "Ok",
        VAL: {
          HASH: "List",
          VAL: {
            hd: {
              HASH: "Atom",
              VAL: "x"
            },
            tl: {
              hd: {
                HASH: "Atom",
                VAL: "x"
              },
              tl: {
                hd: {
                  HASH: "Atom",
                  VAL: "gh"
                },
                tl: {
                  hd: {
                    HASH: "Atom",
                    VAL: "3"
                  },
                  tl: {
                    hd: {
                      HASH: "Atom",
                      VAL: "3"
                    },
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      },
      a
    ]);

eq("File \"sexpm_test.ml\", line 21, characters 7-14", [
      Curry._2(Format.asprintf(/* Format */{
                  _0: {
                    TAG: /* Alpha */15,
                    _0: /* End_of_format */0
                  },
                  _1: "%a"
                }), print_or_error, a).trim(),
      "Ok:(x x gh 3 3)\n".trim()
    ]);

Mt.from_pair_suites("Sexpm_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.print_or_error = print_or_error;
/* a Not a pure module */
