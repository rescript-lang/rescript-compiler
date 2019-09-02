'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Caml_string = require("../../lib/js/caml_string.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

console.log("你好，\n世界");

console.log("\x3f\u003f\b\t\n\v\f\r\0\"\'");

function convert(s) {
  return $$Array.to_list(Array.from(s, (function (x) {
                    var match = x.codePointAt(0);
                    if (match !== undefined) {
                      return match;
                    } else {
                      throw [
                            Caml_builtin_exceptions.assert_failure,
                            /* tuple */[
                              "chn_test.ml",
                              20,
                              18
                            ]
                          ];
                    }
                  })));
}

eq("File \"chn_test.ml\", line 25, characters 7-14", "你好，\n世界", "你好，\n世界");

eq("File \"chn_test.ml\", line 27, characters 7-14", convert("汉字是世界上最美丽的character"), /* constructor */{
      tag: "::",
      Arg0: 27721,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 23383,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 26159,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 19990,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 30028,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 19978,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 26368,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 32654,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 20029,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 30340,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 99,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 104,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 97,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 114,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 97,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: 99,
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: 116,
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: 101,
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: 114,
                                          Arg1: "[]"
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
    });

eq("File \"chn_test.ml\", line 48, characters 5-12", convert("\x3f\x3fa"), /* constructor */{
      tag: "::",
      Arg0: 63,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 63,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 97,
          Arg1: "[]"
        }
      }
    });

eq("File \"chn_test.ml\", line 50, characters 5-12", convert("??a"), /* constructor */{
      tag: "::",
      Arg0: 63,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 63,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 97,
          Arg1: "[]"
        }
      }
    });

eq("File \"chn_test.ml\", line 52, characters 5-12", convert("\u003f\x3fa"), /* constructor */{
      tag: "::",
      Arg0: 63,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 63,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 97,
          Arg1: "[]"
        }
      }
    });

eq("File \"chn_test.ml\", line 54, characters 5-12", convert("🚀🚀a"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 128640,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 97,
          Arg1: "[]"
        }
      }
    });

eq("File \"chn_test.ml\", line 56, characters 5-12", convert("\uD83D\uDE80a"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 97,
        Arg1: "[]"
      }
    });

eq("File \"chn_test.ml\", line 58, characters 5-12", convert("\uD83D\uDE80\x3f"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 63,
        Arg1: "[]"
      }
    });

eq("File \"chn_test.ml\", line 63, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80a"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 128640,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 97,
          Arg1: "[]"
        }
      }
    });

eq("No inline string length", "\uD83D\uDE80\0".length, 3);

eq("No inline string access", Caml_string.get("\uD83D\uDE80\0", 0) & 255, 61);

eq("File \"chn_test.ml\", line 79, characters 5-12", convert("\uD83D\uDE80"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: "[]"
    });

eq("File \"chn_test.ml\", line 81, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80"), /* constructor */{
      tag: "::",
      Arg0: 128640,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 128640,
        Arg1: "[]"
      }
    });

eq("File \"chn_test.ml\", line 82, characters 5-12", convert(" \b\t\n\v\f\ra"), /* constructor */{
      tag: "::",
      Arg0: 32,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 8,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 9,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 10,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 11,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 12,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 13,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 97,
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }
      }
    });

eq("File \"chn_test.ml\", line 89, characters 6-13", convert(" \b\t\n\v\f\r\"\'\\\0a"), /* constructor */{
      tag: "::",
      Arg0: 32,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 8,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 9,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 10,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 11,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 12,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 13,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 34,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 39,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 92,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 0,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 97,
                            Arg1: "[]"
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
    });

Mt.from_pair_suites("Chn_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.convert = convert;
/*  Not a pure module */
