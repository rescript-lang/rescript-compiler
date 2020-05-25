'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Caml_string = require("../../lib/js/caml_string.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

console.log("你好，\n世界");

console.log("\x3f\u003f\b\t\n\v\f\r\0\"\'");

function convert(s) {
  return $$Array.to_list(Array.from(s, (function (x) {
                    var x$1 = x.codePointAt(0);
                    if (x$1 !== undefined) {
                      return x$1;
                    }
                    throw {
                          RE_EXN_ID: "Assert_failure",
                          _1: [
                            "chn_test.ml",
                            20,
                            18
                          ],
                          Error: new Error()
                        };
                  })));
}

eq("File \"chn_test.ml\", line 25, characters 7-14", "你好，\n世界", "你好，\n世界");

eq("File \"chn_test.ml\", line 27, characters 7-14", convert("汉字是世界上最美丽的character"), /* :: */{
      _0: 27721,
      _1: /* :: */{
        _0: 23383,
        _1: /* :: */{
          _0: 26159,
          _1: /* :: */{
            _0: 19990,
            _1: /* :: */{
              _0: 30028,
              _1: /* :: */{
                _0: 19978,
                _1: /* :: */{
                  _0: 26368,
                  _1: /* :: */{
                    _0: 32654,
                    _1: /* :: */{
                      _0: 20029,
                      _1: /* :: */{
                        _0: 30340,
                        _1: /* :: */{
                          _0: 99,
                          _1: /* :: */{
                            _0: 104,
                            _1: /* :: */{
                              _0: 97,
                              _1: /* :: */{
                                _0: 114,
                                _1: /* :: */{
                                  _0: 97,
                                  _1: /* :: */{
                                    _0: 99,
                                    _1: /* :: */{
                                      _0: 116,
                                      _1: /* :: */{
                                        _0: 101,
                                        _1: /* :: */{
                                          _0: 114,
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
            }
          }
        }
      }
    });

eq("File \"chn_test.ml\", line 48, characters 5-12", convert("\x3f\x3fa"), /* :: */{
      _0: 63,
      _1: /* :: */{
        _0: 63,
        _1: /* :: */{
          _0: 97,
          _1: /* [] */0
        }
      }
    });

eq("File \"chn_test.ml\", line 50, characters 5-12", convert("??a"), /* :: */{
      _0: 63,
      _1: /* :: */{
        _0: 63,
        _1: /* :: */{
          _0: 97,
          _1: /* [] */0
        }
      }
    });

eq("File \"chn_test.ml\", line 52, characters 5-12", convert("\u003f\x3fa"), /* :: */{
      _0: 63,
      _1: /* :: */{
        _0: 63,
        _1: /* :: */{
          _0: 97,
          _1: /* [] */0
        }
      }
    });

eq("File \"chn_test.ml\", line 54, characters 5-12", convert("🚀🚀a"), /* :: */{
      _0: 128640,
      _1: /* :: */{
        _0: 128640,
        _1: /* :: */{
          _0: 97,
          _1: /* [] */0
        }
      }
    });

eq("File \"chn_test.ml\", line 56, characters 5-12", convert("\uD83D\uDE80a"), /* :: */{
      _0: 128640,
      _1: /* :: */{
        _0: 97,
        _1: /* [] */0
      }
    });

eq("File \"chn_test.ml\", line 58, characters 5-12", convert("\uD83D\uDE80\x3f"), /* :: */{
      _0: 128640,
      _1: /* :: */{
        _0: 63,
        _1: /* [] */0
      }
    });

eq("File \"chn_test.ml\", line 63, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80a"), /* :: */{
      _0: 128640,
      _1: /* :: */{
        _0: 128640,
        _1: /* :: */{
          _0: 97,
          _1: /* [] */0
        }
      }
    });

eq("No inline string length", "\uD83D\uDE80\0".length, 3);

eq("No inline string access", Caml_string.get("\uD83D\uDE80\0", 0) & 255, 61);

eq("File \"chn_test.ml\", line 79, characters 5-12", convert("\uD83D\uDE80"), /* :: */{
      _0: 128640,
      _1: /* [] */0
    });

eq("File \"chn_test.ml\", line 81, characters 5-12", convert("\uD83D\uDE80\uD83D\uDE80"), /* :: */{
      _0: 128640,
      _1: /* :: */{
        _0: 128640,
        _1: /* [] */0
      }
    });

eq("File \"chn_test.ml\", line 82, characters 5-12", convert(" \b\t\n\v\f\ra"), /* :: */{
      _0: 32,
      _1: /* :: */{
        _0: 8,
        _1: /* :: */{
          _0: 9,
          _1: /* :: */{
            _0: 10,
            _1: /* :: */{
              _0: 11,
              _1: /* :: */{
                _0: 12,
                _1: /* :: */{
                  _0: 13,
                  _1: /* :: */{
                    _0: 97,
                    _1: /* [] */0
                  }
                }
              }
            }
          }
        }
      }
    });

eq("File \"chn_test.ml\", line 89, characters 6-13", convert(" \b\t\n\v\f\r\"\'\\\0a"), /* :: */{
      _0: 32,
      _1: /* :: */{
        _0: 8,
        _1: /* :: */{
          _0: 9,
          _1: /* :: */{
            _0: 10,
            _1: /* :: */{
              _0: 11,
              _1: /* :: */{
                _0: 12,
                _1: /* :: */{
                  _0: 13,
                  _1: /* :: */{
                    _0: 34,
                    _1: /* :: */{
                      _0: 39,
                      _1: /* :: */{
                        _0: 92,
                        _1: /* :: */{
                          _0: 0,
                          _1: /* :: */{
                            _0: 97,
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
    });

Mt.from_pair_suites("Chn_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.convert = convert;
/*  Not a pure module */
