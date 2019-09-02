'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Stream = require("../../lib/js/stream.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function classify(chr) {
  if ((chr & 128) === 0) {
    return /* constructor */{
            tag: "Single",
            Arg0: chr
          };
  } else if ((chr & 64) === 0) {
    return /* constructor */{
            tag: "Cont",
            Arg0: chr & 63
          };
  } else if ((chr & 32) === 0) {
    return /* constructor */{
            tag: "Leading",
            Arg0: 1,
            Arg1: chr & 31
          };
  } else if ((chr & 16) === 0) {
    return /* constructor */{
            tag: "Leading",
            Arg0: 2,
            Arg1: chr & 15
          };
  } else if ((chr & 8) === 0) {
    return /* constructor */{
            tag: "Leading",
            Arg0: 3,
            Arg1: chr & 7
          };
  } else if ((chr & 4) === 0) {
    return /* constructor */{
            tag: "Leading",
            Arg0: 4,
            Arg1: chr & 3
          };
  } else if ((chr & 2) === 0) {
    return /* constructor */{
            tag: "Leading",
            Arg0: 5,
            Arg1: chr & 1
          };
  } else {
    return "Invalid";
  }
}

function utf8_decode(strm) {
  return Stream.slazy((function (param) {
                var match = Stream.peek(strm);
                if (match !== undefined) {
                  Stream.junk(strm);
                  var match$1 = classify(match);
                  if (typeof match$1 === "string") {
                    throw [
                          Stream.$$Error,
                          "Invalid byte"
                        ];
                  } else {
                    switch (/* XXX */match$1.tag) {
                      case "Single" :
                          return Stream.icons(match$1.Arg0, utf8_decode(strm));
                      case "Cont" :
                          throw [
                                Stream.$$Error,
                                "Unexpected continuation byte"
                              ];
                      case "Leading" :
                          var follow = function (strm, _n, _c) {
                            while(true) {
                              var c = _c;
                              var n = _n;
                              if (n === 0) {
                                return c;
                              } else {
                                var match = classify(Stream.next(strm));
                                if (typeof match === "string") {
                                  throw [
                                        Stream.$$Error,
                                        "Continuation byte expected"
                                      ];
                                } else if (/* XXX */match.tag === "Cont") {
                                  _c = (c << 6) | match.Arg0 & 63;
                                  _n = n - 1 | 0;
                                  continue ;
                                } else {
                                  throw [
                                        Stream.$$Error,
                                        "Continuation byte expected"
                                      ];
                                }
                              }
                            };
                          };
                          return Stream.icons(follow(strm, match$1.Arg0, match$1.Arg1), utf8_decode(strm));
                      
                    }
                  }
                } else {
                  return Stream.sempty;
                }
              }));
}

function to_list(xs) {
  var v = /* record */[/* contents */"[]"];
  Stream.iter((function (x) {
          v[0] = /* constructor */{
            tag: "::",
            Arg0: x,
            Arg1: v[0]
          };
          return /* () */0;
        }), xs);
  return List.rev(v[0]);
}

function utf8_list(s) {
  return to_list(utf8_decode(Stream.of_string(s)));
}

function decode(bytes, offset) {
  var offset$1 = offset;
  var match = classify(Caml_bytes.get(bytes, offset$1));
  if (typeof match === "string") {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "decode"
        ];
  } else {
    switch (/* XXX */match.tag) {
      case "Single" :
          return /* tuple */[
                  match.Arg0,
                  offset$1 + 1 | 0
                ];
      case "Cont" :
          throw [
                Caml_builtin_exceptions.invalid_argument,
                "decode"
              ];
      case "Leading" :
          var _n = match.Arg0;
          var _c = match.Arg1;
          var _offset = offset$1 + 1 | 0;
          while(true) {
            var offset$2 = _offset;
            var c = _c;
            var n = _n;
            if (n === 0) {
              return /* tuple */[
                      c,
                      offset$2
                    ];
            } else {
              var match$1 = classify(Caml_bytes.get(bytes, offset$2));
              if (typeof match$1 === "string") {
                throw [
                      Caml_builtin_exceptions.invalid_argument,
                      "decode"
                    ];
              } else if (/* XXX */match$1.tag === "Cont") {
                _offset = offset$2 + 1 | 0;
                _c = (c << 6) | match$1.Arg0 & 63;
                _n = n - 1 | 0;
                continue ;
              } else {
                throw [
                      Caml_builtin_exceptions.invalid_argument,
                      "decode"
                    ];
              }
            }
          };
      
    }
  }
}

function eq_list(cmp, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs !== "[]") {
      if (ys !== "[]" && Curry._2(cmp, xs.Arg0, ys.Arg0)) {
        _ys = ys.Arg1;
        _xs = xs.Arg1;
        continue ;
      } else {
        return false;
      }
    } else {
      return ys === "[]";
    }
  };
}

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  console.log(/* tuple */[
        x,
        y
      ]);
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

List.iter((function (param) {
        return eq("File \"utf8_decode_test.ml\", line 107, characters 7-14", /* tuple */[
                    true,
                    eq_list(Caml_obj.caml_equal, to_list(utf8_decode(Stream.of_string(param[0]))), param[1])
                  ]);
      }), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "\xe4\xbd\xa0\xe5\xa5\xbdBuckleScript,\xe6\x9c\x80\xe5\xa5\xbd\xe7\x9a\x84JS\xe8\xaf\xad\xe8\xa8\x80",
        /* constructor */{
          tag: "::",
          Arg0: 20320,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: 22909,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 66,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 117,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 99,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 107,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 108,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 101,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 83,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 99,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 114,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 105,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 112,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: 116,
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: 44,
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: 26368,
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: 22909,
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: 30340,
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: 74,
                                              Arg1: /* constructor */{
                                                tag: "::",
                                                Arg0: 83,
                                                Arg1: /* constructor */{
                                                  tag: "::",
                                                  Arg0: 35821,
                                                  Arg1: /* constructor */{
                                                    tag: "::",
                                                    Arg0: 35328,
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
              }
            }
          }
        }
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "hello \xe4\xbd\xa0\xe5\xa5\xbd\xef\xbc\x8c\xe4\xb8\xad\xe5\x8d\x8e\xe6\xb0\x91\xe6\x97\x8f hei",
          /* constructor */{
            tag: "::",
            Arg0: 104,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 101,
              Arg1: /* constructor */{
                tag: "::",
                Arg0: 108,
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 108,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 111,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 32,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 20320,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 22909,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 65292,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 20013,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 21326,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 27665,
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: 26063,
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: 32,
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: 104,
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: 101,
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: 105,
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
        ],
        Arg1: "[]"
      }
    });

Mt.from_pair_suites("Utf8_decode_test", suites[0]);

exports.classify = classify;
exports.utf8_decode = utf8_decode;
exports.to_list = to_list;
exports.utf8_list = utf8_list;
exports.decode = decode;
exports.eq_list = eq_list;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
