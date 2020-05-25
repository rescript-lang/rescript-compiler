'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_0 = [
  "captures",
  (function (param) {
      var re = /(\d+)-(?:(\d+))?/g;
      var result = re.exec("3-");
      if (result === null) {
        return {
                TAG: /* Fail */8,
                _0: undefined
              };
      }
      var defined = Caml_array.caml_array_get(result, 1);
      var $$undefined = Caml_array.caml_array_get(result, 2);
      return {
              TAG: /* Eq */0,
              _0: [
                "3",
                null
              ],
              _1: [
                defined,
                $$undefined
              ]
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "fromString",
    (function (param) {
        var contentOf = function (tag, xmlString) {
          var result = new RegExp("<" + (tag + (">(.*?)<\\/" + (tag + ">")))).exec(xmlString);
          if (result !== null) {
            return Caml_option.nullable_to_opt(Caml_array.caml_array_get(result, 1));
          }
          
        };
        return {
                TAG: /* Eq */0,
                _0: contentOf("div", "<div>Hi</div>"),
                _1: "Hi"
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "exec_literal",
      (function (param) {
          var res = /[^.]+/.exec("http://xxx.domain.com");
          if (res !== null) {
            return {
                    TAG: /* Eq */0,
                    _0: "http://xxx",
                    _1: Caml_array.caml_array_get(res, 0)
                  };
          } else {
            return {
                    TAG: /* FailWith */9,
                    _0: "regex should match"
                  };
          }
        })
    ],
    _1: /* :: */{
      _0: [
        "exec_no_match",
        (function (param) {
            var match = /https:\/\/(.*)/.exec("http://xxx.domain.com");
            if (match !== null) {
              return {
                      TAG: /* FailWith */9,
                      _0: "regex should not match"
                    };
            } else {
              return {
                      TAG: /* Ok */4,
                      _0: true
                    };
            }
          })
      ],
      _1: /* :: */{
        _0: [
          "test_str",
          (function (param) {
              var res = new RegExp("foo").test("#foo#");
              return {
                      TAG: /* Eq */0,
                      _0: true,
                      _1: res
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "fromStringWithFlags",
            (function (param) {
                var res = new RegExp("foo", "g");
                return {
                        TAG: /* Eq */0,
                        _0: true,
                        _1: res.global
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "result_index",
              (function (param) {
                  var res = new RegExp("zbar").exec("foobarbazbar");
                  if (res !== null) {
                    return {
                            TAG: /* Eq */0,
                            _0: 8,
                            _1: res.index
                          };
                  } else {
                    return {
                            TAG: /* Fail */8,
                            _0: undefined
                          };
                  }
                })
            ],
            _1: /* :: */{
              _0: [
                "result_input",
                (function (param) {
                    var input = "foobar";
                    var res = /foo/g.exec(input);
                    if (res !== null) {
                      return {
                              TAG: /* Eq */0,
                              _0: input,
                              _1: res.input
                            };
                    } else {
                      return {
                              TAG: /* Fail */8,
                              _0: undefined
                            };
                    }
                  })
              ],
              _1: /* :: */{
                _0: [
                  "t_flags",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: "gi",
                              _1: /./ig.flags
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "t_global",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: true,
                                _1: /./ig.global
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "t_ignoreCase",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: true,
                                  _1: /./ig.ignoreCase
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: [
                        "t_lastIndex",
                        (function (param) {
                            var re = /na/g;
                            re.exec("banana");
                            return {
                                    TAG: /* Eq */0,
                                    _0: 4,
                                    _1: re.lastIndex
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: [
                          "t_setLastIndex",
                          (function (param) {
                              var re = /na/g;
                              var before = re.lastIndex;
                              re.lastIndex = 42;
                              var after = re.lastIndex;
                              return {
                                      TAG: /* Eq */0,
                                      _0: [
                                        0,
                                        42
                                      ],
                                      _1: [
                                        before,
                                        after
                                      ]
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: [
                            "t_multiline",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: false,
                                        _1: /./ig.multiline
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: [
                              "t_source",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: "f.+o",
                                          _1: /f.+o/ig.source
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: [
                                "t_sticky",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: true,
                                            _1: /./yg.sticky
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: [
                                  "t_unicode",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: false,
                                              _1: /./yg.unicode
                                            };
                                    })
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
        }
      }
    }
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_re_test", suites);

exports.suites = suites;
/*  Not a pure module */
