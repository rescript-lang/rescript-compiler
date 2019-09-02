'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "captures",
    (function (param) {
        var re = (/(\d+)-(?:(\d+))?/g);
        var match = re.exec("3-");
        if (match !== null) {
          var defined = Caml_array.caml_array_get(match, 1);
          var $$undefined = Caml_array.caml_array_get(match, 2);
          return /* constructor */{
                  tag: "Eq",
                  Arg0: /* tuple */[
                    "3",
                    null
                  ],
                  Arg1: /* tuple */[
                    defined,
                    $$undefined
                  ]
                };
        } else {
          return /* constructor */{
                  tag: "Fail",
                  Arg0: /* () */0
                };
        }
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "fromString",
      (function (param) {
          var contentOf = function (tag, xmlString) {
            var param = new RegExp("<" + (tag + (">(.*?)<\\/" + (tag + ">")))).exec(xmlString);
            if (param !== null) {
              return Caml_option.nullable_to_opt(Caml_array.caml_array_get(param, 1));
            }
            
          };
          return /* constructor */{
                  tag: "Eq",
                  Arg0: contentOf("div", "<div>Hi</div>"),
                  Arg1: "Hi"
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "exec_literal",
        (function (param) {
            var match = (/[^.]+/).exec("http://xxx.domain.com");
            if (match !== null) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: "http://xxx",
                      Arg1: Caml_array.caml_array_get(match, 0)
                    };
            } else {
              return /* constructor */{
                      tag: "FailWith",
                      Arg0: "regex should match"
                    };
            }
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "exec_no_match",
          (function (param) {
              var match = (/https:\/\/(.*)/).exec("http://xxx.domain.com");
              if (match !== null) {
                return /* constructor */{
                        tag: "FailWith",
                        Arg0: "regex should not match"
                      };
              } else {
                return /* constructor */{
                        tag: "Ok",
                        Arg0: true
                      };
              }
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "test_str",
            (function (param) {
                var res = new RegExp("foo").test("#foo#");
                return /* constructor */{
                        tag: "Eq",
                        Arg0: true,
                        Arg1: res
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "fromStringWithFlags",
              (function (param) {
                  var res = new RegExp("foo", "g");
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: true,
                          Arg1: res.global
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "result_index",
                (function (param) {
                    var match = new RegExp("zbar").exec("foobarbazbar");
                    if (match !== null) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: 8,
                              Arg1: match.index
                            };
                    } else {
                      return /* constructor */{
                              tag: "Fail",
                              Arg0: /* () */0
                            };
                    }
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "result_input",
                  (function (param) {
                      var input = "foobar";
                      var match = (/foo/g).exec(input);
                      if (match !== null) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: input,
                                Arg1: match.input
                              };
                      } else {
                        return /* constructor */{
                                tag: "Fail",
                                Arg0: /* () */0
                              };
                      }
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "t_flags",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: "gi",
                                Arg1: (/./ig).flags
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "t_global",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: true,
                                  Arg1: (/./ig).global
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "t_ignoreCase",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: true,
                                    Arg1: (/./ig).ignoreCase
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "t_lastIndex",
                          (function (param) {
                              var re = (/na/g);
                              re.exec("banana");
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: 4,
                                      Arg1: re.lastIndex
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "t_setLastIndex",
                            (function (param) {
                                var re = (/na/g);
                                var before = re.lastIndex;
                                re.lastIndex = 42;
                                var after = re.lastIndex;
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: /* tuple */[
                                          0,
                                          42
                                        ],
                                        Arg1: /* tuple */[
                                          before,
                                          after
                                        ]
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "t_multiline",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: false,
                                          Arg1: (/./ig).multiline
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "t_source",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: "f.+o",
                                            Arg1: (/f.+o/ig).source
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "t_sticky",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: true,
                                              Arg1: (/./yg).sticky
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "t_unicode",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: false,
                                                Arg1: (/./yg).unicode
                                              };
                                      })
                                  ],
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
};

Mt.from_pair_suites("Js_re_test", suites);

exports.suites = suites;
/*  Not a pure module */
