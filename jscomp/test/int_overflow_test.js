'use strict';

var Mt = require("./mt.js");
var Int32 = require("../../lib/js/int32.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_string = require("../../lib/js/caml_string.js");

function hash_variant(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
    accu = Caml_int32.imul(223, accu) + Caml_string.get(s, i) & 2147483647;
  }
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

function hash_variant2(s) {
  var accu = 0;
  for(var i = 0 ,i_finish = s.length - 1 | 0; i <= i_finish; ++i){
    accu = Caml_int32.imul(223, accu) + Caml_string.get(s, i) | 0;
  }
  accu = accu & 2147483647;
  if (accu > 1073741823) {
    return accu - -2147483648 | 0;
  } else {
    return accu;
  }
}

function fib(n) {
  if (n !== 0 && n !== 1) {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  } else {
    return 1;
  }
}

Mt.from_pair_suites("Int_overflow_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "plus_overflow",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: true,
                    Arg1: (Int32.max_int + 1 | 0) === Int32.min_int
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "minus_overflow",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: true,
                      Arg1: (Int32.min_int - Int32.one | 0) === Int32.max_int
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "flow_again",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: 2147483646,
                        Arg1: (Int32.max_int + Int32.max_int | 0) + Int32.min_int | 0
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "flow_again",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: -2,
                          Arg1: Int32.max_int + Int32.max_int | 0
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "hash_test",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: hash_variant("xxyyzzuuxxzzyy00112233"),
                            Arg1: 544087776
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "hash_test2",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: hash_variant("xxyyzxzzyy"),
                              Arg1: -449896130
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "File \"int_overflow_test.ml\", line 37, characters 2-9",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: hash_variant2("xxyyzzuuxxzzyy00112233"),
                                Arg1: 544087776
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "File \"int_overflow_test.ml\", line 38, characters 2-9",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: hash_variant2("xxyyzxzzyy"),
                                  Arg1: -449896130
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "int_literal_flow",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: -1,
                                    Arg1: -1
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "int_literal_flow2",
                          (function (param) {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: -1,
                                      Arg1: -1
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "int_literal_flow3",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: -1,
                                        Arg1: -1
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "int32_mul",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: -33554431,
                                          Arg1: -33554431
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "File \"int_overflow_test.ml\", line 44, characters 3-10",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: Number("3") | 0,
                                            Arg1: 3
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "File \"int_overflow_test.ml\", line 46, characters 3-10",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: Number("3.2") | 0,
                                              Arg1: 3
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
    });

exports.hash_variant = hash_variant;
exports.hash_variant2 = hash_variant2;
exports.fib = fib;
/*  Not a pure module */
