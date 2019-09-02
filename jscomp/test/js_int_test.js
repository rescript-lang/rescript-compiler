'use strict';

var Mt = require("./mt.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "toExponential",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "1.23456e+5",
                Arg1: (123456).toExponential()
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "toExponentialWithPrecision - digits:2",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: "1.23e+5",
                  Arg1: (123456).toExponential(2)
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "toExponentialWithPrecision - digits:4",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: "1.2346e+5",
                    Arg1: (123456).toExponential(4)
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "toExponentialWithPrecision - digits:20",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: "0.00000000000000000000e+0",
                      Arg1: (0).toExponential(20)
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "File \"js_int_test.ml\", line 12, characters 3-10",
            (function (param) {
                return /* constructor */{
                        tag: "ThrowAny",
                        Arg0: (function (param) {
                            (0).toExponential(101);
                            return /* () */0;
                          })
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "toExponentialWithPrecision - digits:-1",
              (function (param) {
                  return /* constructor */{
                          tag: "ThrowAny",
                          Arg0: (function (param) {
                              (0).toExponential(-1);
                              return /* () */0;
                            })
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "toPrecision",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: "123456",
                            Arg1: (123456).toPrecision()
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "toPrecisionWithPrecision - digits:2",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: "1.2e+5",
                              Arg1: (123456).toPrecision(2)
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "toPrecisionWithPrecision - digits:4",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: "1.235e+5",
                                Arg1: (123456).toPrecision(4)
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "toPrecisionWithPrecision - digits:20",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: "0.0000000000000000000",
                                  Arg1: (0).toPrecision(20)
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "File \"js_int_test.ml\", line 25, characters 3-10",
                        (function (param) {
                            return /* constructor */{
                                    tag: "ThrowAny",
                                    Arg0: (function (param) {
                                        (0).toPrecision(101);
                                        return /* () */0;
                                      })
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "toPrecisionWithPrecision - digits:-1",
                          (function (param) {
                              return /* constructor */{
                                      tag: "ThrowAny",
                                      Arg0: (function (param) {
                                          (0).toPrecision(-1);
                                          return /* () */0;
                                        })
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "toString",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: "123",
                                        Arg1: (123).toString()
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "toStringWithRadix - radix:2",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: "11110001001000000",
                                          Arg1: (123456).toString(2)
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "toStringWithRadix - radix:16",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: "1e240",
                                            Arg1: (123456).toString(16)
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "toStringWithRadix - radix:36",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: "2n9c",
                                              Arg1: (123456).toString(36)
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "toStringWithRadix - radix:37",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "ThrowAny",
                                                Arg0: (function (param) {
                                                    (0).toString(37);
                                                    return /* () */0;
                                                  })
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "toStringWithRadix - radix:1",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "ThrowAny",
                                                  Arg0: (function (param) {
                                                      (0).toString(1);
                                                      return /* () */0;
                                                    })
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "toStringWithRadix - radix:-1",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "ThrowAny",
                                                    Arg0: (function (param) {
                                                        (0).toString(-1);
                                                        return /* () */0;
                                                      })
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
    }
  }
};

Mt.from_pair_suites("Js_int_test", suites);

exports.suites = suites;
/*  Not a pure module */
