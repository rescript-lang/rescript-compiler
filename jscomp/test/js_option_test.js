'use strict';

var Mt = require("./mt.js");
var Js_option = require("../../lib/js/js_option.js");

function simpleEq(a, b) {
  return a === b;
}

var option_suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "option_isSome_Some",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: true,
                Arg1: true
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "option_isSome_None",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: false,
                  Arg1: false
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "option_isNone_Some",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: false,
                    Arg1: false
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "option_isNone_None",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: true,
                      Arg1: true
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "option_isSomeValue_Eq",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: true,
                        Arg1: Js_option.isSomeValue(simpleEq, 2, 2)
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "option_isSomeValue_Diff",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: false,
                          Arg1: Js_option.isSomeValue(simpleEq, 1, 2)
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "option_isSomeValue_DiffNone",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: false,
                            Arg1: Js_option.isSomeValue(simpleEq, 1, undefined)
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "option_getExn_Some",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: 2,
                              Arg1: Js_option.getExn(2)
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "option_equal_Eq",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: true,
                                Arg1: Js_option.equal(simpleEq, 2, 2)
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "option_equal_Diff",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: false,
                                  Arg1: Js_option.equal(simpleEq, 1, 2)
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "option_equal_DiffNone",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: false,
                                    Arg1: Js_option.equal(simpleEq, 1, undefined)
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "option_andThen_SomeSome",
                          (function (param) {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: true,
                                      Arg1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (a) {
                                                  return a + 1 | 0;
                                                }), 2))
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "option_andThen_SomeNone",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: false,
                                        Arg1: Js_option.isSomeValue(simpleEq, 3, Js_option.andThen((function (param) {
                                                    return ;
                                                  }), 2))
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "option_map_Some",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: true,
                                          Arg1: Js_option.isSomeValue(simpleEq, 3, Js_option.map((function (a) {
                                                      return a + 1 | 0;
                                                    }), 2))
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "option_map_None",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: undefined,
                                            Arg1: Js_option.map((function (a) {
                                                    return a + 1 | 0;
                                                  }), undefined)
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "option_default_Some",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: 2,
                                              Arg1: Js_option.getWithDefault(3, 2)
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "option_default_None",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: 3,
                                                Arg1: Js_option.getWithDefault(3, undefined)
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "option_filter_Pass",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: true,
                                                  Arg1: Js_option.isSomeValue(simpleEq, 2, Js_option.filter((function (a) {
                                                              return a % 2 === 0;
                                                            }), 2))
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "option_filter_Reject",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: undefined,
                                                    Arg1: Js_option.filter((function (a) {
                                                            return a % 3 === 0;
                                                          }), 2)
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "option_filter_None",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "Eq",
                                                      Arg0: undefined,
                                                      Arg1: Js_option.filter((function (a) {
                                                              return a % 3 === 0;
                                                            }), undefined)
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "option_firstSome_First",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "Eq",
                                                        Arg0: true,
                                                        Arg1: Js_option.isSomeValue(simpleEq, 3, Js_option.firstSome(3, 2))
                                                      };
                                              })
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "option_firstSome_First",
                                              (function (param) {
                                                  return /* constructor */{
                                                          tag: "Eq",
                                                          Arg0: true,
                                                          Arg1: Js_option.isSomeValue(simpleEq, 2, Js_option.firstSome(undefined, 2))
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "option_firstSome_None",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "Eq",
                                                            Arg0: undefined,
                                                            Arg1: Js_option.firstSome(undefined, undefined)
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
        }
      }
    }
  }
};

Mt.from_pair_suites("Js_option_test", option_suites);

exports.simpleEq = simpleEq;
exports.option_suites = option_suites;
/*  Not a pure module */
