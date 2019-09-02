'use strict';

var Mt = require("./mt.js");
var Js_math = require("../../lib/js/js_math.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "_E",
    (function (param) {
        return /* constructor */{
                tag: "ApproxThreshold",
                Arg0: 0.001,
                Arg1: 2.718,
                Arg2: Math.E
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "_LN2",
      (function (param) {
          return /* constructor */{
                  tag: "ApproxThreshold",
                  Arg0: 0.001,
                  Arg1: 0.693,
                  Arg2: Math.LN2
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "_LN10",
        (function (param) {
            return /* constructor */{
                    tag: "ApproxThreshold",
                    Arg0: 0.001,
                    Arg1: 2.303,
                    Arg2: Math.LN10
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "_LOG2E",
          (function (param) {
              return /* constructor */{
                      tag: "ApproxThreshold",
                      Arg0: 0.001,
                      Arg1: 1.443,
                      Arg2: Math.LOG2E
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "_LOG10E",
            (function (param) {
                return /* constructor */{
                        tag: "ApproxThreshold",
                        Arg0: 0.001,
                        Arg1: 0.434,
                        Arg2: Math.LOG10E
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "_PI",
              (function (param) {
                  return /* constructor */{
                          tag: "ApproxThreshold",
                          Arg0: 0.00001,
                          Arg1: 3.14159,
                          Arg2: Math.PI
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "_SQRT1_2",
                (function (param) {
                    return /* constructor */{
                            tag: "ApproxThreshold",
                            Arg0: 0.001,
                            Arg1: 0.707,
                            Arg2: Math.SQRT1_2
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "_SQRT2",
                  (function (param) {
                      return /* constructor */{
                              tag: "ApproxThreshold",
                              Arg0: 0.001,
                              Arg1: 1.414,
                              Arg2: Math.SQRT2
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "abs_int",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: 4,
                                Arg1: Math.abs(-4)
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "abs_float",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: 1.2,
                                  Arg1: Math.abs(-1.2)
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "acos",
                        (function (param) {
                            return /* constructor */{
                                    tag: "ApproxThreshold",
                                    Arg0: 0.001,
                                    Arg1: 1.159,
                                    Arg2: Math.acos(0.4)
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "acosh",
                          (function (param) {
                              return /* constructor */{
                                      tag: "ApproxThreshold",
                                      Arg0: 0.001,
                                      Arg1: 0.622,
                                      Arg2: Math.acosh(1.2)
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "asin",
                            (function (param) {
                                return /* constructor */{
                                        tag: "ApproxThreshold",
                                        Arg0: 0.001,
                                        Arg1: 0.411,
                                        Arg2: Math.asin(0.4)
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "asinh",
                              (function (param) {
                                  return /* constructor */{
                                          tag: "ApproxThreshold",
                                          Arg0: 0.001,
                                          Arg1: 0.390,
                                          Arg2: Math.asinh(0.4)
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "atan",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "ApproxThreshold",
                                            Arg0: 0.001,
                                            Arg1: 0.380,
                                            Arg2: Math.atan(0.4)
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "atanh",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "ApproxThreshold",
                                              Arg0: 0.001,
                                              Arg1: 0.423,
                                              Arg2: Math.atanh(0.4)
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "atan2",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "ApproxThreshold",
                                                Arg0: 0.001,
                                                Arg1: 0.588,
                                                Arg2: Math.atan2(0.4, 0.6)
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "cbrt",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: 2,
                                                  Arg1: Math.cbrt(8)
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "unsafe_ceil_int",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: 4,
                                                    Arg1: Math.ceil(3.2)
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "ceil_int",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "Eq",
                                                      Arg0: 4,
                                                      Arg1: Js_math.ceil_int(3.2)
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "ceil_float",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "Eq",
                                                        Arg0: 4,
                                                        Arg1: Math.ceil(3.2)
                                                      };
                                              })
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "cos",
                                              (function (param) {
                                                  return /* constructor */{
                                                          tag: "ApproxThreshold",
                                                          Arg0: 0.001,
                                                          Arg1: 0.921,
                                                          Arg2: Math.cos(0.4)
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "cosh",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "ApproxThreshold",
                                                            Arg0: 0.001,
                                                            Arg1: 1.081,
                                                            Arg2: Math.cosh(0.4)
                                                          };
                                                  })
                                              ],
                                              Arg1: /* constructor */{
                                                tag: "::",
                                                Arg0: /* tuple */[
                                                  "exp",
                                                  (function (param) {
                                                      return /* constructor */{
                                                              tag: "ApproxThreshold",
                                                              Arg0: 0.001,
                                                              Arg1: 1.491,
                                                              Arg2: Math.exp(0.4)
                                                            };
                                                    })
                                                ],
                                                Arg1: /* constructor */{
                                                  tag: "::",
                                                  Arg0: /* tuple */[
                                                    "expm1",
                                                    (function (param) {
                                                        return /* constructor */{
                                                                tag: "ApproxThreshold",
                                                                Arg0: 0.001,
                                                                Arg1: 0.491,
                                                                Arg2: Math.expm1(0.4)
                                                              };
                                                      })
                                                  ],
                                                  Arg1: /* constructor */{
                                                    tag: "::",
                                                    Arg0: /* tuple */[
                                                      "unsafe_floor_int",
                                                      (function (param) {
                                                          return /* constructor */{
                                                                  tag: "Eq",
                                                                  Arg0: 3,
                                                                  Arg1: Math.floor(3.2)
                                                                };
                                                        })
                                                    ],
                                                    Arg1: /* constructor */{
                                                      tag: "::",
                                                      Arg0: /* tuple */[
                                                        "floor_int",
                                                        (function (param) {
                                                            return /* constructor */{
                                                                    tag: "Eq",
                                                                    Arg0: 3,
                                                                    Arg1: Js_math.floor_int(3.2)
                                                                  };
                                                          })
                                                      ],
                                                      Arg1: /* constructor */{
                                                        tag: "::",
                                                        Arg0: /* tuple */[
                                                          "floor_float",
                                                          (function (param) {
                                                              return /* constructor */{
                                                                      tag: "Eq",
                                                                      Arg0: 3,
                                                                      Arg1: Math.floor(3.2)
                                                                    };
                                                            })
                                                        ],
                                                        Arg1: /* constructor */{
                                                          tag: "::",
                                                          Arg0: /* tuple */[
                                                            "fround",
                                                            (function (param) {
                                                                return /* constructor */{
                                                                        tag: "Approx",
                                                                        Arg0: 3.2,
                                                                        Arg1: Math.fround(3.2)
                                                                      };
                                                              })
                                                          ],
                                                          Arg1: /* constructor */{
                                                            tag: "::",
                                                            Arg0: /* tuple */[
                                                              "hypot",
                                                              (function (param) {
                                                                  return /* constructor */{
                                                                          tag: "ApproxThreshold",
                                                                          Arg0: 0.001,
                                                                          Arg1: 0.721,
                                                                          Arg2: Math.hypot(0.4, 0.6)
                                                                        };
                                                                })
                                                            ],
                                                            Arg1: /* constructor */{
                                                              tag: "::",
                                                              Arg0: /* tuple */[
                                                                "hypotMany",
                                                                (function (param) {
                                                                    return /* constructor */{
                                                                            tag: "ApproxThreshold",
                                                                            Arg0: 0.001,
                                                                            Arg1: 1.077,
                                                                            Arg2: Math.hypot(0.4, 0.6, 0.8)
                                                                          };
                                                                  })
                                                              ],
                                                              Arg1: /* constructor */{
                                                                tag: "::",
                                                                Arg0: /* tuple */[
                                                                  "imul",
                                                                  (function (param) {
                                                                      return /* constructor */{
                                                                              tag: "Eq",
                                                                              Arg0: 8,
                                                                              Arg1: Math.imul(4, 2)
                                                                            };
                                                                    })
                                                                ],
                                                                Arg1: /* constructor */{
                                                                  tag: "::",
                                                                  Arg0: /* tuple */[
                                                                    "log",
                                                                    (function (param) {
                                                                        return /* constructor */{
                                                                                tag: "ApproxThreshold",
                                                                                Arg0: 0.001,
                                                                                Arg1: -0.916,
                                                                                Arg2: Math.log(0.4)
                                                                              };
                                                                      })
                                                                  ],
                                                                  Arg1: /* constructor */{
                                                                    tag: "::",
                                                                    Arg0: /* tuple */[
                                                                      "log1p",
                                                                      (function (param) {
                                                                          return /* constructor */{
                                                                                  tag: "ApproxThreshold",
                                                                                  Arg0: 0.001,
                                                                                  Arg1: 0.336,
                                                                                  Arg2: Math.log1p(0.4)
                                                                                };
                                                                        })
                                                                    ],
                                                                    Arg1: /* constructor */{
                                                                      tag: "::",
                                                                      Arg0: /* tuple */[
                                                                        "log10",
                                                                        (function (param) {
                                                                            return /* constructor */{
                                                                                    tag: "ApproxThreshold",
                                                                                    Arg0: 0.001,
                                                                                    Arg1: -0.397,
                                                                                    Arg2: Math.log10(0.4)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      Arg1: /* constructor */{
                                                                        tag: "::",
                                                                        Arg0: /* tuple */[
                                                                          "log2",
                                                                          (function (param) {
                                                                              return /* constructor */{
                                                                                      tag: "ApproxThreshold",
                                                                                      Arg0: 0.001,
                                                                                      Arg1: -1.321,
                                                                                      Arg2: Math.log2(0.4)
                                                                                    };
                                                                            })
                                                                        ],
                                                                        Arg1: /* constructor */{
                                                                          tag: "::",
                                                                          Arg0: /* tuple */[
                                                                            "max_int",
                                                                            (function (param) {
                                                                                return /* constructor */{
                                                                                        tag: "Eq",
                                                                                        Arg0: 4,
                                                                                        Arg1: Math.max(2, 4)
                                                                                      };
                                                                              })
                                                                          ],
                                                                          Arg1: /* constructor */{
                                                                            tag: "::",
                                                                            Arg0: /* tuple */[
                                                                              "maxMany_int",
                                                                              (function (param) {
                                                                                  return /* constructor */{
                                                                                          tag: "Eq",
                                                                                          Arg0: 4,
                                                                                          Arg1: Math.max(2, 4, 3)
                                                                                        };
                                                                                })
                                                                            ],
                                                                            Arg1: /* constructor */{
                                                                              tag: "::",
                                                                              Arg0: /* tuple */[
                                                                                "max_float",
                                                                                (function (param) {
                                                                                    return /* constructor */{
                                                                                            tag: "Eq",
                                                                                            Arg0: 4.2,
                                                                                            Arg1: Math.max(2.7, 4.2)
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              Arg1: /* constructor */{
                                                                                tag: "::",
                                                                                Arg0: /* tuple */[
                                                                                  "maxMany_float",
                                                                                  (function (param) {
                                                                                      return /* constructor */{
                                                                                              tag: "Eq",
                                                                                              Arg0: 4.2,
                                                                                              Arg1: Math.max(2.7, 4.2, 3.9)
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                Arg1: /* constructor */{
                                                                                  tag: "::",
                                                                                  Arg0: /* tuple */[
                                                                                    "min_int",
                                                                                    (function (param) {
                                                                                        return /* constructor */{
                                                                                                tag: "Eq",
                                                                                                Arg0: 2,
                                                                                                Arg1: Math.min(2, 4)
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  Arg1: /* constructor */{
                                                                                    tag: "::",
                                                                                    Arg0: /* tuple */[
                                                                                      "minMany_int",
                                                                                      (function (param) {
                                                                                          return /* constructor */{
                                                                                                  tag: "Eq",
                                                                                                  Arg0: 2,
                                                                                                  Arg1: Math.min(2, 4, 3)
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    Arg1: /* constructor */{
                                                                                      tag: "::",
                                                                                      Arg0: /* tuple */[
                                                                                        "min_float",
                                                                                        (function (param) {
                                                                                            return /* constructor */{
                                                                                                    tag: "Eq",
                                                                                                    Arg0: 2.7,
                                                                                                    Arg1: Math.min(2.7, 4.2)
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      Arg1: /* constructor */{
                                                                                        tag: "::",
                                                                                        Arg0: /* tuple */[
                                                                                          "minMany_float",
                                                                                          (function (param) {
                                                                                              return /* constructor */{
                                                                                                      tag: "Eq",
                                                                                                      Arg0: 2.7,
                                                                                                      Arg1: Math.min(2.7, 4.2, 3.9)
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        Arg1: /* constructor */{
                                                                                          tag: "::",
                                                                                          Arg0: /* tuple */[
                                                                                            "random",
                                                                                            (function (param) {
                                                                                                var a = Math.random();
                                                                                                return /* constructor */{
                                                                                                        tag: "Ok",
                                                                                                        Arg0: a >= 0 && a < 1
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          Arg1: /* constructor */{
                                                                                            tag: "::",
                                                                                            Arg0: /* tuple */[
                                                                                              "random_int",
                                                                                              (function (param) {
                                                                                                  var a = Js_math.random_int(1, 3);
                                                                                                  return /* constructor */{
                                                                                                          tag: "Ok",
                                                                                                          Arg0: a >= 1 && a < 3
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            Arg1: /* constructor */{
                                                                                              tag: "::",
                                                                                              Arg0: /* tuple */[
                                                                                                "unsafe_round",
                                                                                                (function (param) {
                                                                                                    return /* constructor */{
                                                                                                            tag: "Eq",
                                                                                                            Arg0: 3,
                                                                                                            Arg1: Math.round(3.2)
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              Arg1: /* constructor */{
                                                                                                tag: "::",
                                                                                                Arg0: /* tuple */[
                                                                                                  "round",
                                                                                                  (function (param) {
                                                                                                      return /* constructor */{
                                                                                                              tag: "Eq",
                                                                                                              Arg0: 3,
                                                                                                              Arg1: Math.round(3.2)
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                Arg1: /* constructor */{
                                                                                                  tag: "::",
                                                                                                  Arg0: /* tuple */[
                                                                                                    "sign_int",
                                                                                                    (function (param) {
                                                                                                        return /* constructor */{
                                                                                                                tag: "Eq",
                                                                                                                Arg0: -1,
                                                                                                                Arg1: Math.sign(-4)
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  Arg1: /* constructor */{
                                                                                                    tag: "::",
                                                                                                    Arg0: /* tuple */[
                                                                                                      "sign_float",
                                                                                                      (function (param) {
                                                                                                          return /* constructor */{
                                                                                                                  tag: "Eq",
                                                                                                                  Arg0: -1,
                                                                                                                  Arg1: Math.sign(-4.2)
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    Arg1: /* constructor */{
                                                                                                      tag: "::",
                                                                                                      Arg0: /* tuple */[
                                                                                                        "sign_float -0",
                                                                                                        (function (param) {
                                                                                                            return /* constructor */{
                                                                                                                    tag: "Eq",
                                                                                                                    Arg0: -0,
                                                                                                                    Arg1: Math.sign(-0)
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      Arg1: /* constructor */{
                                                                                                        tag: "::",
                                                                                                        Arg0: /* tuple */[
                                                                                                          "sin",
                                                                                                          (function (param) {
                                                                                                              return /* constructor */{
                                                                                                                      tag: "ApproxThreshold",
                                                                                                                      Arg0: 0.001,
                                                                                                                      Arg1: 0.389,
                                                                                                                      Arg2: Math.sin(0.4)
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        Arg1: /* constructor */{
                                                                                                          tag: "::",
                                                                                                          Arg0: /* tuple */[
                                                                                                            "sinh",
                                                                                                            (function (param) {
                                                                                                                return /* constructor */{
                                                                                                                        tag: "ApproxThreshold",
                                                                                                                        Arg0: 0.001,
                                                                                                                        Arg1: 0.410,
                                                                                                                        Arg2: Math.sinh(0.4)
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          Arg1: /* constructor */{
                                                                                                            tag: "::",
                                                                                                            Arg0: /* tuple */[
                                                                                                              "sqrt",
                                                                                                              (function (param) {
                                                                                                                  return /* constructor */{
                                                                                                                          tag: "ApproxThreshold",
                                                                                                                          Arg0: 0.001,
                                                                                                                          Arg1: 0.632,
                                                                                                                          Arg2: Math.sqrt(0.4)
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            Arg1: /* constructor */{
                                                                                                              tag: "::",
                                                                                                              Arg0: /* tuple */[
                                                                                                                "tan",
                                                                                                                (function (param) {
                                                                                                                    return /* constructor */{
                                                                                                                            tag: "ApproxThreshold",
                                                                                                                            Arg0: 0.001,
                                                                                                                            Arg1: 0.422,
                                                                                                                            Arg2: Math.tan(0.4)
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              Arg1: /* constructor */{
                                                                                                                tag: "::",
                                                                                                                Arg0: /* tuple */[
                                                                                                                  "tanh",
                                                                                                                  (function (param) {
                                                                                                                      return /* constructor */{
                                                                                                                              tag: "ApproxThreshold",
                                                                                                                              Arg0: 0.001,
                                                                                                                              Arg1: 0.379,
                                                                                                                              Arg2: Math.tanh(0.4)
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                Arg1: /* constructor */{
                                                                                                                  tag: "::",
                                                                                                                  Arg0: /* tuple */[
                                                                                                                    "unsafe_trunc",
                                                                                                                    (function (param) {
                                                                                                                        return /* constructor */{
                                                                                                                                tag: "Eq",
                                                                                                                                Arg0: 4,
                                                                                                                                Arg1: Math.trunc(4.2156)
                                                                                                                              };
                                                                                                                      })
                                                                                                                  ],
                                                                                                                  Arg1: /* constructor */{
                                                                                                                    tag: "::",
                                                                                                                    Arg0: /* tuple */[
                                                                                                                      "trunc",
                                                                                                                      (function (param) {
                                                                                                                          return /* constructor */{
                                                                                                                                  tag: "Eq",
                                                                                                                                  Arg0: 4,
                                                                                                                                  Arg1: Math.trunc(4.2156)
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

Mt.from_pair_suites("Js_math_test", suites);

exports.suites = suites;
/*  Not a pure module */
