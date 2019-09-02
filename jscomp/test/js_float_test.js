'use strict';

var Mt = require("./mt.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "_NaN <> _NaN",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: false,
                Arg1: NaN === NaN
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "isNaN - _NaN",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: true,
                  Arg1: isNaN(NaN)
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "isNaN - 0.",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: false,
                    Arg1: isNaN(0)
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "isFinite - infinity",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: false,
                      Arg1: isFinite(Number.POSITIVE_INFINITY)
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "isFinite - neg_infinity",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: false,
                        Arg1: isFinite(Number.NEGATIVE_INFINITY)
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "isFinite - _NaN",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: false,
                          Arg1: isFinite(NaN)
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "isFinite - 0.",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: true,
                            Arg1: isFinite(0)
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "toExponential",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: "1.23456e+2",
                              Arg1: (123.456).toExponential()
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "toExponential - large number",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: "1.2e+21",
                                Arg1: (1.2e21).toExponential()
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
                                  Arg0: "1.23e+2",
                                  Arg1: (123.456).toExponential(2)
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
                                    Arg0: "1.2346e+2",
                                    Arg1: (123.456).toExponential(4)
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
                            "File \"js_float_test.ml\", line 31, characters 3-10",
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
                                "toFixed",
                                (function (param) {
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: "123",
                                            Arg1: (123.456).toFixed()
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "toFixed - large number",
                                  (function (param) {
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: "1.2e+21",
                                              Arg1: (1.2e21).toFixed()
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "toFixedWithPrecision - digits:2",
                                    (function (param) {
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: "123.46",
                                                Arg1: (123.456).toFixed(2)
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "toFixedWithPrecision - digits:4",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: "123.4560",
                                                  Arg1: (123.456).toFixed(4)
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "toFixedWithPrecision - digits:20",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: "0.00000000000000000000",
                                                    Arg1: (0).toFixed(20)
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "toFixedWithPrecision - digits:101",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "ThrowAny",
                                                      Arg0: (function (param) {
                                                          (0).toFixed(101);
                                                          return /* () */0;
                                                        })
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "toFixedWithPrecision - digits:-1",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "ThrowAny",
                                                        Arg0: (function (param) {
                                                            (0).toFixed(-1);
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
                                                          Arg0: "123.456",
                                                          Arg1: (123.456).toPrecision()
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "toPrecision - large number",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "Eq",
                                                            Arg0: "1.2e+21",
                                                            Arg1: (1.2e21).toPrecision()
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
                                                              Arg0: "1.2e+2",
                                                              Arg1: (123.456).toPrecision(2)
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
                                                                Arg0: "123.5",
                                                                Arg1: (123.456).toPrecision(4)
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
                                                        "File \"js_float_test.ml\", line 61, characters 3-10",
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
                                                                        Arg0: "1.23",
                                                                        Arg1: (1.23).toString()
                                                                      };
                                                              })
                                                          ],
                                                          Arg1: /* constructor */{
                                                            tag: "::",
                                                            Arg0: /* tuple */[
                                                              "toString - large number",
                                                              (function (param) {
                                                                  return /* constructor */{
                                                                          tag: "Eq",
                                                                          Arg0: "1.2e+21",
                                                                          Arg1: (1.2e21).toString()
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
                                                                            Arg0: "1111011.0111010010111100011010100111111011111001110111",
                                                                            Arg1: (123.456).toString(2)
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
                                                                              Arg0: "7b.74bc6a7ef9dc",
                                                                              Arg1: (123.456).toString(16)
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
                                                                                Arg0: "3f",
                                                                                Arg1: (123).toString(36)
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
                                                                        Arg1: /* constructor */{
                                                                          tag: "::",
                                                                          Arg0: /* tuple */[
                                                                            "fromString - 123",
                                                                            (function (param) {
                                                                                return /* constructor */{
                                                                                        tag: "Eq",
                                                                                        Arg0: 123,
                                                                                        Arg1: Number("123")
                                                                                      };
                                                                              })
                                                                          ],
                                                                          Arg1: /* constructor */{
                                                                            tag: "::",
                                                                            Arg0: /* tuple */[
                                                                              "fromString - 12.3",
                                                                              (function (param) {
                                                                                  return /* constructor */{
                                                                                          tag: "Eq",
                                                                                          Arg0: 12.3,
                                                                                          Arg1: Number("12.3")
                                                                                        };
                                                                                })
                                                                            ],
                                                                            Arg1: /* constructor */{
                                                                              tag: "::",
                                                                              Arg0: /* tuple */[
                                                                                "fromString - empty string",
                                                                                (function (param) {
                                                                                    return /* constructor */{
                                                                                            tag: "Eq",
                                                                                            Arg0: 0,
                                                                                            Arg1: Number("")
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              Arg1: /* constructor */{
                                                                                tag: "::",
                                                                                Arg0: /* tuple */[
                                                                                  "fromString - 0x11",
                                                                                  (function (param) {
                                                                                      return /* constructor */{
                                                                                              tag: "Eq",
                                                                                              Arg0: 17,
                                                                                              Arg1: Number("0x11")
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                Arg1: /* constructor */{
                                                                                  tag: "::",
                                                                                  Arg0: /* tuple */[
                                                                                    "fromString - 0b11",
                                                                                    (function (param) {
                                                                                        return /* constructor */{
                                                                                                tag: "Eq",
                                                                                                Arg0: 3,
                                                                                                Arg1: Number("0b11")
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  Arg1: /* constructor */{
                                                                                    tag: "::",
                                                                                    Arg0: /* tuple */[
                                                                                      "fromString - 0o11",
                                                                                      (function (param) {
                                                                                          return /* constructor */{
                                                                                                  tag: "Eq",
                                                                                                  Arg0: 9,
                                                                                                  Arg1: Number("0o11")
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    Arg1: /* constructor */{
                                                                                      tag: "::",
                                                                                      Arg0: /* tuple */[
                                                                                        "fromString - invalid string",
                                                                                        (function (param) {
                                                                                            return /* constructor */{
                                                                                                    tag: "Eq",
                                                                                                    Arg0: true,
                                                                                                    Arg1: isNaN(Number("foo"))
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
};

Mt.from_pair_suites("Js_float_test", suites);

exports.suites = suites;
/*  Not a pure module */
