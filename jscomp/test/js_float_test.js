'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites_0 = [
  "_NaN <> _NaN",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: false,
              _1: NaN === NaN
            };
    })
];

var suites_1 = {
  hd: [
    "isNaN - _NaN",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: true,
                _1: isNaN(NaN)
              };
      })
  ],
  tl: {
    hd: [
      "isNaN - 0.",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: false,
                  _1: isNaN(0)
                };
        })
    ],
    tl: {
      hd: [
        "isFinite - infinity",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: false,
                    _1: isFinite(Pervasives.infinity)
                  };
          })
      ],
      tl: {
        hd: [
          "isFinite - neg_infinity",
          (function (param) {
              return {
                      TAG: /* Eq */0,
                      _0: false,
                      _1: isFinite(Pervasives.neg_infinity)
                    };
            })
        ],
        tl: {
          hd: [
            "isFinite - _NaN",
            (function (param) {
                return {
                        TAG: /* Eq */0,
                        _0: false,
                        _1: isFinite(NaN)
                      };
              })
          ],
          tl: {
            hd: [
              "isFinite - 0.",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: true,
                          _1: isFinite(0)
                        };
                })
            ],
            tl: {
              hd: [
                "toExponential",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: "1.23456e+2",
                            _1: (123.456).toExponential()
                          };
                  })
              ],
              tl: {
                hd: [
                  "toExponential - large number",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: "1.2e+21",
                              _1: (1.2e21).toExponential()
                            };
                    })
                ],
                tl: {
                  hd: [
                    "toExponentialWithPrecision - digits:2",
                    (function (param) {
                        return {
                                TAG: /* Eq */0,
                                _0: "1.23e+2",
                                _1: (123.456).toExponential(2)
                              };
                      })
                  ],
                  tl: {
                    hd: [
                      "toExponentialWithPrecision - digits:4",
                      (function (param) {
                          return {
                                  TAG: /* Eq */0,
                                  _0: "1.2346e+2",
                                  _1: (123.456).toExponential(4)
                                };
                        })
                    ],
                    tl: {
                      hd: [
                        "toExponentialWithPrecision - digits:20",
                        (function (param) {
                            return {
                                    TAG: /* Eq */0,
                                    _0: "0.00000000000000000000e+0",
                                    _1: (0).toExponential(20)
                                  };
                          })
                      ],
                      tl: {
                        hd: [
                          "File \"js_float_test.ml\", line 31, characters 3-10",
                          (function (param) {
                              return {
                                      TAG: /* ThrowAny */7,
                                      _0: (function (param) {
                                          (0).toExponential(101);
                                          
                                        })
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "toExponentialWithPrecision - digits:-1",
                            (function (param) {
                                return {
                                        TAG: /* ThrowAny */7,
                                        _0: (function (param) {
                                            (0).toExponential(-1);
                                            
                                          })
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "toFixed",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: "123",
                                          _1: (123.456).toFixed()
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "toFixed - large number",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: "1.2e+21",
                                            _1: (1.2e21).toFixed()
                                          };
                                  })
                              ],
                              tl: {
                                hd: [
                                  "toFixedWithPrecision - digits:2",
                                  (function (param) {
                                      return {
                                              TAG: /* Eq */0,
                                              _0: "123.46",
                                              _1: (123.456).toFixed(2)
                                            };
                                    })
                                ],
                                tl: {
                                  hd: [
                                    "toFixedWithPrecision - digits:4",
                                    (function (param) {
                                        return {
                                                TAG: /* Eq */0,
                                                _0: "123.4560",
                                                _1: (123.456).toFixed(4)
                                              };
                                      })
                                  ],
                                  tl: {
                                    hd: [
                                      "toFixedWithPrecision - digits:20",
                                      (function (param) {
                                          return {
                                                  TAG: /* Eq */0,
                                                  _0: "0.00000000000000000000",
                                                  _1: (0).toFixed(20)
                                                };
                                        })
                                    ],
                                    tl: {
                                      hd: [
                                        "toFixedWithPrecision - digits:101",
                                        (function (param) {
                                            return {
                                                    TAG: /* ThrowAny */7,
                                                    _0: (function (param) {
                                                        (0).toFixed(101);
                                                        
                                                      })
                                                  };
                                          })
                                      ],
                                      tl: {
                                        hd: [
                                          "toFixedWithPrecision - digits:-1",
                                          (function (param) {
                                              return {
                                                      TAG: /* ThrowAny */7,
                                                      _0: (function (param) {
                                                          (0).toFixed(-1);
                                                          
                                                        })
                                                    };
                                            })
                                        ],
                                        tl: {
                                          hd: [
                                            "toPrecision",
                                            (function (param) {
                                                return {
                                                        TAG: /* Eq */0,
                                                        _0: "123.456",
                                                        _1: (123.456).toPrecision()
                                                      };
                                              })
                                          ],
                                          tl: {
                                            hd: [
                                              "toPrecision - large number",
                                              (function (param) {
                                                  return {
                                                          TAG: /* Eq */0,
                                                          _0: "1.2e+21",
                                                          _1: (1.2e21).toPrecision()
                                                        };
                                                })
                                            ],
                                            tl: {
                                              hd: [
                                                "toPrecisionWithPrecision - digits:2",
                                                (function (param) {
                                                    return {
                                                            TAG: /* Eq */0,
                                                            _0: "1.2e+2",
                                                            _1: (123.456).toPrecision(2)
                                                          };
                                                  })
                                              ],
                                              tl: {
                                                hd: [
                                                  "toPrecisionWithPrecision - digits:4",
                                                  (function (param) {
                                                      return {
                                                              TAG: /* Eq */0,
                                                              _0: "123.5",
                                                              _1: (123.456).toPrecision(4)
                                                            };
                                                    })
                                                ],
                                                tl: {
                                                  hd: [
                                                    "toPrecisionWithPrecision - digits:20",
                                                    (function (param) {
                                                        return {
                                                                TAG: /* Eq */0,
                                                                _0: "0.0000000000000000000",
                                                                _1: (0).toPrecision(20)
                                                              };
                                                      })
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "File \"js_float_test.ml\", line 61, characters 3-10",
                                                      (function (param) {
                                                          return {
                                                                  TAG: /* ThrowAny */7,
                                                                  _0: (function (param) {
                                                                      (0).toPrecision(101);
                                                                      
                                                                    })
                                                                };
                                                        })
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "toPrecisionWithPrecision - digits:-1",
                                                        (function (param) {
                                                            return {
                                                                    TAG: /* ThrowAny */7,
                                                                    _0: (function (param) {
                                                                        (0).toPrecision(-1);
                                                                        
                                                                      })
                                                                  };
                                                          })
                                                      ],
                                                      tl: {
                                                        hd: [
                                                          "toString",
                                                          (function (param) {
                                                              return {
                                                                      TAG: /* Eq */0,
                                                                      _0: "1.23",
                                                                      _1: (1.23).toString()
                                                                    };
                                                            })
                                                        ],
                                                        tl: {
                                                          hd: [
                                                            "toString - large number",
                                                            (function (param) {
                                                                return {
                                                                        TAG: /* Eq */0,
                                                                        _0: "1.2e+21",
                                                                        _1: (1.2e21).toString()
                                                                      };
                                                              })
                                                          ],
                                                          tl: {
                                                            hd: [
                                                              "toStringWithRadix - radix:2",
                                                              (function (param) {
                                                                  return {
                                                                          TAG: /* Eq */0,
                                                                          _0: "1111011.0111010010111100011010100111111011111001110111",
                                                                          _1: (123.456).toString(2)
                                                                        };
                                                                })
                                                            ],
                                                            tl: {
                                                              hd: [
                                                                "toStringWithRadix - radix:16",
                                                                (function (param) {
                                                                    return {
                                                                            TAG: /* Eq */0,
                                                                            _0: "7b.74bc6a7ef9dc",
                                                                            _1: (123.456).toString(16)
                                                                          };
                                                                  })
                                                              ],
                                                              tl: {
                                                                hd: [
                                                                  "toStringWithRadix - radix:36",
                                                                  (function (param) {
                                                                      return {
                                                                              TAG: /* Eq */0,
                                                                              _0: "3f",
                                                                              _1: (123).toString(36)
                                                                            };
                                                                    })
                                                                ],
                                                                tl: {
                                                                  hd: [
                                                                    "toStringWithRadix - radix:37",
                                                                    (function (param) {
                                                                        return {
                                                                                TAG: /* ThrowAny */7,
                                                                                _0: (function (param) {
                                                                                    (0).toString(37);
                                                                                    
                                                                                  })
                                                                              };
                                                                      })
                                                                  ],
                                                                  tl: {
                                                                    hd: [
                                                                      "toStringWithRadix - radix:1",
                                                                      (function (param) {
                                                                          return {
                                                                                  TAG: /* ThrowAny */7,
                                                                                  _0: (function (param) {
                                                                                      (0).toString(1);
                                                                                      
                                                                                    })
                                                                                };
                                                                        })
                                                                    ],
                                                                    tl: {
                                                                      hd: [
                                                                        "toStringWithRadix - radix:-1",
                                                                        (function (param) {
                                                                            return {
                                                                                    TAG: /* ThrowAny */7,
                                                                                    _0: (function (param) {
                                                                                        (0).toString(-1);
                                                                                        
                                                                                      })
                                                                                  };
                                                                          })
                                                                      ],
                                                                      tl: {
                                                                        hd: [
                                                                          "fromString - 123",
                                                                          (function (param) {
                                                                              return {
                                                                                      TAG: /* Eq */0,
                                                                                      _0: 123,
                                                                                      _1: Number("123")
                                                                                    };
                                                                            })
                                                                        ],
                                                                        tl: {
                                                                          hd: [
                                                                            "fromString - 12.3",
                                                                            (function (param) {
                                                                                return {
                                                                                        TAG: /* Eq */0,
                                                                                        _0: 12.3,
                                                                                        _1: Number("12.3")
                                                                                      };
                                                                              })
                                                                          ],
                                                                          tl: {
                                                                            hd: [
                                                                              "fromString - empty string",
                                                                              (function (param) {
                                                                                  return {
                                                                                          TAG: /* Eq */0,
                                                                                          _0: 0,
                                                                                          _1: Number("")
                                                                                        };
                                                                                })
                                                                            ],
                                                                            tl: {
                                                                              hd: [
                                                                                "fromString - 0x11",
                                                                                (function (param) {
                                                                                    return {
                                                                                            TAG: /* Eq */0,
                                                                                            _0: 17,
                                                                                            _1: Number("0x11")
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              tl: {
                                                                                hd: [
                                                                                  "fromString - 0b11",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              TAG: /* Eq */0,
                                                                                              _0: 3,
                                                                                              _1: Number("0b11")
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                tl: {
                                                                                  hd: [
                                                                                    "fromString - 0o11",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                TAG: /* Eq */0,
                                                                                                _0: 9,
                                                                                                _1: Number("0o11")
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  tl: {
                                                                                    hd: [
                                                                                      "fromString - invalid string",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  TAG: /* Eq */0,
                                                                                                  _0: true,
                                                                                                  _1: isNaN(Number("foo"))
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    tl: /* [] */0
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

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_float_test", suites);

exports.suites = suites;
/*  Not a pure module */
