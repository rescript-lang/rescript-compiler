'use strict';

var Mt = require("./mt.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites_0 = [
  "_NaN <> _NaN",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: false,
              _1: NaN === NaN
            };
    })
];

var suites_1 = /* :: */{
  _0: [
    "isNaN - _NaN",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: true,
                _1: isNaN(NaN)
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "isNaN - 0.",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: false,
                  _1: isNaN(0)
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "isFinite - infinity",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: false,
                    _1: isFinite(Pervasives.infinity)
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "isFinite - neg_infinity",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: false,
                      _1: isFinite(Pervasives.neg_infinity)
                    };
            })
        ],
        _1: /* :: */{
          _0: [
            "isFinite - _NaN",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: false,
                        _1: isFinite(NaN)
                      };
              })
          ],
          _1: /* :: */{
            _0: [
              "isFinite - 0.",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: true,
                          _1: isFinite(0)
                        };
                })
            ],
            _1: /* :: */{
              _0: [
                "toExponential",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: "1.23456e+2",
                            _1: (123.456).toExponential()
                          };
                  })
              ],
              _1: /* :: */{
                _0: [
                  "toExponential - large number",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: "1.2e+21",
                              _1: (1.2e21).toExponential()
                            };
                    })
                ],
                _1: /* :: */{
                  _0: [
                    "toExponentialWithPrecision - digits:2",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: "1.23e+2",
                                _1: (123.456).toExponential(2)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: [
                      "toExponentialWithPrecision - digits:4",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: "1.2346e+2",
                                  _1: (123.456).toExponential(4)
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: [
                        "toExponentialWithPrecision - digits:20",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: "0.00000000000000000000e+0",
                                    _1: (0).toExponential(20)
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: [
                          "File \"js_float_test.ml\", line 31, characters 3-10",
                          (function (param) {
                              return {
                                      tag: /* ThrowAny */7,
                                      _0: (function (param) {
                                          (0).toExponential(101);
                                          
                                        })
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: [
                            "toExponentialWithPrecision - digits:-1",
                            (function (param) {
                                return {
                                        tag: /* ThrowAny */7,
                                        _0: (function (param) {
                                            (0).toExponential(-1);
                                            
                                          })
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: [
                              "toFixed",
                              (function (param) {
                                  return {
                                          tag: /* Eq */0,
                                          _0: "123",
                                          _1: (123.456).toFixed()
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: [
                                "toFixed - large number",
                                (function (param) {
                                    return {
                                            tag: /* Eq */0,
                                            _0: "1.2e+21",
                                            _1: (1.2e21).toFixed()
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: [
                                  "toFixedWithPrecision - digits:2",
                                  (function (param) {
                                      return {
                                              tag: /* Eq */0,
                                              _0: "123.46",
                                              _1: (123.456).toFixed(2)
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: [
                                    "toFixedWithPrecision - digits:4",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: "123.4560",
                                                _1: (123.456).toFixed(4)
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: [
                                      "toFixedWithPrecision - digits:20",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: "0.00000000000000000000",
                                                  _1: (0).toFixed(20)
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: [
                                        "toFixedWithPrecision - digits:101",
                                        (function (param) {
                                            return {
                                                    tag: /* ThrowAny */7,
                                                    _0: (function (param) {
                                                        (0).toFixed(101);
                                                        
                                                      })
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: [
                                          "toFixedWithPrecision - digits:-1",
                                          (function (param) {
                                              return {
                                                      tag: /* ThrowAny */7,
                                                      _0: (function (param) {
                                                          (0).toFixed(-1);
                                                          
                                                        })
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: [
                                            "toPrecision",
                                            (function (param) {
                                                return {
                                                        tag: /* Eq */0,
                                                        _0: "123.456",
                                                        _1: (123.456).toPrecision()
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: [
                                              "toPrecision - large number",
                                              (function (param) {
                                                  return {
                                                          tag: /* Eq */0,
                                                          _0: "1.2e+21",
                                                          _1: (1.2e21).toPrecision()
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: [
                                                "toPrecisionWithPrecision - digits:2",
                                                (function (param) {
                                                    return {
                                                            tag: /* Eq */0,
                                                            _0: "1.2e+2",
                                                            _1: (123.456).toPrecision(2)
                                                          };
                                                  })
                                              ],
                                              _1: /* :: */{
                                                _0: [
                                                  "toPrecisionWithPrecision - digits:4",
                                                  (function (param) {
                                                      return {
                                                              tag: /* Eq */0,
                                                              _0: "123.5",
                                                              _1: (123.456).toPrecision(4)
                                                            };
                                                    })
                                                ],
                                                _1: /* :: */{
                                                  _0: [
                                                    "toPrecisionWithPrecision - digits:20",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
                                                                _0: "0.0000000000000000000",
                                                                _1: (0).toPrecision(20)
                                                              };
                                                      })
                                                  ],
                                                  _1: /* :: */{
                                                    _0: [
                                                      "File \"js_float_test.ml\", line 61, characters 3-10",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* ThrowAny */7,
                                                                  _0: (function (param) {
                                                                      (0).toPrecision(101);
                                                                      
                                                                    })
                                                                };
                                                        })
                                                    ],
                                                    _1: /* :: */{
                                                      _0: [
                                                        "toPrecisionWithPrecision - digits:-1",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* ThrowAny */7,
                                                                    _0: (function (param) {
                                                                        (0).toPrecision(-1);
                                                                        
                                                                      })
                                                                  };
                                                          })
                                                      ],
                                                      _1: /* :: */{
                                                        _0: [
                                                          "toString",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Eq */0,
                                                                      _0: "1.23",
                                                                      _1: (1.23).toString()
                                                                    };
                                                            })
                                                        ],
                                                        _1: /* :: */{
                                                          _0: [
                                                            "toString - large number",
                                                            (function (param) {
                                                                return {
                                                                        tag: /* Eq */0,
                                                                        _0: "1.2e+21",
                                                                        _1: (1.2e21).toString()
                                                                      };
                                                              })
                                                          ],
                                                          _1: /* :: */{
                                                            _0: [
                                                              "toStringWithRadix - radix:2",
                                                              (function (param) {
                                                                  return {
                                                                          tag: /* Eq */0,
                                                                          _0: "1111011.0111010010111100011010100111111011111001110111",
                                                                          _1: (123.456).toString(2)
                                                                        };
                                                                })
                                                            ],
                                                            _1: /* :: */{
                                                              _0: [
                                                                "toStringWithRadix - radix:16",
                                                                (function (param) {
                                                                    return {
                                                                            tag: /* Eq */0,
                                                                            _0: "7b.74bc6a7ef9dc",
                                                                            _1: (123.456).toString(16)
                                                                          };
                                                                  })
                                                              ],
                                                              _1: /* :: */{
                                                                _0: [
                                                                  "toStringWithRadix - radix:36",
                                                                  (function (param) {
                                                                      return {
                                                                              tag: /* Eq */0,
                                                                              _0: "3f",
                                                                              _1: (123).toString(36)
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: [
                                                                    "toStringWithRadix - radix:37",
                                                                    (function (param) {
                                                                        return {
                                                                                tag: /* ThrowAny */7,
                                                                                _0: (function (param) {
                                                                                    (0).toString(37);
                                                                                    
                                                                                  })
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: [
                                                                      "toStringWithRadix - radix:1",
                                                                      (function (param) {
                                                                          return {
                                                                                  tag: /* ThrowAny */7,
                                                                                  _0: (function (param) {
                                                                                      (0).toString(1);
                                                                                      
                                                                                    })
                                                                                };
                                                                        })
                                                                    ],
                                                                    _1: /* :: */{
                                                                      _0: [
                                                                        "toStringWithRadix - radix:-1",
                                                                        (function (param) {
                                                                            return {
                                                                                    tag: /* ThrowAny */7,
                                                                                    _0: (function (param) {
                                                                                        (0).toString(-1);
                                                                                        
                                                                                      })
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: [
                                                                          "fromString - 123",
                                                                          (function (param) {
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: 123,
                                                                                      _1: Number("123")
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: [
                                                                            "fromString - 12.3",
                                                                            (function (param) {
                                                                                return {
                                                                                        tag: /* Eq */0,
                                                                                        _0: 12.3,
                                                                                        _1: Number("12.3")
                                                                                      };
                                                                              })
                                                                          ],
                                                                          _1: /* :: */{
                                                                            _0: [
                                                                              "fromString - empty string",
                                                                              (function (param) {
                                                                                  return {
                                                                                          tag: /* Eq */0,
                                                                                          _0: 0,
                                                                                          _1: Number("")
                                                                                        };
                                                                                })
                                                                            ],
                                                                            _1: /* :: */{
                                                                              _0: [
                                                                                "fromString - 0x11",
                                                                                (function (param) {
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: 17,
                                                                                            _1: Number("0x11")
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: [
                                                                                  "fromString - 0b11",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: 3,
                                                                                              _1: Number("0b11")
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: [
                                                                                    "fromString - 0o11",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                tag: /* Eq */0,
                                                                                                _0: 9,
                                                                                                _1: Number("0o11")
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  _1: /* :: */{
                                                                                    _0: [
                                                                                      "fromString - invalid string",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: true,
                                                                                                  _1: isNaN(Number("foo"))
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

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Js_float_test", suites);

exports.suites = suites;
/*  Not a pure module */
