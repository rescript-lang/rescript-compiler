'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_math = require("../../lib/js/js_math.js");

var suites_0 = /* tuple */[
  "_E",
  (function (param) {
      return {
              tag: /* ApproxThreshold */6,
              _0: 0.001,
              _1: 2.718,
              _2: Math.E
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "_LN2",
    (function (param) {
        return {
                tag: /* ApproxThreshold */6,
                _0: 0.001,
                _1: 0.693,
                _2: Math.LN2
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "_LN10",
      (function (param) {
          return {
                  tag: /* ApproxThreshold */6,
                  _0: 0.001,
                  _1: 2.303,
                  _2: Math.LN10
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "_LOG2E",
        (function (param) {
            return {
                    tag: /* ApproxThreshold */6,
                    _0: 0.001,
                    _1: 1.443,
                    _2: Math.LOG2E
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "_LOG10E",
          (function (param) {
              return {
                      tag: /* ApproxThreshold */6,
                      _0: 0.001,
                      _1: 0.434,
                      _2: Math.LOG10E
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "_PI",
            (function (param) {
                return {
                        tag: /* ApproxThreshold */6,
                        _0: 0.00001,
                        _1: 3.14159,
                        _2: Math.PI
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "_SQRT1_2",
              (function (param) {
                  return {
                          tag: /* ApproxThreshold */6,
                          _0: 0.001,
                          _1: 0.707,
                          _2: Math.SQRT1_2
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "_SQRT2",
                (function (param) {
                    return {
                            tag: /* ApproxThreshold */6,
                            _0: 0.001,
                            _1: 1.414,
                            _2: Math.SQRT2
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "abs_int",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: 4,
                              _1: Math.abs(-4)
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "abs_float",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: 1.2,
                                _1: Math.abs(-1.2)
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "acos",
                      (function (param) {
                          return {
                                  tag: /* ApproxThreshold */6,
                                  _0: 0.001,
                                  _1: 1.159,
                                  _2: Math.acos(0.4)
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "acosh",
                        (function (param) {
                            return {
                                    tag: /* ApproxThreshold */6,
                                    _0: 0.001,
                                    _1: 0.622,
                                    _2: Math.acosh(1.2)
                                  };
                          })
                      ],
                      _1: /* :: */{
                        _0: /* tuple */[
                          "asin",
                          (function (param) {
                              return {
                                      tag: /* ApproxThreshold */6,
                                      _0: 0.001,
                                      _1: 0.411,
                                      _2: Math.asin(0.4)
                                    };
                            })
                        ],
                        _1: /* :: */{
                          _0: /* tuple */[
                            "asinh",
                            (function (param) {
                                return {
                                        tag: /* ApproxThreshold */6,
                                        _0: 0.001,
                                        _1: 0.390,
                                        _2: Math.asinh(0.4)
                                      };
                              })
                          ],
                          _1: /* :: */{
                            _0: /* tuple */[
                              "atan",
                              (function (param) {
                                  return {
                                          tag: /* ApproxThreshold */6,
                                          _0: 0.001,
                                          _1: 0.380,
                                          _2: Math.atan(0.4)
                                        };
                                })
                            ],
                            _1: /* :: */{
                              _0: /* tuple */[
                                "atanh",
                                (function (param) {
                                    return {
                                            tag: /* ApproxThreshold */6,
                                            _0: 0.001,
                                            _1: 0.423,
                                            _2: Math.atanh(0.4)
                                          };
                                  })
                              ],
                              _1: /* :: */{
                                _0: /* tuple */[
                                  "atan2",
                                  (function (param) {
                                      return {
                                              tag: /* ApproxThreshold */6,
                                              _0: 0.001,
                                              _1: 0.588,
                                              _2: Math.atan2(0.4, 0.6)
                                            };
                                    })
                                ],
                                _1: /* :: */{
                                  _0: /* tuple */[
                                    "cbrt",
                                    (function (param) {
                                        return {
                                                tag: /* Eq */0,
                                                _0: 2,
                                                _1: Math.cbrt(8)
                                              };
                                      })
                                  ],
                                  _1: /* :: */{
                                    _0: /* tuple */[
                                      "unsafe_ceil_int",
                                      (function (param) {
                                          return {
                                                  tag: /* Eq */0,
                                                  _0: 4,
                                                  _1: Math.ceil(3.2)
                                                };
                                        })
                                    ],
                                    _1: /* :: */{
                                      _0: /* tuple */[
                                        "ceil_int",
                                        (function (param) {
                                            return {
                                                    tag: /* Eq */0,
                                                    _0: 4,
                                                    _1: Js_math.ceil_int(3.2)
                                                  };
                                          })
                                      ],
                                      _1: /* :: */{
                                        _0: /* tuple */[
                                          "ceil_float",
                                          (function (param) {
                                              return {
                                                      tag: /* Eq */0,
                                                      _0: 4,
                                                      _1: Math.ceil(3.2)
                                                    };
                                            })
                                        ],
                                        _1: /* :: */{
                                          _0: /* tuple */[
                                            "cos",
                                            (function (param) {
                                                return {
                                                        tag: /* ApproxThreshold */6,
                                                        _0: 0.001,
                                                        _1: 0.921,
                                                        _2: Math.cos(0.4)
                                                      };
                                              })
                                          ],
                                          _1: /* :: */{
                                            _0: /* tuple */[
                                              "cosh",
                                              (function (param) {
                                                  return {
                                                          tag: /* ApproxThreshold */6,
                                                          _0: 0.001,
                                                          _1: 1.081,
                                                          _2: Math.cosh(0.4)
                                                        };
                                                })
                                            ],
                                            _1: /* :: */{
                                              _0: /* tuple */[
                                                "exp",
                                                (function (param) {
                                                    return {
                                                            tag: /* ApproxThreshold */6,
                                                            _0: 0.001,
                                                            _1: 1.491,
                                                            _2: Math.exp(0.4)
                                                          };
                                                  })
                                              ],
                                              _1: /* :: */{
                                                _0: /* tuple */[
                                                  "expm1",
                                                  (function (param) {
                                                      return {
                                                              tag: /* ApproxThreshold */6,
                                                              _0: 0.001,
                                                              _1: 0.491,
                                                              _2: Math.expm1(0.4)
                                                            };
                                                    })
                                                ],
                                                _1: /* :: */{
                                                  _0: /* tuple */[
                                                    "unsafe_floor_int",
                                                    (function (param) {
                                                        return {
                                                                tag: /* Eq */0,
                                                                _0: 3,
                                                                _1: Math.floor(3.2)
                                                              };
                                                      })
                                                  ],
                                                  _1: /* :: */{
                                                    _0: /* tuple */[
                                                      "floor_int",
                                                      (function (param) {
                                                          return {
                                                                  tag: /* Eq */0,
                                                                  _0: 3,
                                                                  _1: Js_math.floor_int(3.2)
                                                                };
                                                        })
                                                    ],
                                                    _1: /* :: */{
                                                      _0: /* tuple */[
                                                        "floor_float",
                                                        (function (param) {
                                                            return {
                                                                    tag: /* Eq */0,
                                                                    _0: 3,
                                                                    _1: Math.floor(3.2)
                                                                  };
                                                          })
                                                      ],
                                                      _1: /* :: */{
                                                        _0: /* tuple */[
                                                          "fround",
                                                          (function (param) {
                                                              return {
                                                                      tag: /* Approx */5,
                                                                      _0: 3.2,
                                                                      _1: Math.fround(3.2)
                                                                    };
                                                            })
                                                        ],
                                                        _1: /* :: */{
                                                          _0: /* tuple */[
                                                            "hypot",
                                                            (function (param) {
                                                                return {
                                                                        tag: /* ApproxThreshold */6,
                                                                        _0: 0.001,
                                                                        _1: 0.721,
                                                                        _2: Math.hypot(0.4, 0.6)
                                                                      };
                                                              })
                                                          ],
                                                          _1: /* :: */{
                                                            _0: /* tuple */[
                                                              "hypotMany",
                                                              (function (param) {
                                                                  return {
                                                                          tag: /* ApproxThreshold */6,
                                                                          _0: 0.001,
                                                                          _1: 1.077,
                                                                          _2: Math.hypot(0.4, 0.6, 0.8)
                                                                        };
                                                                })
                                                            ],
                                                            _1: /* :: */{
                                                              _0: /* tuple */[
                                                                "imul",
                                                                (function (param) {
                                                                    return {
                                                                            tag: /* Eq */0,
                                                                            _0: 8,
                                                                            _1: Math.imul(4, 2)
                                                                          };
                                                                  })
                                                              ],
                                                              _1: /* :: */{
                                                                _0: /* tuple */[
                                                                  "log",
                                                                  (function (param) {
                                                                      return {
                                                                              tag: /* ApproxThreshold */6,
                                                                              _0: 0.001,
                                                                              _1: -0.916,
                                                                              _2: Math.log(0.4)
                                                                            };
                                                                    })
                                                                ],
                                                                _1: /* :: */{
                                                                  _0: /* tuple */[
                                                                    "log1p",
                                                                    (function (param) {
                                                                        return {
                                                                                tag: /* ApproxThreshold */6,
                                                                                _0: 0.001,
                                                                                _1: 0.336,
                                                                                _2: Math.log1p(0.4)
                                                                              };
                                                                      })
                                                                  ],
                                                                  _1: /* :: */{
                                                                    _0: /* tuple */[
                                                                      "log10",
                                                                      (function (param) {
                                                                          return {
                                                                                  tag: /* ApproxThreshold */6,
                                                                                  _0: 0.001,
                                                                                  _1: -0.397,
                                                                                  _2: Math.log10(0.4)
                                                                                };
                                                                        })
                                                                    ],
                                                                    _1: /* :: */{
                                                                      _0: /* tuple */[
                                                                        "log2",
                                                                        (function (param) {
                                                                            return {
                                                                                    tag: /* ApproxThreshold */6,
                                                                                    _0: 0.001,
                                                                                    _1: -1.321,
                                                                                    _2: Math.log2(0.4)
                                                                                  };
                                                                          })
                                                                      ],
                                                                      _1: /* :: */{
                                                                        _0: /* tuple */[
                                                                          "max_int",
                                                                          (function (param) {
                                                                              return {
                                                                                      tag: /* Eq */0,
                                                                                      _0: 4,
                                                                                      _1: Math.max(2, 4)
                                                                                    };
                                                                            })
                                                                        ],
                                                                        _1: /* :: */{
                                                                          _0: /* tuple */[
                                                                            "maxMany_int",
                                                                            (function (param) {
                                                                                return {
                                                                                        tag: /* Eq */0,
                                                                                        _0: 4,
                                                                                        _1: Math.max(2, 4, 3)
                                                                                      };
                                                                              })
                                                                          ],
                                                                          _1: /* :: */{
                                                                            _0: /* tuple */[
                                                                              "max_float",
                                                                              (function (param) {
                                                                                  return {
                                                                                          tag: /* Eq */0,
                                                                                          _0: 4.2,
                                                                                          _1: Math.max(2.7, 4.2)
                                                                                        };
                                                                                })
                                                                            ],
                                                                            _1: /* :: */{
                                                                              _0: /* tuple */[
                                                                                "maxMany_float",
                                                                                (function (param) {
                                                                                    return {
                                                                                            tag: /* Eq */0,
                                                                                            _0: 4.2,
                                                                                            _1: Math.max(2.7, 4.2, 3.9)
                                                                                          };
                                                                                  })
                                                                              ],
                                                                              _1: /* :: */{
                                                                                _0: /* tuple */[
                                                                                  "min_int",
                                                                                  (function (param) {
                                                                                      return {
                                                                                              tag: /* Eq */0,
                                                                                              _0: 2,
                                                                                              _1: Math.min(2, 4)
                                                                                            };
                                                                                    })
                                                                                ],
                                                                                _1: /* :: */{
                                                                                  _0: /* tuple */[
                                                                                    "minMany_int",
                                                                                    (function (param) {
                                                                                        return {
                                                                                                tag: /* Eq */0,
                                                                                                _0: 2,
                                                                                                _1: Math.min(2, 4, 3)
                                                                                              };
                                                                                      })
                                                                                  ],
                                                                                  _1: /* :: */{
                                                                                    _0: /* tuple */[
                                                                                      "min_float",
                                                                                      (function (param) {
                                                                                          return {
                                                                                                  tag: /* Eq */0,
                                                                                                  _0: 2.7,
                                                                                                  _1: Math.min(2.7, 4.2)
                                                                                                };
                                                                                        })
                                                                                    ],
                                                                                    _1: /* :: */{
                                                                                      _0: /* tuple */[
                                                                                        "minMany_float",
                                                                                        (function (param) {
                                                                                            return {
                                                                                                    tag: /* Eq */0,
                                                                                                    _0: 2.7,
                                                                                                    _1: Math.min(2.7, 4.2, 3.9)
                                                                                                  };
                                                                                          })
                                                                                      ],
                                                                                      _1: /* :: */{
                                                                                        _0: /* tuple */[
                                                                                          "random",
                                                                                          (function (param) {
                                                                                              var a = Math.random();
                                                                                              return {
                                                                                                      tag: /* Ok */4,
                                                                                                      _0: a >= 0 && a < 1
                                                                                                    };
                                                                                            })
                                                                                        ],
                                                                                        _1: /* :: */{
                                                                                          _0: /* tuple */[
                                                                                            "random_int",
                                                                                            (function (param) {
                                                                                                var a = Js_math.random_int(1, 3);
                                                                                                return {
                                                                                                        tag: /* Ok */4,
                                                                                                        _0: a >= 1 && a < 3
                                                                                                      };
                                                                                              })
                                                                                          ],
                                                                                          _1: /* :: */{
                                                                                            _0: /* tuple */[
                                                                                              "unsafe_round",
                                                                                              (function (param) {
                                                                                                  return {
                                                                                                          tag: /* Eq */0,
                                                                                                          _0: 3,
                                                                                                          _1: Math.round(3.2)
                                                                                                        };
                                                                                                })
                                                                                            ],
                                                                                            _1: /* :: */{
                                                                                              _0: /* tuple */[
                                                                                                "round",
                                                                                                (function (param) {
                                                                                                    return {
                                                                                                            tag: /* Eq */0,
                                                                                                            _0: 3,
                                                                                                            _1: Math.round(3.2)
                                                                                                          };
                                                                                                  })
                                                                                              ],
                                                                                              _1: /* :: */{
                                                                                                _0: /* tuple */[
                                                                                                  "sign_int",
                                                                                                  (function (param) {
                                                                                                      return {
                                                                                                              tag: /* Eq */0,
                                                                                                              _0: -1,
                                                                                                              _1: Math.sign(-4)
                                                                                                            };
                                                                                                    })
                                                                                                ],
                                                                                                _1: /* :: */{
                                                                                                  _0: /* tuple */[
                                                                                                    "sign_float",
                                                                                                    (function (param) {
                                                                                                        return {
                                                                                                                tag: /* Eq */0,
                                                                                                                _0: -1,
                                                                                                                _1: Math.sign(-4.2)
                                                                                                              };
                                                                                                      })
                                                                                                  ],
                                                                                                  _1: /* :: */{
                                                                                                    _0: /* tuple */[
                                                                                                      "sign_float -0",
                                                                                                      (function (param) {
                                                                                                          return {
                                                                                                                  tag: /* Eq */0,
                                                                                                                  _0: -0,
                                                                                                                  _1: Math.sign(-0)
                                                                                                                };
                                                                                                        })
                                                                                                    ],
                                                                                                    _1: /* :: */{
                                                                                                      _0: /* tuple */[
                                                                                                        "sin",
                                                                                                        (function (param) {
                                                                                                            return {
                                                                                                                    tag: /* ApproxThreshold */6,
                                                                                                                    _0: 0.001,
                                                                                                                    _1: 0.389,
                                                                                                                    _2: Math.sin(0.4)
                                                                                                                  };
                                                                                                          })
                                                                                                      ],
                                                                                                      _1: /* :: */{
                                                                                                        _0: /* tuple */[
                                                                                                          "sinh",
                                                                                                          (function (param) {
                                                                                                              return {
                                                                                                                      tag: /* ApproxThreshold */6,
                                                                                                                      _0: 0.001,
                                                                                                                      _1: 0.410,
                                                                                                                      _2: Math.sinh(0.4)
                                                                                                                    };
                                                                                                            })
                                                                                                        ],
                                                                                                        _1: /* :: */{
                                                                                                          _0: /* tuple */[
                                                                                                            "sqrt",
                                                                                                            (function (param) {
                                                                                                                return {
                                                                                                                        tag: /* ApproxThreshold */6,
                                                                                                                        _0: 0.001,
                                                                                                                        _1: 0.632,
                                                                                                                        _2: Math.sqrt(0.4)
                                                                                                                      };
                                                                                                              })
                                                                                                          ],
                                                                                                          _1: /* :: */{
                                                                                                            _0: /* tuple */[
                                                                                                              "tan",
                                                                                                              (function (param) {
                                                                                                                  return {
                                                                                                                          tag: /* ApproxThreshold */6,
                                                                                                                          _0: 0.001,
                                                                                                                          _1: 0.422,
                                                                                                                          _2: Math.tan(0.4)
                                                                                                                        };
                                                                                                                })
                                                                                                            ],
                                                                                                            _1: /* :: */{
                                                                                                              _0: /* tuple */[
                                                                                                                "tanh",
                                                                                                                (function (param) {
                                                                                                                    return {
                                                                                                                            tag: /* ApproxThreshold */6,
                                                                                                                            _0: 0.001,
                                                                                                                            _1: 0.379,
                                                                                                                            _2: Math.tanh(0.4)
                                                                                                                          };
                                                                                                                  })
                                                                                                              ],
                                                                                                              _1: /* :: */{
                                                                                                                _0: /* tuple */[
                                                                                                                  "unsafe_trunc",
                                                                                                                  (function (param) {
                                                                                                                      return {
                                                                                                                              tag: /* Eq */0,
                                                                                                                              _0: 4,
                                                                                                                              _1: Math.trunc(4.2156)
                                                                                                                            };
                                                                                                                    })
                                                                                                                ],
                                                                                                                _1: /* :: */{
                                                                                                                  _0: /* tuple */[
                                                                                                                    "trunc",
                                                                                                                    (function (param) {
                                                                                                                        return {
                                                                                                                                tag: /* Eq */0,
                                                                                                                                _0: 4,
                                                                                                                                _1: Math.trunc(4.2156)
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

Mt.from_pair_suites("Js_math_test", suites);

exports.suites = suites;
/*  Not a pure module */
