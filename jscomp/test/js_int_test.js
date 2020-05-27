'use strict';

var Mt = require("./mt.js");

var suites_0 = [
  "toExponential",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: "1.23456e+5",
              _1: (123456).toExponential()
            };
    })
];

var suites_1 = {
  hd: [
    "toExponentialWithPrecision - digits:2",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: "1.23e+5",
                _1: (123456).toExponential(2)
              };
      })
  ],
  tl: {
    hd: [
      "toExponentialWithPrecision - digits:4",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: "1.2346e+5",
                  _1: (123456).toExponential(4)
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
          "File \"js_int_test.ml\", line 12, characters 3-10",
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
              "toPrecision",
              (function (param) {
                  return {
                          TAG: /* Eq */0,
                          _0: "123456",
                          _1: (123456).toPrecision()
                        };
                })
            ],
            tl: {
              hd: [
                "toPrecisionWithPrecision - digits:2",
                (function (param) {
                    return {
                            TAG: /* Eq */0,
                            _0: "1.2e+5",
                            _1: (123456).toPrecision(2)
                          };
                  })
              ],
              tl: {
                hd: [
                  "toPrecisionWithPrecision - digits:4",
                  (function (param) {
                      return {
                              TAG: /* Eq */0,
                              _0: "1.235e+5",
                              _1: (123456).toPrecision(4)
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
                      "File \"js_int_test.ml\", line 25, characters 3-10",
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
                                      _0: "123",
                                      _1: (123).toString()
                                    };
                            })
                        ],
                        tl: {
                          hd: [
                            "toStringWithRadix - radix:2",
                            (function (param) {
                                return {
                                        TAG: /* Eq */0,
                                        _0: "11110001001000000",
                                        _1: (123456).toString(2)
                                      };
                              })
                          ],
                          tl: {
                            hd: [
                              "toStringWithRadix - radix:16",
                              (function (param) {
                                  return {
                                          TAG: /* Eq */0,
                                          _0: "1e240",
                                          _1: (123456).toString(16)
                                        };
                                })
                            ],
                            tl: {
                              hd: [
                                "toStringWithRadix - radix:36",
                                (function (param) {
                                    return {
                                            TAG: /* Eq */0,
                                            _0: "2n9c",
                                            _1: (123456).toString(36)
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
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_int_test", suites);

exports.suites = suites;
/*  Not a pure module */
