'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_null_undefined = require("../../lib/js/js_null_undefined.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "toOption - null",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: undefined,
                Arg1: undefined
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "toOption - undefined",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: undefined,
                  Arg1: undefined
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "toOption - empty",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: undefined,
                    Arg1: undefined
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "toOption - 'a",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: "foo",
                      Arg1: Caml_option.nullable_to_opt("foo")
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "return",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: "something",
                        Arg1: Caml_option.nullable_to_opt("something")
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "test - null",
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
                "test - undefined",
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
                  "test - empty",
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
                    "test - 'a",
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
                      "bind - null",
                      (function (param) {
                          return /* constructor */{
                                  tag: "StrictEq",
                                  Arg0: null,
                                  Arg1: Js_null_undefined.bind(null, (function (v) {
                                          return v;
                                        }))
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "bind - undefined",
                        (function (param) {
                            return /* constructor */{
                                    tag: "StrictEq",
                                    Arg0: undefined,
                                    Arg1: Js_null_undefined.bind(undefined, (function (v) {
                                            return v;
                                          }))
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "bind - empty",
                          (function (param) {
                              return /* constructor */{
                                      tag: "StrictEq",
                                      Arg0: undefined,
                                      Arg1: Js_null_undefined.bind(undefined, (function (v) {
                                              return v;
                                            }))
                                    };
                            })
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "bind - 'a",
                            (function (param) {
                                return /* constructor */{
                                        tag: "Eq",
                                        Arg0: 4,
                                        Arg1: Js_null_undefined.bind(2, (function (n) {
                                                return (n << 1);
                                              }))
                                      };
                              })
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "iter - null",
                              (function (param) {
                                  var hit = /* record */[/* contents */false];
                                  Js_null_undefined.iter(null, (function (param) {
                                          hit[0] = true;
                                          return /* () */0;
                                        }));
                                  return /* constructor */{
                                          tag: "Eq",
                                          Arg0: false,
                                          Arg1: hit[0]
                                        };
                                })
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "iter - undefined",
                                (function (param) {
                                    var hit = /* record */[/* contents */false];
                                    Js_null_undefined.iter(undefined, (function (param) {
                                            hit[0] = true;
                                            return /* () */0;
                                          }));
                                    return /* constructor */{
                                            tag: "Eq",
                                            Arg0: false,
                                            Arg1: hit[0]
                                          };
                                  })
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "iter - empty",
                                  (function (param) {
                                      var hit = /* record */[/* contents */false];
                                      Js_null_undefined.iter(undefined, (function (param) {
                                              hit[0] = true;
                                              return /* () */0;
                                            }));
                                      return /* constructor */{
                                              tag: "Eq",
                                              Arg0: false,
                                              Arg1: hit[0]
                                            };
                                    })
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "iter - 'a",
                                    (function (param) {
                                        var hit = /* record */[/* contents */0];
                                        Js_null_undefined.iter(2, (function (v) {
                                                hit[0] = v;
                                                return /* () */0;
                                              }));
                                        return /* constructor */{
                                                tag: "Eq",
                                                Arg0: 2,
                                                Arg1: hit[0]
                                              };
                                      })
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "fromOption - None",
                                      (function (param) {
                                          return /* constructor */{
                                                  tag: "Eq",
                                                  Arg0: undefined,
                                                  Arg1: Js_null_undefined.fromOption(undefined)
                                                };
                                        })
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "fromOption - Some",
                                        (function (param) {
                                            return /* constructor */{
                                                    tag: "Eq",
                                                    Arg0: 2,
                                                    Arg1: Js_null_undefined.fromOption(2)
                                                  };
                                          })
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "null <> undefined",
                                          (function (param) {
                                              return /* constructor */{
                                                      tag: "Ok",
                                                      Arg0: true
                                                    };
                                            })
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "null <> empty",
                                            (function (param) {
                                                return /* constructor */{
                                                        tag: "Ok",
                                                        Arg0: true
                                                      };
                                              })
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "undefined = empty",
                                              (function (param) {
                                                  return /* constructor */{
                                                          tag: "Ok",
                                                          Arg0: true
                                                        };
                                                })
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "File \"js_null_undefined_test.ml\", line 42, characters 2-9",
                                                (function (param) {
                                                    return /* constructor */{
                                                            tag: "Ok",
                                                            Arg0: true
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

Mt.from_pair_suites("Js_null_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
