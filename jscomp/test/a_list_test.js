'use strict';

var Mt = require("./mt.js");
var Ext_list_test = require("./ext_list_test.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "drop",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: Ext_list_test.drop(3, /* constructor */{
                      tag: "::",
                      Arg0: 0,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 2,
                          Arg1: "[]"
                        }
                      }
                    }),
                Arg1: "[]"
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "drop1",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: Ext_list_test.drop(2, /* constructor */{
                        tag: "::",
                        Arg0: 0,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 2,
                            Arg1: "[]"
                          }
                        }
                      }),
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 2,
                    Arg1: "[]"
                  }
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "flat_map",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: /* constructor */{
                      tag: "::",
                      Arg0: 0,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 0,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 1,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 0,
                              Arg1: "[]"
                            }
                          }
                        }
                      }
                    },
                    Arg1: Ext_list_test.flat_map((function (x) {
                            if (x % 2 === 0) {
                              return /* constructor */{
                                      tag: "::",
                                      Arg0: 0,
                                      Arg1: "[]"
                                    };
                            } else {
                              return /* constructor */{
                                      tag: "::",
                                      Arg0: 1,
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: 1,
                                        Arg1: "[]"
                                      }
                                    };
                            }
                          }), /* constructor */{
                          tag: "::",
                          Arg0: 0,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 0,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 3,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 0,
                                Arg1: "[]"
                              }
                            }
                          }
                        })
                  };
          })
      ],
      Arg1: "[]"
    }
  }
};

Mt.from_pair_suites("A_list_test", suites);

exports.suites = suites;
/*  Not a pure module */
