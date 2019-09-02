'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

var list_suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "length",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: 1,
                Arg1: List.length(/* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        0,
                        1,
                        2,
                        3,
                        4
                      ],
                      Arg1: "[]"
                    })
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "length2",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: 5,
                  Arg1: List.length(/* constructor */{
                        tag: "::",
                        Arg0: 0,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 1,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 2,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 3,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 4,
                                Arg1: "[]"
                              }
                            }
                          }
                        }
                      })
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "long_length",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: 30000,
                    Arg1: List.length($$Array.to_list($$Array.init(30000, (function (param) {
                                    return 0;
                                  }))))
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "sort",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: List.sort(Caml_primitive.caml_int_compare, /* constructor */{
                            tag: "::",
                            Arg0: 4,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 1,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 2,
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: 3,
                                  Arg1: "[]"
                                }
                              }
                            }
                          }),
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 1,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 2,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 3,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 4,
                              Arg1: "[]"
                            }
                          }
                        }
                      }
                    };
            })
        ],
        Arg1: "[]"
      }
    }
  }
};

Mt.from_pair_suites("List_test", list_suites);

exports.list_suites = list_suites;
/*  Not a pure module */
