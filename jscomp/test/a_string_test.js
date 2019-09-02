'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Ext_string_test = require("./ext_string_test.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "split",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: Ext_string_test.split(true, "hihi", /* "i" */105),
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "h",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "h",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "",
                      Arg1: "[]"
                    }
                  }
                }
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "split_non_empty",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: Ext_string_test.split(undefined, "hihi", /* "i" */105),
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: "h",
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: "h",
                      Arg1: "[]"
                    }
                  }
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "split_empty",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Ext_string_test.split(true, "", /* "i" */105),
                    Arg1: "[]"
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "split_normal",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: Ext_string_test.split(true, "h i i", /* " " */32),
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: "h",
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "i",
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: "i",
                            Arg1: "[]"
                          }
                        }
                      }
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "split_by",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: List.filter((function (s) {
                                  return s !== "";
                                }))(Ext_string_test.split_by(undefined, (function (x) {
                                    if (x === /* " " */32) {
                                      return true;
                                    } else {
                                      return x === /* "\t" */9;
                                    }
                                  }), "h hgso hgso \t hi")),
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: "h",
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: "hgso",
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: "hgso",
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: "hi",
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
  }
};

Mt.from_pair_suites("A_string_test", suites);

var split = Ext_string_test.split;

var split_by = Ext_string_test.split_by;

exports.split = split;
exports.split_by = split_by;
exports.suites = suites;
/*  Not a pure module */
