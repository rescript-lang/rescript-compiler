'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Genlex = require("../../lib/js/genlex.js");
var Stream = require("../../lib/js/stream.js");

var lexer = Genlex.make_lexer(/* constructor */{
      tag: "::",
      Arg0: "+",
      Arg1: /* constructor */{
        tag: "::",
        Arg0: "-",
        Arg1: /* constructor */{
          tag: "::",
          Arg0: "*",
          Arg1: /* constructor */{
            tag: "::",
            Arg0: "/",
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "let",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: "=",
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: "(",
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: ")",
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }
      }
    });

function to_list(s) {
  var _acc = "[]";
  while(true) {
    var acc = _acc;
    var v;
    try {
      v = Stream.next(s);
    }
    catch (exn){
      if (exn === Stream.Failure) {
        return List.rev(acc);
      } else {
        throw exn;
      }
    }
    _acc = /* constructor */{
      tag: "::",
      Arg0: v,
      Arg1: acc
    };
    continue ;
  };
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "lexer_stream_genlex",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: /* constructor */{
                  tag: "::",
                  Arg0: /* constructor */{
                    tag: "Int",
                    Arg0: 3
                  },
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* constructor */{
                      tag: "Kwd",
                      Arg0: "("
                    },
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* constructor */{
                        tag: "Int",
                        Arg0: 3
                      },
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* constructor */{
                          tag: "Kwd",
                          Arg0: "+"
                        },
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* constructor */{
                            tag: "Int",
                            Arg0: 2
                          },
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* constructor */{
                              tag: "Int",
                              Arg0: -1
                            },
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* constructor */{
                                tag: "Kwd",
                                Arg0: ")"
                              },
                              Arg1: "[]"
                            }
                          }
                        }
                      }
                    }
                  }
                },
                Arg1: to_list(lexer(Stream.of_string("3(3 + 2 -1)")))
              };
      })
  ],
  Arg1: "[]"
};

Mt.from_pair_suites("Genlex_test", suites);

exports.lexer = lexer;
exports.to_list = to_list;
exports.suites = suites;
/* lexer Not a pure module */
