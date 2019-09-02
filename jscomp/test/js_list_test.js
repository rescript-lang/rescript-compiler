'use strict';

var Mt = require("./mt.js");
var Js_list = require("../../lib/js/js_list.js");
var Js_vector = require("../../lib/js/js_vector.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

eq("File \"js_list_test.ml\", line 11, characters 7-14", Js_list.flatten(/* constructor */{
          tag: "::",
          Arg0: /* constructor */{
            tag: "::",
            Arg0: 1,
            Arg1: /* constructor */{
              tag: "::",
              Arg0: 2,
              Arg1: "[]"
            }
          },
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "::",
              Arg0: 3,
              Arg1: "[]"
            },
            Arg1: /* constructor */{
              tag: "::",
              Arg0: "[]",
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* constructor */{
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
                },
                Arg1: "[]"
              }
            }
          }
        }), /* constructor */{
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
        }
      }
    });

eq("File \"js_list_test.ml\", line 14, characters 7-14", Js_list.filterMap((function (x) {
            if (x % 2 === 0) {
              return x;
            }
            
          }), /* constructor */{
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
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 5,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 6,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 7,
                      Arg1: "[]"
                    }
                  }
                }
              }
            }
          }
        }), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 4,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 6,
          Arg1: "[]"
        }
      }
    });

eq("File \"js_list_test.ml\", line 17, characters 7-14", Js_list.filterMap((function (x) {
            if (x % 2 === 0) {
              return x;
            }
            
          }), /* constructor */{
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
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 5,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 6,
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }), /* constructor */{
      tag: "::",
      Arg0: 2,
      Arg1: /* constructor */{
        tag: "::",
        Arg0: 4,
        Arg1: /* constructor */{
          tag: "::",
          Arg0: 6,
          Arg1: "[]"
        }
      }
    });

eq("File \"js_list_test.ml\", line 20, characters 7-14", Js_list.countBy((function (x) {
            return x % 2 === 0;
          }), /* constructor */{
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
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: 5,
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: 6,
                    Arg1: "[]"
                  }
                }
              }
            }
          }
        }), 3);

function f(i) {
  return i;
}

var v = Js_vector.toList(Js_vector.init(100000, f));

eq("File \"js_list_test.ml\", line 23, characters 7-14", Js_list.countBy((function (x) {
            return x % 2 === 0;
          }), v), 50000);

var vv = Js_list.foldRight((function (x, y) {
        return /* constructor */{
                tag: "::",
                Arg0: x,
                Arg1: y
              };
      }), v, "[]");

eq("File \"js_list_test.ml\", line 27, characters 7-14", true, Js_list.equal((function (x, y) {
            return x === y;
          }), v, vv));

var vvv = Js_list.filter((function (x) {
        return x % 10 === 0;
      }), vv);

eq("File \"js_list_test.ml\", line 31, characters 7-14", Js_list.length(vvv), 10000);

function f$1(x) {
  return Caml_int32.imul(x, 10);
}

eq("File \"js_list_test.ml\", line 32, characters 7-14", true, Js_list.equal((function (x, y) {
            return x === y;
          }), vvv, Js_vector.toList(Js_vector.init(10000, f$1))));

Mt.from_pair_suites("Js_list_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
