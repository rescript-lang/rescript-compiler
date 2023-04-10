'use strict';

var Mt = require("./mt.js");
var Js_list = require("../../lib/js/js_list.js");
var Js_vector = require("../../lib/js/js_vector.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
}

eq("File \"js_list_test.res\", line 11, characters 4-11", Js_list.flatten({
          hd: {
            hd: 1,
            tl: {
              hd: 2,
              tl: /* [] */0
            }
          },
          tl: {
            hd: {
              hd: 3,
              tl: /* [] */0
            },
            tl: {
              hd: /* [] */0,
              tl: {
                hd: {
                  hd: 1,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 3,
                      tl: /* [] */0
                    }
                  }
                },
                tl: /* [] */0
              }
            }
          }
        }), {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: {
            hd: 1,
            tl: {
              hd: 2,
              tl: {
                hd: 3,
                tl: /* [] */0
              }
            }
          }
        }
      }
    });

eq("File \"js_list_test.res\", line 15, characters 5-12", Js_list.filterMap((function (x) {
            if (x % 2 === 0) {
              return x;
            }
            
          }), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: {
                      hd: 7,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }), {
      hd: 2,
      tl: {
        hd: 4,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    });

eq("File \"js_list_test.res\", line 22, characters 5-12", Js_list.filterMap((function (x) {
            if (x % 2 === 0) {
              return x;
            }
            
          }), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }), {
      hd: 2,
      tl: {
        hd: 4,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    });

eq("File \"js_list_test.res\", line 29, characters 5-12", Js_list.countBy((function (x) {
            return x % 2 === 0;
          }), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: {
                  hd: 5,
                  tl: {
                    hd: 6,
                    tl: /* [] */0
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

eq("File \"js_list_test.res\", line 31, characters 5-12", Js_list.countBy((function (x) {
            return x % 2 === 0;
          }), v), 50000);

var vv = Js_list.foldRight((function (x, y) {
        return {
                hd: x,
                tl: y
              };
      }), v, /* [] */0);

eq("File \"js_list_test.res\", line 33, characters 5-12", true, Js_list.equal((function (x, y) {
            return x === y;
          }), v, vv));

var vvv = Js_list.filter((function (x) {
        return x % 10 === 0;
      }), vv);

eq("File \"js_list_test.res\", line 36, characters 5-12", Js_list.length(vvv), 10000);

function f$1(x) {
  return Math.imul(x, 10);
}

eq("File \"js_list_test.res\", line 37, characters 5-12", true, Js_list.equal((function (x, y) {
            return x === y;
          }), vvv, Js_vector.toList(Js_vector.init(10000, f$1))));

Mt.from_pair_suites("Js_list_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
