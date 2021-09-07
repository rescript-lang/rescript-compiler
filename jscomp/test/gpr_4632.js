'use strict';


var myList = {
  hd: 1,
  tl: {
    hd: 2,
    tl: /* [] */0
  }
};

var T0;

if (myList) {
  T0 = {
    myList: myList,
    head: 1,
    tail: {
      hd: 2,
      tl: /* [] */0
    }
  };
} else {
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "gpr_4632.ml",
          6,
          6
        ],
        Error: new Error()
      };
}

var myList$1 = {
  hd: [
    1,
    2,
    3
  ],
  tl: /* [] */0
};

if (myList$1) {
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "gpr_4632.ml",
          12,
          6
        ],
        Error: new Error()
      };
}

throw {
      RE_EXN_ID: "Match_failure",
      _1: [
        "gpr_4632.ml",
        12,
        6
      ],
      Error: new Error()
    };

exports.T0 = T0;
exports.T1 = T1;
/* T0 Not a pure module */
