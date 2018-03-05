'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

function f1(x) {
  if (x !== null) {
    return x + 1 | 0;
  } else {
    return 3;
  }
}

function f2(x) {
  if (x !== null) {
    return x + 1 | 0;
  } else {
    return 3;
  }
}

function f5(h, _) {
  var u = Curry._1(h, 32);
  if (u !== null) {
    return u + 1 | 0;
  } else {
    return 3;
  }
}

function f4(h, x) {
  var u = Curry._1(h, 32);
  var v = 32 + x | 0;
  if (u !== null) {
    return u + 1 | 0;
  } else {
    return 1 + v | 0;
  }
}

function f6(x, y) {
  return +(x === y);
}

function f7(x) {
  return x;
}

function f8(x) {
  if (x !== null) {
    if (x !== null) {
      return 0;
    } else {
      return 1;
    }
  } else {
    return 2;
  }
}

var u = f8(/* None */0);

function f9(x) {
  if (x === null) {
    return /* None */0;
  } else {
    return [x];
  }
}

function f10(x) {
  return +(x === null);
}

var f11 = +(3 === null);

var Test_null = /* module */[
  /* f1 */f1,
  /* f2 */f2,
  /* f5 */f5,
  /* f4 */f4,
  /* f6 */f6,
  /* f7 */f7,
  /* f8 */f8,
  /* u */u,
  /* f9 */f9,
  /* f10 */f10,
  /* f11 */f11
];

function f1$1(x) {
  if (x !== undefined) {
    return x + 1 | 0;
  } else {
    return 3;
  }
}

function f2$1(x) {
  if (x !== undefined) {
    return x + 1 | 0;
  } else {
    return 3;
  }
}

function f5$1(h, _) {
  var u = Curry._1(h, 32);
  if (u !== undefined) {
    return u + 1 | 0;
  } else {
    return 3;
  }
}

function f4$1(h, x) {
  var u = Curry._1(h, 32);
  var v = 32 + x | 0;
  if (u !== undefined) {
    return u + 1 | 0;
  } else {
    return 1 + v | 0;
  }
}

function f6$1(x, y) {
  return +(x === y);
}

function f7$1(x) {
  return x;
}

function f8$1(x) {
  if (x !== undefined) {
    if (x !== undefined) {
      return 0;
    } else {
      return 1;
    }
  } else {
    return 2;
  }
}

var u$1 = f8$1(/* None */0);

function f9$1(x) {
  if (x === undefined) {
    return /* None */0;
  } else {
    return [x];
  }
}

function f10$1(x) {
  return +(x === undefined);
}

var f11$1 = +(3 === undefined);

var Test_def = /* module */[
  /* f1 */f1$1,
  /* f2 */f2$1,
  /* f5 */f5$1,
  /* f4 */f4$1,
  /* f6 */f6$1,
  /* f7 */f7$1,
  /* f8 */f8$1,
  /* u */u$1,
  /* f9 */f9$1,
  /* f10 */f10$1,
  /* f11 */f11$1
];

function f1$2(x) {
  if (x == null) {
    return 3;
  } else {
    return x + 1 | 0;
  }
}

function f2$2(x) {
  if (x == null) {
    return 3;
  } else {
    return x + 1 | 0;
  }
}

function f5$2(h, _) {
  var u = Curry._1(h, 32);
  if (u == null) {
    return 3;
  } else {
    return u + 1 | 0;
  }
}

function f4$2(h, x) {
  var u = Curry._1(h, 32);
  var v = 32 + x | 0;
  if (u == null) {
    return 1 + v | 0;
  } else {
    return u + 1 | 0;
  }
}

function f6$2(x, y) {
  return +(x === y);
}

function f7$2(x) {
  return x;
}

function f8$2(x) {
  if (x == null) {
    return 2;
  } else if (x == null) {
    return 1;
  } else {
    return 0;
  }
}

var u$2 = f8$2(/* None */0);

function f9$2(x) {
  if (x == null) {
    return /* None */0;
  } else {
    return [x];
  }
}

function f10$2(x) {
  return +(x == null);
}

var f11$2 = /* false */0;

var Test_null_def = /* module */[
  /* f1 */f1$2,
  /* f2 */f2$2,
  /* f5 */f5$2,
  /* f4 */f4$2,
  /* f6 */f6$2,
  /* f7 */f7$2,
  /* f8 */f8$2,
  /* u */u$2,
  /* f9 */f9$2,
  /* f10 */f10$2,
  /* f11 */f11$2
];

eq("File \"test_zero_nullable.ml\", line 227, characters 7-14", f1$2(0), 1);

eq("File \"test_zero_nullable.ml\", line 228, characters 7-14", f1$2((null)), 3);

eq("File \"test_zero_nullable.ml\", line 229, characters 7-14", f1$2((undefined)), 3);

eq("File \"test_zero_nullable.ml\", line 231, characters 7-14", f1(0), 1);

eq("File \"test_zero_nullable.ml\", line 232, characters 7-14", f1((null)), 3);

eq("File \"test_zero_nullable.ml\", line 234, characters 7-14", f1$1(0), 1);

eq("File \"test_zero_nullable.ml\", line 235, characters 7-14", f1$1((undefined)), 3);

Mt.from_pair_suites("test_zero_nullable.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Test_null = Test_null;
exports.Test_def = Test_def;
exports.Test_null_def = Test_null_def;
/* u Not a pure module */
