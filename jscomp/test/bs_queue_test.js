// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_obj = require("../../lib/js/caml_obj.js");
let Belt_Array = require("../../lib/js/belt_Array.js");
let Belt_MutableQueue = require("../../lib/js/belt_MutableQueue.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

function does_raise(f, q) {
  try {
    f(q);
    return false;
  } catch (exn) {
    return true;
  }
}

function $plus$plus(q, x) {
  Belt_MutableQueue.add(q, x);
  return q;
}

let q = {
  length: 0,
  first: undefined,
  last: undefined
};

if (!(Caml_obj.equal(Belt_MutableQueue.toArray(q), []) && q.length === 0)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        25,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray((Belt_MutableQueue.add(q, 1), q)), [1]) && q.length === 1)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        26,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray((Belt_MutableQueue.add(q, 2), q)), [
    1,
    2
  ]) && q.length === 2)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        27,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray((Belt_MutableQueue.add(q, 3), q)), [
    1,
    2,
    3
  ]) && q.length === 3)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        28,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray((Belt_MutableQueue.add(q, 4), q)), [
    1,
    2,
    3,
    4
  ]) && q.length === 4)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        29,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        30,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray(q), [
    2,
    3,
    4
  ]) && q.length === 3)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        31,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q) !== 2) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        32,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray(q), [
    3,
    4
  ]) && q.length === 2)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        33,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q) !== 3) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        34,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray(q), [4]) && q.length === 1)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        35,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q) !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        36,
        2
      ]
    }
  });
}

if (!(Caml_obj.equal(Belt_MutableQueue.toArray(q), []) && q.length === 0)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        37,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.popExn, q)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        38,
        2
      ]
    }
  });
}

let q$1 = {
  length: 0,
  first: undefined,
  last: undefined
};

if (Belt_MutableQueue.popExn((Belt_MutableQueue.add(q$1, 1), q$1)) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        43,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.popExn, q$1)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        44,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn((Belt_MutableQueue.add(q$1, 2), q$1)) !== 2) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        45,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.popExn, q$1)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        46,
        2
      ]
    }
  });
}

if (q$1.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        47,
        2
      ]
    }
  });
}

let q$2 = {
  length: 0,
  first: undefined,
  last: undefined
};

if (Belt_MutableQueue.peekExn((Belt_MutableQueue.add(q$2, 1), q$2)) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        52,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.peekExn((Belt_MutableQueue.add(q$2, 2), q$2)) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        53,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.peekExn((Belt_MutableQueue.add(q$2, 3), q$2)) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        54,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.peekExn(q$2) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        55,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q$2) !== 1) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        56,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.peekExn(q$2) !== 2) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        57,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q$2) !== 2) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        58,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.peekExn(q$2) !== 3) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        59,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.popExn(q$2) !== 3) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        60,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.peekExn, q$2)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        61,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.peekExn, q$2)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        62,
        2
      ]
    }
  });
}

let q$3 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i = 1; i <= 10; ++i) {
  Belt_MutableQueue.add(q$3, i);
}

Belt_MutableQueue.clear(q$3);

if (q$3.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        71,
        2
      ]
    }
  });
}

if (!does_raise(Belt_MutableQueue.popExn, q$3)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        72,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(q$3, {
    length: 0,
    first: undefined,
    last: undefined
  })) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        73,
        2
      ]
    }
  });
}

Belt_MutableQueue.add(q$3, 42);

if (Belt_MutableQueue.popExn(q$3) !== 42) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        75,
        2
      ]
    }
  });
}

let q1 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i$1 = 1; i$1 <= 10; ++i$1) {
  Belt_MutableQueue.add(q1, i$1);
}

let q2 = Belt_MutableQueue.copy(q1);

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1), [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        84,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2), [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        85,
        2
      ]
    }
  });
}

if (q1.length !== 10) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        86,
        2
      ]
    }
  });
}

if (q2.length !== 10) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        87,
        2
      ]
    }
  });
}

for (let i$2 = 1; i$2 <= 10; ++i$2) {
  if (Belt_MutableQueue.popExn(q1) !== i$2) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          89,
          4
        ]
      }
    });
  }
  
}

for (let i$3 = 1; i$3 <= 10; ++i$3) {
  if (Belt_MutableQueue.popExn(q2) !== i$3) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          92,
          4
        ]
      }
    });
  }
  
}

let q$4 = {
  length: 0,
  first: undefined,
  last: undefined
};

if (q$4.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        98,
        2
      ]
    }
  });
}

for (let i$4 = 1; i$4 <= 10; ++i$4) {
  Belt_MutableQueue.add(q$4, i$4);
  if (q$4.length !== i$4) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          101,
          4
        ]
      }
    });
  }
  if (q$4.length === 0) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          102,
          4
        ]
      }
    });
  }
  
}

for (let i$5 = 10; i$5 >= 1; --i$5) {
  if (q$4.length !== i$5) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          105,
          4
        ]
      }
    });
  }
  if (q$4.length === 0) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          106,
          4
        ]
      }
    });
  }
  Belt_MutableQueue.popExn(q$4);
}

if (q$4.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        109,
        2
      ]
    }
  });
}

if (q$4.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        110,
        2
      ]
    }
  });
}

let q$5 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i$6 = 1; i$6 <= 10; ++i$6) {
  Belt_MutableQueue.add(q$5, i$6);
}

let i$7 = {
  contents: 1
};

Belt_MutableQueue.forEach(q$5, ((j) => {
  if (i$7.contents !== j) {
    throw new Error("Assert_failure", {
      cause: {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "bs_queue_test.res",
          120,
          4
        ]
      }
    });
  }
  i$7.contents = i$7.contents + 1 | 0;
}));

let q1$1 = {
  length: 0,
  first: undefined,
  last: undefined
};

let q2$1 = {
  length: 0,
  first: undefined,
  last: undefined
};

if (q1$1.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        127,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$1), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        128,
        2
      ]
    }
  });
}

if (q2$1.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        129,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$1), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        130,
        2
      ]
    }
  });
}

Belt_MutableQueue.transfer(q1$1, q2$1);

if (q1$1.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        132,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$1), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        133,
        2
      ]
    }
  });
}

if (q2$1.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        134,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$1), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        135,
        2
      ]
    }
  });
}

let q1$2 = {
  length: 0,
  first: undefined,
  last: undefined
};

let q2$2 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i$8 = 1; i$8 <= 4; ++i$8) {
  Belt_MutableQueue.add(q1$2, i$8);
}

if (q1$2.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        143,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$2), [
    1,
    2,
    3,
    4
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        144,
        2
      ]
    }
  });
}

if (q2$2.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        145,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$2), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        146,
        2
      ]
    }
  });
}

Belt_MutableQueue.transfer(q1$2, q2$2);

if (q1$2.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        148,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$2), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        149,
        2
      ]
    }
  });
}

if (q2$2.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        150,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$2), [
    1,
    2,
    3,
    4
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        151,
        2
      ]
    }
  });
}

let q1$3 = {
  length: 0,
  first: undefined,
  last: undefined
};

let q2$3 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i$9 = 5; i$9 <= 8; ++i$9) {
  Belt_MutableQueue.add(q2$3, i$9);
}

if (q1$3.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        159,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$3), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        160,
        2
      ]
    }
  });
}

if (q2$3.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        161,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$3), [
    5,
    6,
    7,
    8
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        162,
        2
      ]
    }
  });
}

Belt_MutableQueue.transfer(q1$3, q2$3);

if (q1$3.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        164,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$3), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        165,
        2
      ]
    }
  });
}

if (q2$3.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        166,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$3), [
    5,
    6,
    7,
    8
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        167,
        2
      ]
    }
  });
}

let q1$4 = {
  length: 0,
  first: undefined,
  last: undefined
};

let q2$4 = {
  length: 0,
  first: undefined,
  last: undefined
};

for (let i$10 = 1; i$10 <= 4; ++i$10) {
  Belt_MutableQueue.add(q1$4, i$10);
}

for (let i$11 = 5; i$11 <= 8; ++i$11) {
  Belt_MutableQueue.add(q2$4, i$11);
}

if (q1$4.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        178,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$4), [
    1,
    2,
    3,
    4
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        179,
        2
      ]
    }
  });
}

if (q2$4.length !== 4) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        180,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$4), [
    5,
    6,
    7,
    8
  ])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        181,
        2
      ]
    }
  });
}

Belt_MutableQueue.transfer(q1$4, q2$4);

if (q1$4.length !== 0) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        183,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q1$4), [])) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        184,
        2
      ]
    }
  });
}

let v = [
  5,
  6,
  7,
  8,
  1,
  2,
  3,
  4
];

if (q2$4.length !== 8) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        186,
        2
      ]
    }
  });
}

if (!Caml_obj.equal(Belt_MutableQueue.toArray(q2$4), v)) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        187,
        2
      ]
    }
  });
}

if (Belt_MutableQueue.reduce(q2$4, 0, ((x, y) => {
    return x - y | 0;
  })) !== Belt_Array.reduce(v, 0, ((x, y) => {
    return x - y | 0;
  }))) {
  throw new Error("Assert_failure", {
    cause: {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "bs_queue_test.res",
        189,
        2
      ]
    }
  });
}

console.log("OK");

let q$6 = Belt_MutableQueue.fromArray([
  1,
  2,
  3,
  4
]);

let q1$5 = Belt_MutableQueue.map(q$6, ((x) => {
  return x - 1 | 0;
}));

eq("File \"bs_queue_test.res\", line 197, characters 5-12", Belt_MutableQueue.toArray(q1$5), [
  0,
  1,
  2,
  3
]);

let q$7 = Belt_MutableQueue.fromArray([]);

b("File \"bs_queue_test.res\", line 198, characters 4-11", q$7.length === 0);

let q$8 = Belt_MutableQueue.map(Belt_MutableQueue.fromArray([]), ((x) => {
  return x + 1 | 0;
}));

b("File \"bs_queue_test.res\", line 199, characters 4-11", q$8.length === 0);

Mt.from_pair_suites("Bs_queue_test", suites.contents);

let Q;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.Q = Q;
exports.does_raise = does_raise;
exports.$plus$plus = $plus$plus;
/*  Not a pure module */
