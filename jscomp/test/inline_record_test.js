// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let List = require("../../lib/js/list.js");
let Caml_exceptions = require("../../lib/js/caml_exceptions.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

let v = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
};

let v1 = {
  TAG: "A1",
  more: {
    hd: 1,
    tl: {
      hd: 2,
      tl: /* [] */0
    }
  }
};

function f(x) {
  if (x.TAG === "A0") {
    return List.fold_left((prim0, prim1) => prim0 + prim1 | 0, x.lbl, x.more);
  } else {
    return List.fold_left((prim0, prim1) => prim0 + prim1 | 0, 0, x.more);
  }
}

eq("File \"inline_record_test.res\", line 19, characters 3-10", f(v), 3);

eq("File \"inline_record_test.res\", line 20, characters 3-10", f(v1), 3);

console.log(f(v));

console.log(f(v1));

let A0 = /* @__PURE__ */Caml_exceptions.create("Inline_record_test.A0");

let v3 = {
  RE_EXN_ID: A0,
  lbl: 3,
  more: /* [] */0
};

let tmp;

if (A0 === A0) {
  tmp = 3;
} else {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "inline_record_test.res",
      47,
      9
    ],
    Error: new Error()
  };
}

eq("File \"inline_record_test.res\", line 44, characters 2-9", tmp, 3);

function ff(x) {
  if (x.TAG === "A0") {
    x.x = x.x + 1 | 0;
  } else {
    x.z = x.z + 2 | 0;
  }
}

let v4 = {
  TAG: "A0",
  x: 0,
  y: 0,
  z: 0
};

let v5 = {
  TAG: "A1",
  z: 0
};

for (let i = 0; i <= 10; ++i) {
  ff(v4);
  ff(v5);
}

let tmp$1;

if (v4.TAG === "A0") {
  tmp$1 = v4.x;
} else {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "inline_record_test.res",
      74,
      9
    ],
    Error: new Error()
  };
}

eq("File \"inline_record_test.res\", line 71, characters 2-9", tmp$1, 11);

let tmp$2;

if (v5.TAG === "A0") {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "inline_record_test.res",
      83,
      9
    ],
    Error: new Error()
  };
}

tmp$2 = v5.z;

eq("File \"inline_record_test.res\", line 80, characters 2-9", tmp$2, 22);

let A4 = /* @__PURE__ */Caml_exceptions.create("Inline_record_test.A4");

let v6 = {
  RE_EXN_ID: A4,
  x: 0,
  y: 0,
  z: 0
};

function ff0(x) {
  if (x.RE_EXN_ID === A4) {
    x.x = x.x + 1 | 0;
    x.z = x.z + 1 | 0;
    return;
  }
  
}

for (let i$1 = 0; i$1 <= 10; ++i$1) {
  ff0(v6);
}

let tmp$3;

if (v6.RE_EXN_ID === A4) {
  tmp$3 = v6.x;
} else {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "inline_record_test.res",
      108,
      9
    ],
    Error: new Error()
  };
}

eq("File \"inline_record_test.res\", line 105, characters 2-9", tmp$3, 11);

function ff1(x) {
  if (typeof x !== "object") {
    return "A1";
  } else {
    return {
      TAG: "A0",
      lbl: x.lbl + 1 | 0,
      more: x.more
    };
  }
}

Mt.from_pair_suites("Inline_record_test", suites.contents);

let b = {
  TAG: "B"
};

if (typeof b !== "object") {
  console.log("A!");
} else {
  console.log("B");
}

console.log("10!");

let v2 = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
};

let vvv = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
};

let r = {
  y: 10
};

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.v1 = v1;
exports.f = f;
exports.v2 = v2;
exports.A0 = A0;
exports.v3 = v3;
exports.vvv = vvv;
exports.ff = ff;
exports.v4 = v4;
exports.v5 = v5;
exports.A4 = A4;
exports.v6 = v6;
exports.ff0 = ff0;
exports.ff1 = ff1;
exports.b = b;
exports.r = r;
/*  Not a pure module */
