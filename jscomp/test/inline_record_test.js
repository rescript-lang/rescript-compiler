'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
};

var v1 = {
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
    return List.fold_left((function (prim0, prim1) {
                  return prim0 + prim1 | 0;
                }), x.lbl, x.more);
  } else {
    return List.fold_left((function (prim0, prim1) {
                  return prim0 + prim1 | 0;
                }), 0, x.more);
  }
}

eq("File \"inline_record_test.res\", line 19, characters 3-10", f(v), 3);

eq("File \"inline_record_test.res\", line 20, characters 3-10", f(v1), 3);

console.log(f(v));

console.log(f(v1));

var A0 = /* @__PURE__ */Caml_exceptions.create("Inline_record_test.A0");

var v3 = {
  RE_EXN_ID: A0,
  lbl: 3,
  more: /* [] */0
};

var tmp;

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

var v4 = {
  TAG: "A0",
  x: 0,
  y: 0,
  z: 0
};

var v5 = {
  TAG: "A1",
  z: 0
};

for(var i = 0; i <= 10; ++i){
  ff(v4);
  ff(v5);
}

var tmp$1;

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

var tmp$2;

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

var A4 = /* @__PURE__ */Caml_exceptions.create("Inline_record_test.A4");

var v6 = {
  RE_EXN_ID: A4,
  x: 0,
  y: 0,
  z: 0
};

function ff0(x) {
  if (x.RE_EXN_ID === A4) {
    x.x = x.x + 1 | 0;
    x.z = x.z + 1 | 0;
    return ;
  }
  
}

for(var i$1 = 0; i$1 <= 10; ++i$1){
  ff0(v6);
}

var tmp$3;

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

var v2 = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
};

var vvv = {
  TAG: "A0",
  lbl: 3,
  more: /* [] */0
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
/*  Not a pure module */
