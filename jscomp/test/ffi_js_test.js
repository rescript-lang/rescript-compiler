// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let keys = (function (x){return Object.keys(x)});

function $$higher_order(x){
   return function(y,z){
      return x + y + z
   }
  }
;

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, param) {
  let y = param[1];
  let x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (() => {
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

let int_config = {
  hi: 3,
  low: 32
};

let string_config = {
  hi: 3,
  low: "32"
};

eq("File \"ffi_js_test.res\", line 30, characters 12-19", [
  6,
  $$higher_order(1)(2, 3)
]);

let same_type_0 = {
  hd: int_config,
  tl: {
    hd: {
      hi: 3,
      low: 32
    },
    tl: /* [] */0
  }
};

let same_type_1 = {
  hd: string_config,
  tl: {
    hd: {
      hi: 3,
      low: "32"
    },
    tl: /* [] */0
  }
};

let same_type = [
  same_type_0,
  same_type_1
];

eq("File \"ffi_js_test.res\", line 38, characters 5-12", [
  Object.keys(int_config).length,
  2
]);

eq("File \"ffi_js_test.res\", line 39, characters 5-12", [
  Object.keys(string_config).length,
  2
]);

let u = {
  contents: 3
};

let side_effect_config = (u.contents = u.contents + 1 | 0, "Int", {
  hi: 3,
  low: 32
});

eq("File \"ffi_js_test.res\", line 53, characters 12-19", [
  u.contents,
  4
]);

function vv(z) {
  return z.hh();
}

function v(z) {
  return z.ff();
}

function vvv(z) {
  return z.ff_pipe();
}

function vvvv(z) {
  return z.ff_pipe2();
}

function create_prim() {
  return {
    "x'": 3,
    "x''": 3,
    "x''''": 2
  };
}

function ffff(x) {
  x.setGADT = 3;
  x.setGADT2 = [
    3,
    "3"
  ];
  x.setGADT2 = [
    "3",
    3
  ];
  let match = x[3];
  console.log([
    match[0],
    match[1]
  ]);
  console.log(x.getGADT);
  let match$1 = x.getGADT2;
  console.log(match$1[0], match$1[1]);
  let match$2 = x[0];
  console.log(match$2[0], match$2[1]);
  x[0] = [
    1,
    "x"
  ];
  x[3] = [
    3,
    "x"
  ];
}

Mt.from_pair_suites("Ffi_js_test", suites.contents);

exports.keys = keys;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.int_config = int_config;
exports.string_config = string_config;
exports.same_type = same_type;
exports.u = u;
exports.side_effect_config = side_effect_config;
exports.vv = vv;
exports.v = v;
exports.vvv = vvv;
exports.vvvv = vvvv;
exports.create_prim = create_prim;
exports.ffff = ffff;
/*  Not a pure module */
