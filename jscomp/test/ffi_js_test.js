'use strict';

var Mt = require("./mt.js");

var keys = (function (x){return Object.keys(x)});

function $$higher_order(x){
   return function(y,z){
      return x + y + z
   }
  }
;

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

var int_config = {
  hi: 3,
  low: 32
};

var string_config = {
  hi: 3,
  low: "32"
};

eq("File \"ffi_js_test.ml\", line 32, characters 5-12", [
      6,
      $$higher_order(1)(2, 3)
    ]);

var same_type_0 = /* :: */{
  _0: int_config,
  _1: /* :: */{
    _0: {
      hi: 3,
      low: 32
    },
    _1: /* [] */0
  }
};

var same_type_1 = /* :: */{
  _0: string_config,
  _1: /* :: */{
    _0: {
      hi: 3,
      low: "32"
    },
    _1: /* [] */0
  }
};

var same_type = [
  same_type_0,
  same_type_1
];

var v_obj = {
  hi: (function () {
      console.log("hei");
      
    })
};

eq("File \"ffi_js_test.ml\", line 44, characters 5-12", [
      Object.keys(int_config).length,
      2
    ]);

eq("File \"ffi_js_test.ml\", line 45, characters 5-12", [
      Object.keys(string_config).length,
      2
    ]);

eq("File \"ffi_js_test.ml\", line 46, characters 5-12", [
      Object.keys(v_obj).indexOf("hi_x"),
      -1
    ]);

eq("File \"ffi_js_test.ml\", line 47, characters 5-12", [
      Object.keys(v_obj).indexOf("hi"),
      0
    ]);

var u = {
  contents: 3
};

var side_effect_config = (u.contents = u.contents + 1 | 0, {
    hi: 3,
    low: 32
  });

eq("File \"ffi_js_test.ml\", line 54, characters 5-12", [
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

function create_prim(param) {
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
  var match = x[3];
  console.log([
        match[0],
        match[1]
      ]);
  console.log(x.getGADT);
  var match$1 = x.getGADT2;
  console.log(match$1[0], match$1[1]);
  var match$2 = x[0];
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
exports.v_obj = v_obj;
exports.u = u;
exports.side_effect_config = side_effect_config;
exports.vv = vv;
exports.v = v;
exports.vvv = vvv;
exports.vvvv = vvvv;
exports.create_prim = create_prim;
exports.ffff = ffff;
/*  Not a pure module */
