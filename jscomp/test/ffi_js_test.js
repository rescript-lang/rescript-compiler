'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var keys = ( function (x){return Object.keys(x)});


  function $$higher_order(x){
   return function(y,z){
      return x + y + z 
   }
  }

;

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
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

var int_config = {
  hi: 3,
  low: 32
};

var string_config = {
  hi: 3,
  low: "32"
};

eq("File \"ffi_js_test.ml\", line 32, characters 5-12", /* tuple */[
      6,
      $$higher_order(1)(2, 3)
    ]);

var same_type_000 = /* :: */[
  int_config,
  /* :: */[
    {
      hi: 3,
      low: 32
    },
    /* [] */0
  ]
];

var same_type_001 = /* :: */[
  string_config,
  /* :: */[
    {
      hi: 3,
      low: "32"
    },
    /* [] */0
  ]
];

var same_type = /* tuple */[
  same_type_000,
  same_type_001
];

var v_obj = {
  hi: (function () {
      console.log("hei");
      return /* () */0;
    })
};

eq("File \"ffi_js_test.ml\", line 44, characters 5-12", /* tuple */[
      Object.keys(int_config).length,
      2
    ]);

eq("File \"ffi_js_test.ml\", line 45, characters 5-12", /* tuple */[
      Object.keys(string_config).length,
      2
    ]);

eq("File \"ffi_js_test.ml\", line 46, characters 5-12", /* tuple */[
      Object.keys(v_obj).indexOf("hi_x"),
      -1
    ]);

eq("File \"ffi_js_test.ml\", line 47, characters 5-12", /* tuple */[
      Object.keys(v_obj).indexOf("hi"),
      0
    ]);

var u = [3];

var side_effect_config = (u[0] = u[0] + 1 | 0, {
    hi: 3,
    low: 32
  });

eq("File \"ffi_js_test.ml\", line 54, characters 5-12", /* tuple */[
      u[0],
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
  x.setGADT2 = /* tuple */[
    3,
    "3"
  ];
  x.setGADT2 = /* tuple */[
    "3",
    3
  ];
  var match = x[3];
  console.log(/* tuple */[
        match[0],
        match[1]
      ]);
  console.log(x.getGADT);
  var match$1 = x.getGADT2;
  console.log(match$1[0], match$1[1]);
  var match$2 = x[0];
  console.log(match$2[0], match$2[1]);
  x[0] = /* tuple */[
    1,
    "x"
  ];
  x[3] = /* tuple */[
    3,
    "x"
  ];
  return /* () */0;
}

Mt.from_pair_suites("ffi_js_test.ml", suites[0]);

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
/* keys Not a pure module */
