'use strict';

var Mt    = require("./mt");
var Block = require("../../lib/js/block");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var v5 = {
  x: 3,
  y: 3,
  setY: function (v) {
    var self = this ;
    self.y = 2;
    return /* tuple */[
            self.y,
            v
          ];
  },
  say: function () {
    var self = this ;
    return self.x + self.y | 0;
  },
  hihi: function () {
    var self = this ;
    return self.x + self.say() | 0;
  },
  bark: function () {
    console.log("bark");
    return /* () */0;
  },
  xz: function () {
    return 3;
  }
};

var v = {
  x: 3,
  y: 0,
  reset: function () {
    var self = this ;
    return self.y = 0;
  },
  incr: function () {
    var self = this ;
    return self.y = self.y + 1 | 0;
  },
  getY: function () {
    var self = this ;
    return self.y;
  },
  say: function () {
    var self = this ;
    return self.x + self.y | 0;
  }
};

var u = {
  incr: function () {
    console.log("hey");
    return /* () */0;
  },
  getY: function () {
    return 3;
  },
  say: function () {
    return 7;
  }
};

var test_type_001 = /* :: */[
  v,
  /* [] */0
];

var test_type = /* :: */[
  u,
  test_type_001
];

eq('File "ppx_this_obj_field.ml", line 60, characters 5-12', /* tuple */[
      6,
      v5.say()
    ]);

var a = v.say();

v.incr();

var b = v.say();

v.incr();

var c = v.say();

v.incr();

eq('File "ppx_this_obj_field.ml", line 67, characters 5-12', /* tuple */[
      /* tuple */[
        3,
        4,
        5
      ],
      /* tuple */[
        a,
        b,
        c
      ]
    ]);

Mt.from_pair_suites("ppx_this_obj_field.ml", suites[0]);

exports.suites    = suites;
exports.test_id   = test_id;
exports.eq        = eq;
exports.v5        = v5;
exports.v         = v;
exports.u         = u;
exports.test_type = test_type;
/* v5 Not a pure module */
