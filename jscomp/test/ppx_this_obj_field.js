'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

var v5 = {
  x: 3,
  y: 3,
  setY: (function (v) {
      var self = this ;
      self.y = 2;
      return /* tuple */[
              self.y,
              v
            ];
    }),
  say: (function () {
      var self = this ;
      return self.x + self.y | 0;
    }),
  hihi: (function (u) {
      var self = this ;
      return self.x + self.say() | 0;
    }),
  bark: (function () {
      console.log("bark");
      return /* () */0;
    }),
  xz: (function () {
      return 3;
    })
};

var v = {
  x: 3,
  y: 0,
  reset: (function () {
      var self = this ;
      self.y = 0;
      return /* () */0;
    }),
  incr: (function () {
      var self = this ;
      self.y = self.y + 1 | 0;
      return /* () */0;
    }),
  getY: (function () {
      var self = this ;
      return self.y;
    }),
  say: (function () {
      var self = this ;
      return self.x + self.y | 0;
    })
};

var u = {
  incr: (function () {
      console.log("hey");
      return /* () */0;
    }),
  getY: (function () {
      return 3;
    }),
  say: (function () {
      return 7;
    })
};

var test_type_001 = /* :: */[
  v,
  /* [] */0
];

var test_type = /* :: */[
  u,
  test_type_001
];

var z = {
  x: /* record */{
    contents: 3
  },
  setX: (function (x) {
      var self = this ;
      self.x.contents = x;
      return /* () */0;
    }),
  getX: (function () {
      var self = this ;
      return self.x.contents;
    })
};

var eventObj = {
  events: /* array */[],
  empty: (function () {
      var self = this ;
      var a = self.events;
      a.splice(0);
      return /* () */0;
    }),
  push: (function (a) {
      var self = this ;
      var xs = self.events;
      xs.push(a);
      return /* () */0;
    }),
  needRebuild: (function () {
      var self = this ;
      return self.events.length !== 0;
    })
};

function test__(x) {
  return eventObj.push(x);
}

var zz = {
  x: 3,
  setX: (function (x) {
      var self = this ;
      self.x = x;
      return /* () */0;
    }),
  getX: (function () {
      var self = this ;
      return self.x;
    })
};

var test_type2_001 = /* :: */[
  zz,
  /* [] */0
];

var test_type2 = /* :: */[
  z,
  test_type2_001
];

eq("File \"ppx_this_obj_field.ml\", line 92, characters 5-12", /* tuple */[
      6,
      v5.say()
    ]);

var a = v.say();

v.incr();

var b = v.say();

v.incr();

var c = v.say();

v.incr();

eq("File \"ppx_this_obj_field.ml\", line 99, characters 5-12", /* tuple */[
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

var aa = z.getX();

z.setX(32);

var bb = z.getX();

eq("File \"ppx_this_obj_field.ml\", line 103, characters 5-12", /* tuple */[
      /* tuple */[
        3,
        32
      ],
      /* tuple */[
        aa,
        bb
      ]
    ]);

Mt.from_pair_suites("Ppx_this_obj_field", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v5 = v5;
exports.v = v;
exports.u = u;
exports.test_type = test_type;
exports.z = z;
exports.eventObj = eventObj;
exports.test__ = test__;
exports.zz = zz;
exports.test_type2 = test_type2;
/* v5 Not a pure module */
