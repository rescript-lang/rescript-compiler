'use strict';

var Mt = require("./mt.js");

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
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
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
      
    }),
  incr: (function () {
      var self = this ;
      self.y = self.y + 1 | 0;
      
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
      
    }),
  getY: (function () {
      return 3;
    }),
  say: (function () {
      return 7;
    })
};

var test_type_1 = /* :: */{
  _0: v,
  _1: /* [] */0
};

var test_type = /* :: */{
  _0: u,
  _1: test_type_1
};

var z = {
  x: {
    contents: 3
  },
  setX: (function (x) {
      var self = this ;
      self.x.contents = x;
      
    }),
  getX: (function () {
      var self = this ;
      return self.x.contents;
    })
};

var eventObj = {
  events: [],
  empty: (function () {
      var self = this ;
      var a = self.events;
      a.splice(0);
      
    }),
  push: (function (a) {
      var self = this ;
      var xs = self.events;
      xs.push(a);
      
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
      
    }),
  getX: (function () {
      var self = this ;
      return self.x;
    })
};

var test_type2_1 = /* :: */{
  _0: zz,
  _1: /* [] */0
};

var test_type2 = /* :: */{
  _0: z,
  _1: test_type2_1
};

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
