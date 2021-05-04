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
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

var v5 = {
  x: 3,
  y: 3,
  setY: (function (v) {
      var self = this ;
      self.y = 2;
      return [
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

var test_type_1 = {
  hd: v,
  tl: /* [] */0
};

var test_type = {
  hd: u,
  tl: test_type_1
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

var test_type2_1 = {
  hd: zz,
  tl: /* [] */0
};

var test_type2 = {
  hd: z,
  tl: test_type2_1
};

eq("File \"ppx_this_obj_field.ml\", line 92, characters 5-12", [
      6,
      v5.say()
    ]);

var a = v.say();

v.incr();

var b = v.say();

v.incr();

var c = v.say();

v.incr();

eq("File \"ppx_this_obj_field.ml\", line 99, characters 5-12", [
      [
        3,
        4,
        5
      ],
      [
        a,
        b,
        c
      ]
    ]);

var aa = z.getX();

z.setX(32);

var bb = z.getX();

eq("File \"ppx_this_obj_field.ml\", line 103, characters 5-12", [
      [
        3,
        32
      ],
      [
        aa,
        bb
      ]
    ]);

var f = {
  x: 3,
  hei: (function () {
      var y = this ;
      y.x = y.x + 3 | 0;
      
    })
};

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
exports.f = f;
/* v5 Not a pure module */
