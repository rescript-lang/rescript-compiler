'use strict';

var SysBluebird = require("sys-bluebird");

function f(p) {
  return p.catch(3);
}

var p = new SysBluebird.Promise();

p.then((function (x) {
          return x + 3 | 0;
        })).catch((function (reason) {
        return reason;
      }));

var u = {
  then: 3,
  catch: 32
};

var uu = {
  "'x": 3
};

var hh = uu["'x"];

exports.f = f;
exports.u = u;
exports.uu = uu;
exports.hh = hh;
/* p Not a pure module */
