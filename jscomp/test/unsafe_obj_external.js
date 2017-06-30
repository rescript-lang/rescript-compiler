'use strict';


var v = {
  x: (function () {
      return 3;
    }),
  say: (function (x) {
      var self = this ;
      return self.x() + x | 0;
    })
};

var u = v.x() + v.say(3) | 0;

exports.v = v;
exports.u = u;
/* v Not a pure module */
