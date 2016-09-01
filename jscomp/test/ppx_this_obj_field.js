'use strict';


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

console.log(v5.say());

exports.v5 = v5;
/* v5 Not a pure module */
