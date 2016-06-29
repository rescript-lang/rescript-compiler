// GENERATED CODE BY BUCKLESCRIPT VERSION 0.6.2 , PLEASE EDIT WITH CARE
'use strict';


var u = {
  x: 3,
  y: 32,
  bark: function ($$this, _, _$1) {
    console.log(/* tuple */[
          $$this.length,
          $$this.x,
          $$this.y
        ]);
    return /* () */0;
  },
  length: 32
};

u.bark(u, 1, 2);

var js_obj = {
  x: 3,
  y: 32,
  bark: function (x, y) {
    var o = this ;
    console.log(/* tuple */[
          o.length,
          o.x,
          o.y,
          x,
          y
        ]);
    return x + y | 0;
  },
  length: 32
};

js_obj.bark(1, 2);

js_obj.bark(1, 2);

exports.js_obj = js_obj;
/* u Not a pure module */
