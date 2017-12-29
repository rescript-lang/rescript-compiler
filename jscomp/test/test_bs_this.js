'use strict';


function uux_this(x, y) {
  var o = this ;
  return (o.length + x | 0) + y | 0;
}

function even(x) {
  var o = this ;
  return x + o | 0;
}

function bark() {
  return (function (x, y) {
      var o = this ;
      console.log(/* tuple */[
            o.length,
            o.x,
            o.y,
            x,
            y
          ]);
      return x + y | 0;
    });
}

var js_obj = {
  bark: (function (x, y) {
      var o = this ;
      console.log(o);
      return x + y | 0;
    })
};

function f(x) {
  x.onload = (function () {
      var o = this ;
      console.log(o);
      return /* () */0;
    });
  return x.addEventListener("onload", (function () {
                var o = this ;
                console.log(o.response);
                return /* () */0;
              }));
}

function u(x) {
  return x;
}

exports.uux_this = uux_this;
exports.even = even;
exports.bark = bark;
exports.js_obj = js_obj;
exports.f = f;
exports.u = u;
/* uux_this Not a pure module */
