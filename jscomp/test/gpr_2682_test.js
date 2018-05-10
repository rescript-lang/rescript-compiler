'use strict';


var sum = function (a,b){ 
  return a + b
};

var v = sum(1, 2);

function f(a) {
  return a + (3) | 0;
}

var b = f(1);

var c = f(2);

var forIn = function (o,foo){
  for (var i in o){
    foo(o)
  }
  };

function log(x) {
  console.log(x);
  return /* () */0;
}

var N = /* module */[/* log2 */log];

forIn({
      x: 3
    }, (function (x) {
        console.log(x);
        return /* () */0;
      }));

forIn({
      x: 3,
      y: 3
    }, (function (x) {
        console.log(x);
        return /* () */0;
      }));

var f3 = function (){return true};

var bbbb = f3();

exports.sum = sum;
exports.v = v;
exports.f = f;
exports.b = b;
exports.c = c;
exports.forIn = forIn;
exports.N = N;
exports.f3 = f3;
exports.bbbb = bbbb;
/* v Not a pure module */
