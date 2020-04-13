'use strict';

var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");

var N = { };

function f(X, xs) {
  return X.forEach(xs, {
              i: (function (x) {
                  console.log(x.x);
                  
                })
            });
}

Belt_List.forEachU(/* :: */[
      {
        x: 3
      },
      /* [] */0
    ], (function (x) {
        console.log(x.x);
        
      }));

var Foo = { };

var bar = [{
    foo: "bar"
  }];

Belt_Array.mapU(bar, (function (b) {
        return b.foo;
      }));

exports.N = N;
exports.f = f;
exports.Foo = Foo;
exports.bar = bar;
/*  Not a pure module */
