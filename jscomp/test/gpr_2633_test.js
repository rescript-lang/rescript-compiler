'use strict';

var Curry = require("../../lib/js/curry.js");

function on1(foo, $$event) {
  foo.on((function () {
            switch ($$event[0]) {
              case 4895187 : 
                  return "bar";
              case 5097222 : 
                  return "foo";
              
            }
          })(), $$event[1]);
  return /* () */0;
}

function on2(foo, h, $$event) {
  foo.on((function () {
            switch (Curry._1(h, $$event)[0]) {
              case 4895187 : 
                  return "bar";
              case 5097222 : 
                  return "foo";
              
            }
          })(), Curry._1(h, $$event)[1]);
  return /* () */0;
}

exports.on1 = on1;
exports.on2 = on2;
/* No side effect */
