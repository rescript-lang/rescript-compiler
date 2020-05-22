'use strict';

var Curry = require("../../lib/js/curry.js");

function on1(foo, $$event) {
  foo.on((function () {
            switch ($$event.HASH) {
              case 4895187 :
                  return "bar";
              case 5097222 :
                  return "foo";
              
            }
          })(), $$event.value);
  
}

function on2(foo, h, $$event) {
  foo.on((function () {
            switch (Curry._1(h, $$event).HASH) {
              case 4895187 :
                  return "bar";
              case 5097222 :
                  return "foo";
              
            }
          })(), Curry._1(h, $$event).value);
  
}

exports.on1 = on1;
exports.on2 = on2;
/* No side effect */
