'use strict';

var Curry = require("../../lib/js/curry.js");

function on1(foo, $$event) {
  foo.on((function () {
            switch ($$event.HASH) {
              case "bar" :
                  return "bar";
              case "foo" :
                  return "foo";
              
            }
          })(), $$event.VAL);
  
}

function on2(foo, h, $$event) {
  foo.on((function () {
            switch (Curry._1(h, $$event).HASH) {
              case "bar" :
                  return "bar";
              case "foo" :
                  return "foo";
              
            }
          })(), Curry._1(h, $$event).VAL);
  
}

exports.on1 = on1;
exports.on2 = on2;
/* No side effect */
