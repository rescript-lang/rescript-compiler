'use strict';

var Bs_MapInt = require("../../lib/js/bs_MapInt.js");

function should(b) {
  if (b) {
    return 0;
  } else {
    throw new Error("IMPOSSIBLE");
  }
}

function test() {
  var m = /* Empty */0;
  for(var i = 0; i <= 999999; ++i){
    m = Bs_MapInt.add(i, i, m);
  }
  for(var i$1 = 0; i$1 <= 999999; ++i$1){
    should(+(Bs_MapInt.find(i$1, m) !== /* None */0));
  }
  return /* () */0;
}

test(/* () */0);

exports.should = should;
exports.test   = test;
/*  Not a pure module */
