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
  var m = Bs_MapInt.empty;
  for(var i = 0; i <= 999999; ++i){
    m = Bs_MapInt.set(m, i, i);
  }
  for(var i$1 = 0; i$1 <= 999999; ++i$1){
    should(+(Bs_MapInt.get(m, i$1) !== /* None */0));
  }
  for(var i$2 = 0; i$2 <= 999999; ++i$2){
    m = Bs_MapInt.remove(m, i$2);
  }
  return should(Bs_MapInt.isEmpty(m));
}

test(/* () */0);

var M = 0;

exports.should = should;
exports.M = M;
exports.test = test;
/*  Not a pure module */
