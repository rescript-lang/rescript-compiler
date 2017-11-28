'use strict';

var Immutable = require('immutable');
var Map = Immutable.Map;
var m = new Map();
function should(b){
    if (!b){
        throw new Error("impossible")
    }
}
function test() {
  var count = 1000000;
  for(var i = 0; i < count; ++i) {
    m = m.set(i, i);
  }
  for(var j = 0; j < count; ++j) {
    should(m.get(j) !== undefined);
  }
}

test();