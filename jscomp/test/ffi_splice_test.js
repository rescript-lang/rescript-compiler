'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}


function Make (){
  this.data = []
  for(var i = 0; i < arguments.length; ++i){
   this.data[i] = arguments[i]
}
}

Make.prototype.sum = function(){
  var result  = 0;
  for(var k = 0; k < this.data.length; ++k){
    result = result + this.data[k]
  };
  return result
}  

Make.prototype.add = function(){
  
} 

;

function f(x) {
  return x.test("a", "b").test("a", "b");
}

var v = new Make(1, 2, 3, 4);

var u = v.sum();

eq("File \"ffi_splice_test.ml\", line 57, characters 12-19", u, 10);

Mt.from_pair_suites("ffi_splice_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.v = v;
exports.u = u;
/*  Not a pure module */
