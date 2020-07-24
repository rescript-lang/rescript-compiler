'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, b$1) {
  return Mt.bool_suites(test_id, suites, loc, b$1);
}

function makeWrapper(foo, param) {
  var tmp = {};
  if (foo !== undefined) {
    tmp.foo = Caml_option.valFromOption(foo);
  }
  console.log(tmp);
  
}

function makeWrapper2(foo, param) {
  console.log({
        foo: foo
      });
  
}

makeWrapper2("a", undefined);

function makeWrapper3(foo, param) {
  console.log(2);
  var tmp = {};
  if (foo !== undefined) {
    tmp.foo = Caml_option.valFromOption(foo);
  }
  return tmp;
}

function makeWrapper4(foo, param) {
  console.log(2);
  var tmp = {};
  var tmp$1 = foo > 100 ? undefined : (
      foo > 10 ? "b" : "a"
    );
  if (tmp$1 !== undefined) {
    tmp.foo = Caml_option.valFromOption(tmp$1);
  }
  return tmp;
}

b("File \"gpr_2503_test.ml\", line 31, characters 5-12", "a" === makeWrapper3("a", undefined).foo);

b("File \"gpr_2503_test.ml\", line 34, characters 5-12", undefined === makeWrapper3(undefined, undefined).foo);

b("File \"gpr_2503_test.ml\", line 37, characters 5-12", "a" === makeWrapper4(1, undefined).foo);

b("File \"gpr_2503_test.ml\", line 40, characters 5-12", "b" === makeWrapper4(11, undefined).foo);

b("File \"gpr_2503_test.ml\", line 43, characters 5-12", undefined === makeWrapper4(111, undefined).foo);

Mt.from_pair_suites("Gpr_2503_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.makeWrapper = makeWrapper;
exports.makeWrapper2 = makeWrapper2;
exports.makeWrapper3 = makeWrapper3;
exports.makeWrapper4 = makeWrapper4;
/*  Not a pure module */
