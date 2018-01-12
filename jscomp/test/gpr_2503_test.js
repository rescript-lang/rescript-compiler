'use strict';

var Mt = require("./mt.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, b$1) {
  return Mt.bool_suites(test_id, suites, loc, b$1);
}

function makeWrapper(foo, _) {
  var tmp = { };
  if (foo) {
    tmp.foo = (function () {
          switch (foo[0]) {
            case 97 : 
                return "a";
            case 98 : 
                return "b";
            
          }
        })();
  }
  console.log(tmp);
  return /* () */0;
}

function makeWrapper2(foo, _) {
  console.log({
        foo: (function () {
              switch (foo) {
                case 97 : 
                    return "a";
                case 98 : 
                    return "b";
                
              }
            })()
      });
  return /* () */0;
}

makeWrapper2(/* a */97, /* () */0);

function makeWrapper3(foo, _) {
  console.log(2);
  var tmp = { };
  if (foo) {
    tmp.foo = (function () {
          switch (foo[0]) {
            case 97 : 
                return "a";
            case 98 : 
                return "b";
            
          }
        })();
  }
  return tmp;
}

function makeWrapper4(foo, _) {
  console.log(2);
  var tmp = { };
  var tmp$1 = foo > 100 ? /* None */0 : (
      foo > 10 ? /* Some */[/* b */98] : /* Some */[/* a */97]
    );
  if (tmp$1) {
    tmp.foo = (function () {
          switch (tmp$1[0]) {
            case 97 : 
                return "a";
            case 98 : 
                return "b";
            
          }
        })();
  }
  return tmp;
}

b("File \"gpr_2503_test.ml\", line 31, characters 5-12", "a" === makeWrapper3(/* Some */[/* a */97], /* () */0).foo);

b("File \"gpr_2503_test.ml\", line 34, characters 5-12", undefined === makeWrapper3(/* None */0, /* () */0).foo);

b("File \"gpr_2503_test.ml\", line 37, characters 5-12", "a" === makeWrapper4(1, /* () */0).foo);

b("File \"gpr_2503_test.ml\", line 40, characters 5-12", "b" === makeWrapper4(11, /* () */0).foo);

b("File \"gpr_2503_test.ml\", line 43, characters 5-12", undefined === makeWrapper4(111, /* () */0).foo);

Mt.from_pair_suites("gpr_2503_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.makeWrapper = makeWrapper;
exports.makeWrapper2 = makeWrapper2;
exports.makeWrapper3 = makeWrapper3;
exports.makeWrapper4 = makeWrapper4;
/*  Not a pure module */
