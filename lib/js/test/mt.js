// GENERATED CODE BY BUCKLESCRIPT VERSION 0.8.1 , PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../caml_builtin_exceptions");
var Curry                   = require("../curry");
var Assert                  = require("assert");
var List                    = require("../list");

function from_suites(name, suite) {
  describe(name, function () {
        return List.iter(function (param) {
                    it(param[0], param[1]);
                    return /* () */0;
                  }, suite);
      });
  return /* () */0;
}

function close_enough(x, y) {
  return +(Math.abs(x - y) < 0.0000001);
}

function from_pair_suites(name, suites) {
  describe(name, function () {
        return List.iter(function (param) {
                    var code = param[1];
                    it(param[0], function () {
                          var match = Curry._1(code, /* () */0);
                          switch (match.tag | 0) {
                            case 0 : 
                                Assert.deepEqual(match[0], match[1]);
                                return /* () */0;
                            case 1 : 
                                Assert.notDeepEqual(match[0], match[1]);
                                return /* () */0;
                            case 2 : 
                                if (close_enough(match[0], match[1])) {
                                  return 0;
                                }
                                else {
                                  throw [
                                        Caml_builtin_exceptions.assert_failure,
                                        [
                                          "mt.ml",
                                          56,
                                          16
                                        ]
                                      ];
                                }
                                break;
                            case 3 : 
                                Assert.throws(match[0]);
                                return /* () */0;
                            
                          }
                        });
                    return /* () */0;
                  }, suites);
      });
  return /* () */0;
}

exports.from_suites      = from_suites;
exports.from_pair_suites = from_pair_suites;
/* assert Not a pure module */
