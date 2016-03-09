// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Assert                  = require("assert");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_curry              = require("../runtime/caml_curry");
var List                    = require("../stdlib/list");

function from_suites(name, suite) {
  return describe(name, function () {
              return List.iter(function (param) {
                          return it(param[0], param[1]);
                        }, suite);
            });
}

function close_enough(x, y) {
  return +(Math.abs(x - y) < 0.0000001);
}

function from_pair_suites(name, suites) {
  return describe(name, function () {
              return List.iter(function (param) {
                          var code = param[1];
                          return it(param[0], function () {
                                      var match = Caml_curry.app1(code, /* () */0);
                                      switch (match.tag | 0) {
                                        case 0 : 
                                            return Assert.deepEqual(match[0], match[1]);
                                        case 1 : 
                                            return Assert.notDeepEqual(match[0], match[1]);
                                        case 2 : 
                                            if (close_enough(match[0], match[1])) {
                                              return 0;
                                            }
                                            else {
                                              throw [
                                                    Caml_builtin_exceptions.assert_failure,
                                                    [
                                                      "mt.ml",
                                                      54,
                                                      16
                                                    ]
                                                  ];
                                            }
                                            break;
                                        case 3 : 
                                            return Assert.throws(match[0]);
                                        
                                      }
                                    });
                        }, suites);
            });
}

exports.from_suites      = from_suites;
exports.from_pair_suites = from_pair_suites;
/* assert Not a pure module */
