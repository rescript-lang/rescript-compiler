// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_curry = require("../runtime/caml_curry");
var Assert     = require("assert");
var List       = require("../stdlib/list");

function from_suites(name, suite) {
  return describe(name, function () {
              return List.iter(function (param) {
                          return it(param[0], param[1]);
                        }, suite);
            });
}

function from_pair_suites(name, suites) {
  return describe(name, function () {
              return List.iter(function (param) {
                          var code = param[1];
                          return it(param[0], function () {
                                      var match = Caml_curry.app1(code, /* () */0);
                                      if (match.tag) {
                                        return Assert.notDeepEqual(match[0], match[1]);
                                      }
                                      else {
                                        return Assert.deepEqual(match[0], match[1]);
                                      }
                                    });
                        }, suites);
            });
}

exports.from_suites      = from_suites;
exports.from_pair_suites = from_pair_suites;
/* assert Not a pure module */
