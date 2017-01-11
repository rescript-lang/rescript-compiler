'use strict';

var Process                 = require("process");
var Assert                  = require("assert");
var $$Array                 = require("../../lib/js/array");
var Curry                   = require("../../lib/js/curry");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var List                    = require("../../lib/js/list");
var Path                    = require("path");

function is_mocha() {
  var match = $$Array.to_list(Process.argv);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      var exec = Path.basename(match$1[0]);
      if (exec === "mocha") {
        return /* true */1;
      }
      else {
        return +(exec === "_mocha");
      }
    }
    else {
      return /* false */0;
    }
  }
  else {
    return /* false */0;
  }
}

function from_suites(name, suite) {
  var match = $$Array.to_list(Process.argv);
  if (match && is_mocha(/* () */0)) {
    describe(name, function () {
          return List.iter(function (param) {
                      it(param[0], param[1]);
                      return /* () */0;
                    }, suite);
        });
    return /* () */0;
  }
  else {
    return /* () */0;
  }
}

function close_enough(x, y) {
  return +(Math.abs(x - y) < 0.0000001);
}

function from_pair_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match && is_mocha(/* () */0)) {
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
                                            71,
                                            20
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
  else {
    return /* () */0;
  }
}

exports.from_suites      = from_suites;
exports.from_pair_suites = from_pair_suites;
/* process Not a pure module */
