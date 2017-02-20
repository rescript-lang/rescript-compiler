'use strict';

var List    = require("../../lib/js/list");
var Path    = require("path");
var $$Array = require("../../lib/js/array");
var Curry   = require("../../lib/js/curry");
var Assert  = require("assert");
var Process = require("process");

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

function close_enough($staropt$star, a, b) {
  var threshold = $staropt$star ? $staropt$star[0] : 0.0000001;
  return +(Math.abs(a - b) < threshold);
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
                                  var b = match[0];
                                  Assert.ok(b ? true : false);
                                  return /* () */0;
                              case 3 : 
                                  var b$1 = match[1];
                                  var a = match[0];
                                  if (close_enough(/* None */0, a, b$1)) {
                                    return 0;
                                  }
                                  else {
                                    Assert.deepEqual(a, b$1);
                                    return /* () */0;
                                  }
                              case 4 : 
                                  var b$2 = match[2];
                                  var a$1 = match[1];
                                  if (close_enough(/* Some */[match[0]], a$1, b$2)) {
                                    return 0;
                                  }
                                  else {
                                    Assert.deepEqual(a$1, b$2);
                                    return /* () */0;
                                  }
                              case 5 : 
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
/* path Not a pure module */
