'use strict';

var List = require("../../lib/js/list.js");
var Path = require("path");
var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Assert = require("assert");
var Process = require("process");

function assert_fail(msg) {
  Assert.fail(/* () */0, /* () */0, msg, "");
  return /* () */0;
}

function is_mocha(param) {
  var match = $$Array.to_list(Process.argv);
  if (match !== "[]") {
    var match$1 = match.Arg1;
    if (match$1 !== "[]") {
      var exec = Path.basename(match$1.Arg0);
      if (exec === "mocha") {
        return true;
      } else {
        return exec === "_mocha";
      }
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function from_suites(name, suite) {
  var match = $$Array.to_list(Process.argv);
  if (match !== "[]" && is_mocha(/* () */0)) {
    describe(name, (function () {
            return List.iter((function (param) {
                          var partial_arg = param[1];
                          it(param[0], (function () {
                                  return Curry._1(partial_arg, /* () */0);
                                }));
                          return /* () */0;
                        }), suite);
          }));
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function close_enough($staropt$star, a, b) {
  var threshold = $staropt$star !== undefined ? $staropt$star : 0.0000001;
  return Math.abs(a - b) < threshold;
}

function handleCode(spec) {
  switch (/* XXX */spec.tag) {
    case "Eq" :
        Assert.deepEqual(spec.Arg0, spec.Arg1);
        return /* () */0;
    case "Neq" :
        Assert.notDeepEqual(spec.Arg0, spec.Arg1);
        return /* () */0;
    case "StrictEq" :
        Assert.strictEqual(spec.Arg0, spec.Arg1);
        return /* () */0;
    case "StrictNeq" :
        Assert.notStrictEqual(spec.Arg0, spec.Arg1);
        return /* () */0;
    case "Ok" :
        Assert.ok(spec.Arg0);
        return /* () */0;
    case "Approx" :
        var b = spec.Arg1;
        var a = spec.Arg0;
        if (close_enough(undefined, a, b)) {
          return 0;
        } else {
          Assert.deepEqual(a, b);
          return /* () */0;
        }
    case "ApproxThreshold" :
        var b$1 = spec.Arg2;
        var a$1 = spec.Arg1;
        if (close_enough(spec.Arg0, a$1, b$1)) {
          return 0;
        } else {
          Assert.deepEqual(a$1, b$1);
          return /* () */0;
        }
    case "ThrowAny" :
        Assert.throws(spec.Arg0);
        return /* () */0;
    case "Fail" :
        return assert_fail("failed");
    case "FailWith" :
        return assert_fail(spec.Arg0);
    
  }
}

function from_pair_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match !== "[]") {
    if (is_mocha(/* () */0)) {
      describe(name, (function () {
              return List.iter((function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    return handleCode(Curry._1(code, /* () */0));
                                  }));
                            return /* () */0;
                          }), suites);
            }));
      return /* () */0;
    } else {
      var name$1 = name;
      var suites$1 = suites;
      console.log(/* tuple */[
            name$1,
            "testing"
          ]);
      return List.iter((function (param) {
                    var name = param[0];
                    var match = Curry._1(param[1], /* () */0);
                    switch (/* XXX */match.tag) {
                      case "Eq" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "eq?",
                                match.Arg1
                              ]);
                          return /* () */0;
                      case "Neq" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "neq?",
                                match.Arg1
                              ]);
                          return /* () */0;
                      case "StrictEq" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "strict_eq?",
                                match.Arg1
                              ]);
                          return /* () */0;
                      case "StrictNeq" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "strict_neq?",
                                match.Arg1
                              ]);
                          return /* () */0;
                      case "Ok" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "ok?"
                              ]);
                          return /* () */0;
                      case "Approx" :
                          console.log(/* tuple */[
                                name,
                                match.Arg0,
                                "~",
                                match.Arg1
                              ]);
                          return /* () */0;
                      case "ApproxThreshold" :
                          console.log(/* tuple */[
                                name,
                                match.Arg1,
                                "~",
                                match.Arg2,
                                " (",
                                match.Arg0,
                                ")"
                              ]);
                          return /* () */0;
                      case "ThrowAny" :
                          return /* () */0;
                      case "Fail" :
                          console.log("failed");
                          return /* () */0;
                      case "FailWith" :
                          console.log("failed: " + match.Arg0);
                          return /* () */0;
                      
                    }
                  }), suites$1);
    }
  } else {
    return /* () */0;
  }
}

var val_unit = Promise.resolve(/* () */0);

function from_promise_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match !== "[]") {
    if (is_mocha(/* () */0)) {
      describe(name, (function () {
              return List.iter((function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    return code.then((function (x) {
                                                  handleCode(x);
                                                  return val_unit;
                                                }));
                                  }));
                            return /* () */0;
                          }), suites);
            }));
      return /* () */0;
    } else {
      console.log("promise suites");
      return /* () */0;
    }
  } else {
    return /* () */0;
  }
}

function eq_suites(test_id, suites, loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

function bool_suites(test_id, suites, loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Ok",
                  Arg0: x
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

function throw_suites(test_id, suites, loc, x) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "ThrowAny",
                  Arg0: x
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

exports.from_suites = from_suites;
exports.from_pair_suites = from_pair_suites;
exports.from_promise_suites = from_promise_suites;
exports.eq_suites = eq_suites;
exports.bool_suites = bool_suites;
exports.throw_suites = throw_suites;
/* val_unit Not a pure module */
