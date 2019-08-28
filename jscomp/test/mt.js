'use strict';

var List = require("../../lib/js/list.js");
var Path = require("path");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Assert = require("assert");
var Process = require("process");
var Pervasives = require("../../lib/js/pervasives.js");

function assert_fail(msg) {
  Assert.fail(/* () */0, /* () */0, msg, "");
  return /* () */0;
}

function is_mocha(param) {
  var match = $$Array.to_list(Process.argv);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      var exec = Path.basename(match$1[0]);
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
  if (match && is_mocha(/* () */0)) {
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
  switch (spec.tag | 0) {
    case /* Eq */0 :
        Assert.deepEqual(spec[0], spec[1]);
        return /* () */0;
    case /* Neq */1 :
        Assert.notDeepEqual(spec[0], spec[1]);
        return /* () */0;
    case /* StrictEq */2 :
        Assert.strictEqual(spec[0], spec[1]);
        return /* () */0;
    case /* StrictNeq */3 :
        Assert.notStrictEqual(spec[0], spec[1]);
        return /* () */0;
    case /* Ok */4 :
        Assert.ok(spec[0]);
        return /* () */0;
    case /* Approx */5 :
        var b = spec[1];
        var a = spec[0];
        if (close_enough(undefined, a, b)) {
          return 0;
        } else {
          Assert.deepEqual(a, b);
          return /* () */0;
        }
    case /* ApproxThreshold */6 :
        var b$1 = spec[2];
        var a$1 = spec[1];
        if (close_enough(spec[0], a$1, b$1)) {
          return 0;
        } else {
          Assert.deepEqual(a$1, b$1);
          return /* () */0;
        }
    case /* ThrowAny */7 :
        Assert.throws(spec[0]);
        return /* () */0;
    case /* Fail */8 :
        return assert_fail("failed");
    case /* FailWith */9 :
        return assert_fail(spec[0]);
    
  }
}

function from_pair_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match) {
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
                    switch (match.tag | 0) {
                      case /* Eq */0 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "eq?",
                                match[1]
                              ]);
                          return /* () */0;
                      case /* Neq */1 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "neq?",
                                match[1]
                              ]);
                          return /* () */0;
                      case /* StrictEq */2 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "strict_eq?",
                                match[1]
                              ]);
                          return /* () */0;
                      case /* StrictNeq */3 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "strict_neq?",
                                match[1]
                              ]);
                          return /* () */0;
                      case /* Ok */4 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "ok?"
                              ]);
                          return /* () */0;
                      case /* Approx */5 :
                          console.log(/* tuple */[
                                name,
                                match[0],
                                "~",
                                match[1]
                              ]);
                          return /* () */0;
                      case /* ApproxThreshold */6 :
                          console.log(/* tuple */[
                                name,
                                match[1],
                                "~",
                                match[2],
                                " (",
                                match[0],
                                ")"
                              ]);
                          return /* () */0;
                      case /* ThrowAny */7 :
                          return /* () */0;
                      case /* Fail */8 :
                          console.log("failed");
                          return /* () */0;
                      case /* FailWith */9 :
                          console.log("failed: " + match[0]);
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
  if (match) {
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
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function bool_suites(test_id, suites, loc, x) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Ok */Block.__(4, [x]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function throw_suites(test_id, suites, loc, x) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* ThrowAny */Block.__(7, [x]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

exports.from_suites = from_suites;
exports.from_pair_suites = from_pair_suites;
exports.from_promise_suites = from_promise_suites;
exports.eq_suites = eq_suites;
exports.bool_suites = bool_suites;
exports.throw_suites = throw_suites;
/* val_unit Not a pure module */
