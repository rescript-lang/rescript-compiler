'use strict';

var Mt                      = require("./mt");
var Block                   = require("../../lib/js/block");
var Js_json                 = require("../../lib/js/js_json");
var Js_primitive            = require("../../lib/js/js_primitive");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

var suites = [/* [] */0];

var counter = [0];

function add_test(loc, test) {
  counter[0] = counter[0] + 1 | 0;
  var id = loc + (" id " + counter[0]);
  suites[0] = /* :: */[
    /* tuple */[
      id,
      test
    ],
    suites[0]
  ];
  return /* () */0;
}

function eq(loc, x, y) {
  return add_test(loc, function () {
              return /* Eq */Block.__(0, [
                        x,
                        y
                      ]);
            });
}

function false_(loc) {
  return add_test(loc, function () {
              return /* Ok */Block.__(2, [/* false */0]);
            });
}

function true_(loc) {
  return add_test(loc, function () {
              return /* Ok */Block.__(2, [/* true */1]);
            });
}

var v = JSON.parse(' { "x" : [1, 2, 3 ] } ');

add_test('File "js_json_test.ml", line 23, characters 11-18', function () {
      var match = Js_json.reify_type(v);
      if (match[0] !== 2) {
        return /* Ok */Block.__(2, [/* false */0]);
      }
      else {
        var match$1 = match[1]["x"];
        if (match$1 !== undefined) {
          var match$2 = Js_json.reify_type(match$1);
          if (match$2[0] !== 3) {
            return /* Ok */Block.__(2, [/* false */0]);
          }
          else {
            match$2[1].forEach(function (x) {
                  var match = Js_json.reify_type(x);
                  if (match[0] !== 1) {
                    throw [
                          Caml_builtin_exceptions.assert_failure,
                          [
                            "js_json_test.ml",
                            37,
                            21
                          ]
                        ];
                  }
                  else {
                    return /* () */0;
                  }
                });
            return /* Ok */Block.__(2, [/* true */1]);
          }
        }
        else {
          return /* Ok */Block.__(2, [/* false */0]);
        }
      }
    });

eq('File "js_json_test.ml", line 48, characters 5-12', Js_json.test(v, /* Object */2), /* true */1);

var json = JSON.parse(JSON.stringify(Js_json.$$null));

var match = Js_json.reify_type(json);

if (match[0] >= 5) {
  add_test('File "js_json_test.ml", line 54, characters 26-33', function () {
        return /* Ok */Block.__(2, [/* true */1]);
      });
}
else {
  add_test('File "js_json_test.ml", line 55, characters 16-23', function () {
        return /* Ok */Block.__(2, [/* false */0]);
      });
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var match$1 = Js_json.reify_type(json$1);

if (match$1[0] !== 0) {
  add_test('File "js_json_test.ml", line 65, characters 16-23', function () {
        return /* Ok */Block.__(2, [/* false */0]);
      });
}
else {
  eq('File "js_json_test.ml", line 64, characters 25-32', match$1[1], "test string");
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var match$2 = Js_json.reify_type(json$2);

if (match$2[0] !== 1) {
  add_test('File "js_json_test.ml", line 75, characters 18-25', function () {
        return /* Ok */Block.__(2, [/* false */0]);
      });
}
else {
  eq('File "js_json_test.ml", line 74, characters 25-32', match$2[1], 1.23456789);
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v ? true : false));
  var match = Js_json.reify_type(json);
  if (match[0] !== 4) {
    return add_test('File "js_json_test.ml", line 85, characters 18-25', function () {
                return /* Ok */Block.__(2, [/* false */0]);
              });
  }
  else {
    return eq('File "js_json_test.ml", line 84, characters 28-35', +match[1], v);
  }
}

test(/* true */1);

test(/* false */0);

function option_get(param) {
  if (param) {
    return param[0];
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "js_json_test.ml",
            91,
            36
          ]
        ];
  }
}

var dict = { };

dict["a"] = "test string";

dict["b"] = 123.0;

var json$3 = JSON.parse(JSON.stringify(dict));

var match$3 = Js_json.reify_type(json$3);

var x = match$3[1];

if (match$3[0] !== 2) {
  add_test('File "js_json_test.ml", line 123, characters 16-23', function () {
        return /* Ok */Block.__(2, [/* false */0]);
      });
}
else {
  var match$4 = Js_json.reify_type(option_get(Js_primitive.undefined_to_opt(x["a"])));
  if (match$4[0] !== 0) {
    add_test('File "js_json_test.ml", line 121, characters 18-25', function () {
          return /* Ok */Block.__(2, [/* false */0]);
        });
  }
  else if (match$4[1] !== "test string") {
    add_test('File "js_json_test.ml", line 112, characters 18-25', function () {
          return /* Ok */Block.__(2, [/* false */0]);
        });
  }
  else {
    var match$5 = Js_json.reify_type(option_get(Js_primitive.undefined_to_opt(x["b"])));
    var b = match$5[1];
    if (match$5[0] !== 1) {
      add_test('File "js_json_test.ml", line 119, characters 22-29', function () {
            return /* Ok */Block.__(2, [/* false */0]);
          });
    }
    else {
      add_test('File "js_json_test.ml", line 118, characters 19-26', function () {
            return /* Approx */Block.__(3, [
                      123.0,
                      b
                    ]);
          });
    }
  }
}

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites     = suites;
exports.add_test   = add_test;
exports.eq         = eq;
exports.false_     = false_;
exports.true_      = true_;
exports.option_get = option_get;
/* v Not a pure module */
