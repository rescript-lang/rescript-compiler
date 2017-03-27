'use strict';

var Mt                      = require("./mt");
var $$Array                 = require("../../lib/js/array");
var Block                   = require("../../lib/js/block");
var Js_json                 = require("../../lib/js/js_json");
var Caml_obj                = require("../../lib/js/caml_obj");
var Caml_array              = require("../../lib/js/caml_array");
var Js_boolean              = require("../../lib/js/js_boolean");
var Js_primitive            = require("../../lib/js/js_primitive");
var Js_json_decode          = require("../../lib/js/js_json_decode");
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

function throws(loc, f) {
  return add_test(loc, function () {
              return /* ThrowAny */Block.__(7, [f]);
            });
}

function false_(loc) {
  return add_test(loc, function () {
              return /* Ok */Block.__(4, [/* false */0]);
            });
}

function true_(loc) {
  return add_test(loc, function () {
              return /* Ok */Block.__(4, [/* true */1]);
            });
}

eq('File "js_json_test.ml", line 23, characters 5-12', Js_json.parse("null"), /* Ok */Block.__(0, [null]));

eq('File "js_json_test.ml", line 25, characters 5-12', Js_json.parse("-"), /* Error */Block.__(1, ["SyntaxError: Unexpected end of JSON input"]));

function f() {
  JSON.parse("-");
  return /* () */0;
}

add_test('File "js_json_test.ml", line 27, characters 9-16', function () {
      return /* ThrowAny */Block.__(7, [f]);
    });

var v = JSON.parse(' { "x" : [1, 2, 3 ] } ');

add_test('File "js_json_test.ml", line 33, characters 11-18', function () {
      var match = Js_json.reifyType(v);
      if (match[0] !== 2) {
        return /* Ok */Block.__(4, [/* false */0]);
      } else {
        var match$1 = match[1]["x"];
        if (match$1 !== undefined) {
          var match$2 = Js_json.reifyType(match$1);
          if (match$2[0] !== 3) {
            return /* Ok */Block.__(4, [/* false */0]);
          } else {
            match$2[1].forEach(function (x) {
                  var match = Js_json.reifyType(x);
                  if (match[0] !== 1) {
                    throw [
                          Caml_builtin_exceptions.assert_failure,
                          [
                            "js_json_test.ml",
                            47,
                            21
                          ]
                        ];
                  } else {
                    return /* () */0;
                  }
                });
            return /* Ok */Block.__(4, [/* true */1]);
          }
        } else {
          return /* Ok */Block.__(4, [/* false */0]);
        }
      }
    });

eq('File "js_json_test.ml", line 58, characters 5-12', Js_json.test(v, /* Object */2), /* true */1);

var json = JSON.parse(JSON.stringify(null));

var match = Js_json.reifyType(json);

if (match[0] >= 5) {
  add_test('File "js_json_test.ml", line 64, characters 26-33', function () {
        return /* Ok */Block.__(4, [/* true */1]);
      });
} else {
  console.log(match[1]);
  add_test('File "js_json_test.ml", line 65, characters 26-33', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var match$1 = Js_json.reifyType(json$1);

if (match$1[0] !== 0) {
  add_test('File "js_json_test.ml", line 75, characters 16-23', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq('File "js_json_test.ml", line 74, characters 25-32', match$1[1], "test string");
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var match$2 = Js_json.reifyType(json$2);

if (match$2[0] !== 1) {
  add_test('File "js_json_test.ml", line 85, characters 18-25', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq('File "js_json_test.ml", line 84, characters 25-32', match$2[1], 1.23456789);
}

var json$3 = JSON.parse(JSON.stringify(-1347440721));

var match$3 = Js_json.reifyType(json$3);

if (match$3[0] !== 1) {
  add_test('File "js_json_test.ml", line 95, characters 18-25', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq('File "js_json_test.ml", line 94, characters 25-32', match$3[1] | 0, -1347440721);
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v));
  var match = Js_json.reifyType(json);
  if (match[0] !== 4) {
    return add_test('File "js_json_test.ml", line 105, characters 18-25', function () {
                return /* Ok */Block.__(4, [/* false */0]);
              });
  } else {
    return eq('File "js_json_test.ml", line 104, characters 28-35', match[1], v);
  }
}

test(true);

test(false);

function option_get(param) {
  if (param) {
    return param[0];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "js_json_test.ml",
            111,
            36
          ]
        ];
  }
}

var dict = { };

dict["a"] = "test string";

dict["b"] = 123.0;

var json$4 = JSON.parse(JSON.stringify(dict));

var match$4 = Js_json.reifyType(json$4);

var x = match$4[1];

if (match$4[0] !== 2) {
  add_test('File "js_json_test.ml", line 143, characters 16-23', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  var match$5 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(x["a"])));
  if (match$5[0] !== 0) {
    add_test('File "js_json_test.ml", line 141, characters 18-25', function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else if (match$5[1] !== "test string") {
    add_test('File "js_json_test.ml", line 132, characters 18-25', function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else {
    var match$6 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(x["b"])));
    var b = match$6[1];
    if (match$6[0] !== 1) {
      add_test('File "js_json_test.ml", line 139, characters 22-29', function () {
            return /* Ok */Block.__(4, [/* false */0]);
          });
    } else {
      add_test('File "js_json_test.ml", line 138, characters 19-26', function () {
            return /* Approx */Block.__(5, [
                      123.0,
                      b
                    ]);
          });
    }
  }
}

function eq_at_i(loc, json, i, kind, expected) {
  var match = Js_json.reifyType(json);
  if (match[0] !== 3) {
    return add_test(loc, function () {
                return /* Ok */Block.__(4, [/* false */0]);
              });
  } else {
    var match$1 = Js_json.reifyType(Caml_array.caml_array_get(match[1], i));
    if (Caml_obj.caml_equal(match$1[0], kind)) {
      return eq(loc, match$1[1], expected);
    } else {
      return add_test(loc, function () {
                  return /* Ok */Block.__(4, [/* false */0]);
                });
    }
  }
}

var json$5 = JSON.parse(JSON.stringify($$Array.map(function (prim) {
              return prim;
            }, /* array */[
              "string 0",
              "string 1",
              "string 2"
            ])));

eq_at_i('File "js_json_test.ml", line 174, characters 10-17', json$5, 0, /* String */0, "string 0");

eq_at_i('File "js_json_test.ml", line 175, characters 10-17', json$5, 1, /* String */0, "string 1");

eq_at_i('File "js_json_test.ml", line 176, characters 10-17', json$5, 2, /* String */0, "string 2");

var json$6 = JSON.parse(JSON.stringify(/* array */[
          "string 0",
          "string 1",
          "string 2"
        ]));

eq_at_i('File "js_json_test.ml", line 186, characters 10-17', json$6, 0, /* String */0, "string 0");

eq_at_i('File "js_json_test.ml", line 187, characters 10-17', json$6, 1, /* String */0, "string 1");

eq_at_i('File "js_json_test.ml", line 188, characters 10-17', json$6, 2, /* String */0, "string 2");

var a = /* float array */[
  1.0000001,
  10000000000.1,
  123.0
];

var json$7 = JSON.parse(JSON.stringify(a));

eq_at_i('File "js_json_test.ml", line 200, characters 10-17', json$7, 0, /* Number */1, Caml_array.caml_array_get(a, 0));

eq_at_i('File "js_json_test.ml", line 201, characters 10-17', json$7, 1, /* Number */1, Caml_array.caml_array_get(a, 1));

eq_at_i('File "js_json_test.ml", line 202, characters 10-17', json$7, 2, /* Number */1, Caml_array.caml_array_get(a, 2));

var a$1 = /* int array */[
  0,
  -1347440721,
  -268391749
];

var json$8 = JSON.parse(JSON.stringify($$Array.map(function (prim) {
              return prim;
            }, a$1)));

eq_at_i('File "js_json_test.ml", line 215, characters 10-17', json$8, 0, /* Number */1, Caml_array.caml_array_get(a$1, 0));

eq_at_i('File "js_json_test.ml", line 216, characters 10-17', json$8, 1, /* Number */1, Caml_array.caml_array_get(a$1, 1));

eq_at_i('File "js_json_test.ml", line 217, characters 10-17', json$8, 2, /* Number */1, Caml_array.caml_array_get(a$1, 2));

var a$2 = /* int array */[
  /* true */1,
  /* false */0,
  /* true */1
];

var json$9 = JSON.parse(JSON.stringify($$Array.map(Js_boolean.to_js_boolean, a$2)));

var b$1 = Caml_array.caml_array_get(a$2, 0);

eq_at_i('File "js_json_test.ml", line 230, characters 10-17', json$9, 0, /* Boolean */4, b$1 ? true : false);

var b$2 = Caml_array.caml_array_get(a$2, 1);

eq_at_i('File "js_json_test.ml", line 231, characters 10-17', json$9, 1, /* Boolean */4, b$2 ? true : false);

var b$3 = Caml_array.caml_array_get(a$2, 2);

eq_at_i('File "js_json_test.ml", line 232, characters 10-17', json$9, 2, /* Boolean */4, b$3 ? true : false);

function make_d(s, i) {
  var d = { };
  d["a"] = s;
  d["b"] = i;
  return d;
}

var a$3 = /* array */[
  make_d("aaa", 123),
  make_d("bbb", 456)
];

var json$10 = JSON.parse(JSON.stringify(a$3));

var match$7 = Js_json.reifyType(json$10);

if (match$7[0] !== 3) {
  add_test('File "js_json_test.ml", line 264, characters 16-23', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  var match$8 = Js_json.reifyType(Caml_array.caml_array_get(match$7[1], 1));
  if (match$8[0] !== 2) {
    add_test('File "js_json_test.ml", line 262, characters 18-25', function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else {
    var match$9 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(match$8[1]["a"])));
    if (match$9[0] !== 0) {
      add_test('File "js_json_test.ml", line 260, characters 20-27', function () {
            return /* Ok */Block.__(4, [/* false */0]);
          });
    } else {
      eq('File "js_json_test.ml", line 259, characters 29-36', match$9[1], "bbb");
    }
  }
}

try {
  JSON.parse("{{ A}");
  add_test('File "js_json_test.ml", line 270, characters 11-18', function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
}
catch (exn){
  add_test('File "js_json_test.ml", line 273, characters 10-17', function () {
        return /* Ok */Block.__(4, [/* true */1]);
      });
}

eq('File "js_json_test.ml", line 277, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(/* int array */[
              1,
              2,
              3
            ])), /* Some */["[1,2,3]"]);

eq('File "js_json_test.ml", line 281, characters 2-9', Js_primitive.undefined_to_opt(JSON.stringify({
              foo: 1,
              bar: "hello",
              baz: {
                baaz: 10
              }
            })), /* Some */['{"foo":1,"bar":"hello","baz":{"baaz":10}}']);

eq('File "js_json_test.ml", line 285, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(null)), /* Some */["null"]);

eq('File "js_json_test.ml", line 287, characters 12-19', Js_primitive.undefined_to_opt(JSON.stringify(undefined)), /* None */0);

eq('File "js_json_test.ml", line 290, characters 5-12', Js_json.decodeString("test"), /* Some */["test"]);

eq('File "js_json_test.ml", line 292, characters 5-12', Js_json.decodeString(true), /* None */0);

eq('File "js_json_test.ml", line 294, characters 5-12', Js_json.decodeString(/* array */[]), /* None */0);

eq('File "js_json_test.ml", line 296, characters 5-12', Js_json.decodeString(null), /* None */0);

eq('File "js_json_test.ml", line 298, characters 5-12', Js_json.decodeString({ }), /* None */0);

eq('File "js_json_test.ml", line 300, characters 5-12', Js_json.decodeString(1.23), /* None */0);

eq('File "js_json_test.ml", line 304, characters 5-12', Js_json.decodeNumber("test"), /* None */0);

eq('File "js_json_test.ml", line 306, characters 5-12', Js_json.decodeNumber(true), /* None */0);

eq('File "js_json_test.ml", line 308, characters 5-12', Js_json.decodeNumber(/* array */[]), /* None */0);

eq('File "js_json_test.ml", line 310, characters 5-12', Js_json.decodeNumber(null), /* None */0);

eq('File "js_json_test.ml", line 312, characters 5-12', Js_json.decodeNumber({ }), /* None */0);

eq('File "js_json_test.ml", line 314, characters 5-12', Js_json.decodeNumber(1.23), /* Some */[1.23]);

eq('File "js_json_test.ml", line 318, characters 5-12', Js_json.decodeObject("test"), /* None */0);

eq('File "js_json_test.ml", line 320, characters 5-12', Js_json.decodeObject(true), /* None */0);

eq('File "js_json_test.ml", line 322, characters 5-12', Js_json.decodeObject(/* array */[]), /* None */0);

eq('File "js_json_test.ml", line 324, characters 5-12', Js_json.decodeObject(null), /* None */0);

eq('File "js_json_test.ml", line 326, characters 5-12', Js_json.decodeObject({ }), /* Some */[{ }]);

eq('File "js_json_test.ml", line 329, characters 5-12', Js_json.decodeObject(1.23), /* None */0);

eq('File "js_json_test.ml", line 333, characters 5-12', Js_json.decodeArray("test"), /* None */0);

eq('File "js_json_test.ml", line 335, characters 5-12', Js_json.decodeArray(true), /* None */0);

eq('File "js_json_test.ml", line 337, characters 5-12', Js_json.decodeArray(/* array */[]), /* Some */[/* array */[]]);

eq('File "js_json_test.ml", line 339, characters 5-12', Js_json.decodeArray(null), /* None */0);

eq('File "js_json_test.ml", line 341, characters 5-12', Js_json.decodeArray({ }), /* None */0);

eq('File "js_json_test.ml", line 343, characters 5-12', Js_json.decodeArray(1.23), /* None */0);

eq('File "js_json_test.ml", line 347, characters 5-12', Js_json.decodeBoolean("test"), /* None */0);

eq('File "js_json_test.ml", line 349, characters 5-12', Js_json.decodeBoolean(true), /* Some */[true]);

eq('File "js_json_test.ml", line 351, characters 5-12', Js_json.decodeBoolean(/* array */[]), /* None */0);

eq('File "js_json_test.ml", line 353, characters 5-12', Js_json.decodeBoolean(null), /* None */0);

eq('File "js_json_test.ml", line 355, characters 5-12', Js_json.decodeBoolean({ }), /* None */0);

eq('File "js_json_test.ml", line 357, characters 5-12', Js_json.decodeBoolean(1.23), /* None */0);

eq('File "js_json_test.ml", line 361, characters 5-12', Js_json.decodeNull("test"), /* None */0);

eq('File "js_json_test.ml", line 363, characters 5-12', Js_json.decodeNull(true), /* None */0);

eq('File "js_json_test.ml", line 365, characters 5-12', Js_json.decodeNull(/* array */[]), /* None */0);

eq('File "js_json_test.ml", line 367, characters 5-12', Js_json.decodeNull(null), /* Some */[null]);

eq('File "js_json_test.ml", line 369, characters 5-12', Js_json.decodeNull({ }), /* None */0);

eq('File "js_json_test.ml", line 371, characters 5-12', Js_json.decodeNull(1.23), /* None */0);

eq('File "js_json_test.ml", line 381, characters 5-12', Js_json_decode.$$boolean(true), /* Ok */Block.__(0, [true]));

eq('File "js_json_test.ml", line 383, characters 5-12', Js_json_decode.$$boolean(1.23), /* Error */Block.__(1, ["Expected boolean, got 1.23"]));

eq('File "js_json_test.ml", line 385, characters 5-12', Js_json_decode.$$boolean(23), /* Error */Block.__(1, ["Expected boolean, got 23"]));

eq('File "js_json_test.ml", line 387, characters 5-12', Js_json_decode.$$boolean("test"), /* Error */Block.__(1, ['Expected boolean, got "test"']));

eq('File "js_json_test.ml", line 389, characters 5-12', Js_json_decode.$$boolean(null), /* Error */Block.__(1, ["Expected boolean, got null"]));

eq('File "js_json_test.ml", line 391, characters 5-12', Js_json_decode.$$boolean(/* array */[]), /* Error */Block.__(1, ["Expected boolean, got []"]));

eq('File "js_json_test.ml", line 393, characters 5-12', Js_json_decode.$$boolean({ }), /* Error */Block.__(1, ["Expected boolean, got {}"]));

eq('File "js_json_test.ml", line 399, characters 5-12', Js_json_decode.$$float(true), /* Error */Block.__(1, ["Expected number, got true"]));

eq('File "js_json_test.ml", line 401, characters 5-12', Js_json_decode.$$float(1.23), /* Ok */Block.__(0, [1.23]));

eq('File "js_json_test.ml", line 403, characters 5-12', Js_json_decode.$$float(23), /* Ok */Block.__(0, [23]));

eq('File "js_json_test.ml", line 405, characters 5-12', Js_json_decode.$$float("test"), /* Error */Block.__(1, ['Expected number, got "test"']));

eq('File "js_json_test.ml", line 407, characters 5-12', Js_json_decode.$$float(null), /* Error */Block.__(1, ["Expected number, got null"]));

eq('File "js_json_test.ml", line 409, characters 5-12', Js_json_decode.$$float(/* array */[]), /* Error */Block.__(1, ["Expected number, got []"]));

eq('File "js_json_test.ml", line 411, characters 5-12', Js_json_decode.$$float({ }), /* Error */Block.__(1, ["Expected number, got {}"]));

eq('File "js_json_test.ml", line 417, characters 5-12', Js_json_decode.$$int(true), /* Error */Block.__(1, ["Expected number, got true"]));

eq('File "js_json_test.ml", line 419, characters 5-12', Js_json_decode.$$int(1.23), /* Error */Block.__(1, ["Expected integer, got 1.23"]));

eq('File "js_json_test.ml", line 421, characters 5-12', Js_json_decode.$$int(23), /* Ok */Block.__(0, [23]));

eq('File "js_json_test.ml", line 423, characters 5-12', Js_json_decode.$$int("test"), /* Error */Block.__(1, ['Expected number, got "test"']));

eq('File "js_json_test.ml", line 425, characters 5-12', Js_json_decode.$$int(null), /* Error */Block.__(1, ["Expected number, got null"]));

eq('File "js_json_test.ml", line 427, characters 5-12', Js_json_decode.$$int(/* array */[]), /* Error */Block.__(1, ["Expected number, got []"]));

eq('File "js_json_test.ml", line 429, characters 5-12', Js_json_decode.$$int({ }), /* Error */Block.__(1, ["Expected number, got {}"]));

eq('File "js_json_test.ml", line 435, characters 5-12', Js_json_decode.string(true), /* Error */Block.__(1, ["Expected string, got true"]));

eq('File "js_json_test.ml", line 437, characters 5-12', Js_json_decode.string(1.23), /* Error */Block.__(1, ["Expected string, got 1.23"]));

eq('File "js_json_test.ml", line 439, characters 5-12', Js_json_decode.string(23), /* Error */Block.__(1, ["Expected string, got 23"]));

eq('File "js_json_test.ml", line 441, characters 5-12', Js_json_decode.string("test"), /* Ok */Block.__(0, ["test"]));

eq('File "js_json_test.ml", line 443, characters 5-12', Js_json_decode.string(null), /* Error */Block.__(1, ["Expected string, got null"]));

eq('File "js_json_test.ml", line 445, characters 5-12', Js_json_decode.string(/* array */[]), /* Error */Block.__(1, ["Expected string, got []"]));

eq('File "js_json_test.ml", line 447, characters 5-12', Js_json_decode.string({ }), /* Error */Block.__(1, ["Expected string, got {}"]));

eq('File "js_json_test.ml", line 453, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, true), /* Error */Block.__(1, ["Expected number, got true"]));

eq('File "js_json_test.ml", line 455, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, 1.23), /* Error */Block.__(1, ["Expected integer, got 1.23"]));

eq('File "js_json_test.ml", line 457, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, 23), /* Ok */Block.__(0, [23]));

eq('File "js_json_test.ml", line 459, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, "test"), /* Error */Block.__(1, ['Expected number, got "test"']));

eq('File "js_json_test.ml", line 461, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, null), /* Ok */Block.__(0, [null]));

eq('File "js_json_test.ml", line 463, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, /* array */[]), /* Error */Block.__(1, ["Expected number, got []"]));

eq('File "js_json_test.ml", line 465, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$int, { }), /* Error */Block.__(1, ["Expected number, got {}"]));

eq('File "js_json_test.ml", line 467, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$boolean, true), /* Ok */Block.__(0, [true]));

eq('File "js_json_test.ml", line 469, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$float, 1.23), /* Ok */Block.__(0, [1.23]));

eq('File "js_json_test.ml", line 471, characters 5-12', Js_json_decode.nullable(Js_json_decode.string, "test"), /* Ok */Block.__(0, ["test"]));

var partial_arg = null;

eq('File "js_json_test.ml", line 473, characters 5-12', Js_json_decode.nullable(function (param) {
          return Js_json_decode.nullAs(partial_arg, param);
        }, null), /* Ok */Block.__(0, [null]));

eq('File "js_json_test.ml", line 475, characters 5-12', Js_json_decode.nullable(Js_json_decode.$$boolean, 1), /* Error */Block.__(1, ["Expected boolean, got 1"]));

eq('File "js_json_test.ml", line 481, characters 5-12', Js_json_decode.nullAs(0, true), /* Error */Block.__(1, ["Expected null, got true"]));

eq('File "js_json_test.ml", line 483, characters 5-12', Js_json_decode.nullAs(0, 1.23), /* Error */Block.__(1, ["Expected null, got 1.23"]));

eq('File "js_json_test.ml", line 485, characters 5-12', Js_json_decode.nullAs(0, 23), /* Error */Block.__(1, ["Expected null, got 23"]));

eq('File "js_json_test.ml", line 487, characters 5-12', Js_json_decode.nullAs(0, "test"), /* Error */Block.__(1, ['Expected null, got "test"']));

eq('File "js_json_test.ml", line 489, characters 5-12', Js_json_decode.nullAs(0, null), /* Ok */Block.__(0, [0]));

eq('File "js_json_test.ml", line 491, characters 5-12', Js_json_decode.nullAs(0, /* array */[]), /* Error */Block.__(1, ["Expected null, got []"]));

eq('File "js_json_test.ml", line 493, characters 5-12', Js_json_decode.nullAs(0, { }), /* Error */Block.__(1, ["Expected null, got {}"]));

eq('File "js_json_test.ml", line 495, characters 5-12', Js_json_decode.nullAs(null, null), /* Ok */Block.__(0, [null]));

eq('File "js_json_test.ml", line 497, characters 5-12', Js_json_decode.nullAs(/* None */0, null), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 499, characters 5-12', Js_json_decode.nullAs(/* Some */["foo"], null), /* Ok */Block.__(0, [/* Some */["foo"]]));

eq('File "js_json_test.ml", line 505, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, true), /* Error */Block.__(1, ["Expected array, got true"]));

eq('File "js_json_test.ml", line 507, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, 1.23), /* Error */Block.__(1, ["Expected array, got 1.23"]));

eq('File "js_json_test.ml", line 509, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, 23), /* Error */Block.__(1, ["Expected array, got 23"]));

eq('File "js_json_test.ml", line 511, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, "test"), /* Error */Block.__(1, ['Expected array, got "test"']));

eq('File "js_json_test.ml", line 513, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, null), /* Error */Block.__(1, ["Expected array, got null"]));

eq('File "js_json_test.ml", line 515, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, /* array */[]), /* Ok */Block.__(0, [/* int array */[]]));

eq('File "js_json_test.ml", line 517, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, { }), /* Error */Block.__(1, ["Expected array, got {}"]));

eq('File "js_json_test.ml", line 519, characters 5-12', Js_json_decode.array(Js_json_decode.$$boolean, JSON.parse(" [true, false, true] ")), /* Ok */Block.__(0, [/* array */[
          true,
          false,
          true
        ]]));

eq('File "js_json_test.ml", line 521, characters 5-12', Js_json_decode.array(Js_json_decode.$$float, JSON.parse(" [1, 2, 3] ")), /* Ok */Block.__(0, [/* float array */[
          1,
          2,
          3
        ]]));

eq('File "js_json_test.ml", line 523, characters 5-12', Js_json_decode.array(Js_json_decode.$$int, JSON.parse(" [1, 2, 3] ")), /* Ok */Block.__(0, [/* int array */[
          1,
          2,
          3
        ]]));

eq('File "js_json_test.ml", line 525, characters 5-12', Js_json_decode.array(Js_json_decode.string, JSON.parse(' ["a", "b", "c"] ')), /* Ok */Block.__(0, [/* array */[
          "a",
          "b",
          "c"
        ]]));

var partial_arg$1 = null;

eq('File "js_json_test.ml", line 527, characters 5-12', Js_json_decode.array(function (param) {
          return Js_json_decode.nullAs(partial_arg$1, param);
        }, JSON.parse(" [null, null, null] ")), /* Ok */Block.__(0, [/* array */[
          null,
          null,
          null
        ]]));

eq('File "js_json_test.ml", line 529, characters 5-12', Js_json_decode.array(Js_json_decode.$$boolean, JSON.parse(" [1, 2, 3] ")), /* Error */Block.__(1, ["Expected boolean, got 1"]));

eq('File "js_json_test.ml", line 535, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, true), /* Error */Block.__(1, ["Expected object, got true"]));

eq('File "js_json_test.ml", line 537, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, 1.23), /* Error */Block.__(1, ["Expected object, got 1.23"]));

eq('File "js_json_test.ml", line 539, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, 23), /* Error */Block.__(1, ["Expected object, got 23"]));

eq('File "js_json_test.ml", line 541, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, "test"), /* Error */Block.__(1, ['Expected object, got "test"']));

eq('File "js_json_test.ml", line 543, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, null), /* Error */Block.__(1, ["Expected object, got null"]));

eq('File "js_json_test.ml", line 545, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, /* array */[]), /* Error */Block.__(1, ["Expected object, got []"]));

eq('File "js_json_test.ml", line 547, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, { }), /* Ok */Block.__(0, [{ }]));

eq('File "js_json_test.ml", line 550, characters 5-12', Js_json_decode.dict(Js_json_decode.$$boolean, JSON.parse(' { "a": true, "b": false } ')), /* Ok */Block.__(0, [{
          a: /* true */1,
          b: /* false */0
        }]));

eq('File "js_json_test.ml", line 553, characters 5-12', Js_json_decode.dict(Js_json_decode.$$float, JSON.parse(' { "a": 1.2, "b": 2.3 } ')), /* Ok */Block.__(0, [{
          a: 1.2,
          b: 2.3
        }]));

eq('File "js_json_test.ml", line 556, characters 5-12', Js_json_decode.dict(Js_json_decode.$$int, JSON.parse(' { "a": 1, "b": 2 } ')), /* Ok */Block.__(0, [{
          a: 1,
          b: 2
        }]));

eq('File "js_json_test.ml", line 559, characters 5-12', Js_json_decode.dict(Js_json_decode.string, JSON.parse(' { "a": "x", "b": "y" } ')), /* Ok */Block.__(0, [{
          a: "x",
          b: "y"
        }]));

var partial_arg$2 = null;

eq('File "js_json_test.ml", line 562, characters 5-12', Js_json_decode.dict(function (param) {
          return Js_json_decode.nullAs(partial_arg$2, param);
        }, JSON.parse(' { "a": null, "b": null } ')), /* Ok */Block.__(0, [{
          a: null,
          b: null
        }]));

eq('File "js_json_test.ml", line 565, characters 5-12', Js_json_decode.dict(Js_json_decode.string, JSON.parse(' { "a": null, "b": null } ')), /* Error */Block.__(1, ["Expected string, got null"]));

eq('File "js_json_test.ml", line 572, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, true), /* Error */Block.__(1, ["Expected object, got true"]));

eq('File "js_json_test.ml", line 574, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, 1.23), /* Error */Block.__(1, ["Expected object, got 1.23"]));

eq('File "js_json_test.ml", line 576, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, 23), /* Error */Block.__(1, ["Expected object, got 23"]));

eq('File "js_json_test.ml", line 578, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, "test"), /* Error */Block.__(1, ['Expected object, got "test"']));

eq('File "js_json_test.ml", line 580, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, null), /* Error */Block.__(1, ["Expected object, got null"]));

eq('File "js_json_test.ml", line 582, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, /* array */[]), /* Error */Block.__(1, ["Expected object, got []"]));

eq('File "js_json_test.ml", line 584, characters 5-12', Js_json_decode.field("foo", Js_json_decode.$$int, { }), /* Error */Block.__(1, ["Expected field 'foo'"]));

eq('File "js_json_test.ml", line 587, characters 5-12', Js_json_decode.field("b", Js_json_decode.$$boolean, JSON.parse(' { "a": true, "b": false } ')), /* Ok */Block.__(0, [false]));

eq('File "js_json_test.ml", line 590, characters 5-12', Js_json_decode.field("b", Js_json_decode.$$float, JSON.parse(' { "a": 1.2, "b": 2.3 } ')), /* Ok */Block.__(0, [2.3]));

eq('File "js_json_test.ml", line 593, characters 5-12', Js_json_decode.field("b", Js_json_decode.$$int, JSON.parse(' { "a": 1, "b": 2 } ')), /* Ok */Block.__(0, [2]));

eq('File "js_json_test.ml", line 596, characters 5-12', Js_json_decode.field("b", Js_json_decode.string, JSON.parse(' { "a": "x", "b": "y" } ')), /* Ok */Block.__(0, ["y"]));

var partial_arg$3 = null;

eq('File "js_json_test.ml", line 599, characters 5-12', Js_json_decode.field("b", function (param) {
          return Js_json_decode.nullAs(partial_arg$3, param);
        }, JSON.parse(' { "a": null, "b": null } ')), /* Ok */Block.__(0, [null]));

eq('File "js_json_test.ml", line 602, characters 5-12', Js_json_decode.field("b", Js_json_decode.string, JSON.parse(' { "a": null, "b": null } ')), /* Error */Block.__(1, ["Expected string, got null"]));

eq('File "js_json_test.ml", line 609, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, true), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 611, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, 1.23), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 613, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, 23), /* Ok */Block.__(0, [/* Some */[23]]));

eq('File "js_json_test.ml", line 615, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, "test"), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 617, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, null), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 619, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, /* array */[]), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 621, characters 5-12', Js_json_decode.optional(Js_json_decode.$$int, { }), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 623, characters 5-12', Js_json_decode.optional(Js_json_decode.$$boolean, true), /* Ok */Block.__(0, [/* Some */[true]]));

eq('File "js_json_test.ml", line 625, characters 5-12', Js_json_decode.optional(Js_json_decode.$$float, 1.23), /* Ok */Block.__(0, [/* Some */[1.23]]));

eq('File "js_json_test.ml", line 627, characters 5-12', Js_json_decode.optional(Js_json_decode.string, "test"), /* Ok */Block.__(0, [/* Some */["test"]]));

var partial_arg$4 = null;

eq('File "js_json_test.ml", line 629, characters 5-12', Js_json_decode.optional(function (param) {
          return Js_json_decode.nullAs(partial_arg$4, param);
        }, null), /* Ok */Block.__(0, [/* Some */[null]]));

eq('File "js_json_test.ml", line 631, characters 5-12', Js_json_decode.optional(Js_json_decode.$$boolean, 1), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 633, characters 5-12', Js_json_decode.optional(function (param) {
          return Js_json_decode.field("x", Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2} ')), /* Ok */Block.__(0, [/* Some */[2]]));

eq('File "js_json_test.ml", line 635, characters 5-12', Js_json_decode.optional(function (param) {
          return Js_json_decode.field("x", Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2.3} ')), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 637, characters 5-12', Js_json_decode.optional(function (param) {
          return Js_json_decode.field("y", Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2} ')), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 639, characters 5-12', Js_json_decode.field("x", function (param) {
          return Js_json_decode.optional(Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2} ')), /* Ok */Block.__(0, [/* Some */[2]]));

eq('File "js_json_test.ml", line 641, characters 5-12', Js_json_decode.field("x", function (param) {
          return Js_json_decode.optional(Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2.3} ')), /* Ok */Block.__(0, [/* None */0]));

eq('File "js_json_test.ml", line 643, characters 5-12', Js_json_decode.field("y", function (param) {
          return Js_json_decode.optional(Js_json_decode.$$int, param);
        }, JSON.parse(' { "x": 2} ')), /* Error */Block.__(1, ["Expected field 'y'"]));

eq('File "js_json_test.ml", line 650, characters 5-12', Js_json_decode.dict(function (param) {
          return Js_json_decode.array(function (param) {
                      return Js_json_decode.array(Js_json_decode.$$int, param);
                    }, param);
        }, JSON.parse(' { "a": [[1, 2], [3]], "b": [[4], [5, 6]] } ')), /* Ok */Block.__(0, [{
          a: /* array */[
            /* int array */[
              1,
              2
            ],
            /* int array */[3]
          ],
          b: /* array */[
            /* int array */[4],
            /* int array */[
              5,
              6
            ]
          ]
        }]));

eq('File "js_json_test.ml", line 653, characters 5-12', Js_json_decode.dict(function (param) {
          return Js_json_decode.array(function (param) {
                      return Js_json_decode.array(Js_json_decode.$$int, param);
                    }, param);
        }, JSON.parse(' { "a": [[1, 2], [true]], "b": [[4], [5, 6]] } ')), /* Error */Block.__(1, ["Expected number, got true"]));

eq('File "js_json_test.ml", line 656, characters 5-12', Js_json_decode.dict(function (param) {
          return Js_json_decode.array(function (param) {
                      return Js_json_decode.array(Js_json_decode.$$int, param);
                    }, param);
        }, JSON.parse(' { "a": [[1, 2], "foo"], "b": [[4], [5, 6]] } ')), /* Error */Block.__(1, ['Expected array, got "foo"']));

var json$11 = JSON.parse(' { "foo": [1, 2, 3], "bar": "baz" } ');

eq('File "js_json_test.ml", line 659, characters 5-12', /* tuple */[
      Js_json_decode.field("foo", function (param) {
            return Js_json_decode.array(Js_json_decode.$$int, param);
          }, json$11),
      Js_json_decode.field("bar", Js_json_decode.string, json$11)
    ], /* tuple */[
      /* Ok */Block.__(0, [/* int array */[
            1,
            2,
            3
          ]]),
      /* Ok */Block.__(0, ["baz"])
    ]);

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites     = suites;
exports.add_test   = add_test;
exports.eq         = eq;
exports.throws     = throws;
exports.false_     = false_;
exports.true_      = true_;
exports.option_get = option_get;
exports.eq_at_i    = eq_at_i;
/*  Not a pure module */
