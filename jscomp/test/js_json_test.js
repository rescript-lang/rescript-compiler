'use strict';

var Mt                      = require("./mt");
var $$Array                 = require("../../lib/js/array");
var Block                   = require("../../lib/js/block");
var Js_json                 = require("../../lib/js/js_json");
var Caml_obj                = require("../../lib/js/caml_obj");
var Caml_array              = require("../../lib/js/caml_array");
var Js_boolean              = require("../../lib/js/js_boolean");
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
              return /* Ok */Block.__(4, [/* false */0]);
            });
}

function true_(loc) {
  return add_test(loc, function () {
              return /* Ok */Block.__(4, [/* true */1]);
            });
}

var v = JSON.parse(" { \"x\" : [1, 2, 3 ] } ");

add_test("File \"js_json_test.ml\", line 23, characters 11-18", function () {
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
                            37,
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

eq("File \"js_json_test.ml\", line 48, characters 5-12", Js_json.test(v, /* Object */2), /* true */1);

var json = JSON.parse(JSON.stringify(null));

var match = Js_json.reifyType(json);

if (match[0] >= 5) {
  add_test("File \"js_json_test.ml\", line 54, characters 26-33", function () {
        return /* Ok */Block.__(4, [/* true */1]);
      });
} else {
  console.log(match[1]);
  add_test("File \"js_json_test.ml\", line 55, characters 26-33", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var match$1 = Js_json.reifyType(json$1);

if (match$1[0] !== 0) {
  add_test("File \"js_json_test.ml\", line 65, characters 16-23", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq("File \"js_json_test.ml\", line 64, characters 25-32", match$1[1], "test string");
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var match$2 = Js_json.reifyType(json$2);

if (match$2[0] !== 1) {
  add_test("File \"js_json_test.ml\", line 75, characters 18-25", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq("File \"js_json_test.ml\", line 74, characters 25-32", match$2[1], 1.23456789);
}

var json$3 = JSON.parse(JSON.stringify(-1347440721));

var match$3 = Js_json.reifyType(json$3);

if (match$3[0] !== 1) {
  add_test("File \"js_json_test.ml\", line 85, characters 18-25", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  eq("File \"js_json_test.ml\", line 84, characters 25-32", match$3[1] | 0, -1347440721);
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v));
  var match = Js_json.reifyType(json);
  if (match[0] !== 4) {
    return add_test("File \"js_json_test.ml\", line 95, characters 18-25", function () {
                return /* Ok */Block.__(4, [/* false */0]);
              });
  } else {
    return eq("File \"js_json_test.ml\", line 94, characters 28-35", match[1], v);
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
            101,
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
  add_test("File \"js_json_test.ml\", line 133, characters 16-23", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  var match$5 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(x["a"])));
  if (match$5[0] !== 0) {
    add_test("File \"js_json_test.ml\", line 131, characters 18-25", function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else if (match$5[1] !== "test string") {
    add_test("File \"js_json_test.ml\", line 122, characters 18-25", function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else {
    var match$6 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(x["b"])));
    var b = match$6[1];
    if (match$6[0] !== 1) {
      add_test("File \"js_json_test.ml\", line 129, characters 22-29", function () {
            return /* Ok */Block.__(4, [/* false */0]);
          });
    } else {
      add_test("File \"js_json_test.ml\", line 128, characters 19-26", function () {
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

eq_at_i("File \"js_json_test.ml\", line 164, characters 10-17", json$5, 0, /* String */0, "string 0");

eq_at_i("File \"js_json_test.ml\", line 165, characters 10-17", json$5, 1, /* String */0, "string 1");

eq_at_i("File \"js_json_test.ml\", line 166, characters 10-17", json$5, 2, /* String */0, "string 2");

var json$6 = JSON.parse(JSON.stringify(/* array */[
          "string 0",
          "string 1",
          "string 2"
        ]));

eq_at_i("File \"js_json_test.ml\", line 176, characters 10-17", json$6, 0, /* String */0, "string 0");

eq_at_i("File \"js_json_test.ml\", line 177, characters 10-17", json$6, 1, /* String */0, "string 1");

eq_at_i("File \"js_json_test.ml\", line 178, characters 10-17", json$6, 2, /* String */0, "string 2");

var a = /* float array */[
  1.0000001,
  10000000000.1,
  123.0
];

var json$7 = JSON.parse(JSON.stringify(a));

eq_at_i("File \"js_json_test.ml\", line 190, characters 10-17", json$7, 0, /* Number */1, Caml_array.caml_array_get(a, 0));

eq_at_i("File \"js_json_test.ml\", line 191, characters 10-17", json$7, 1, /* Number */1, Caml_array.caml_array_get(a, 1));

eq_at_i("File \"js_json_test.ml\", line 192, characters 10-17", json$7, 2, /* Number */1, Caml_array.caml_array_get(a, 2));

var a$1 = /* int array */[
  0,
  -1347440721,
  -268391749
];

var json$8 = JSON.parse(JSON.stringify($$Array.map(function (prim) {
              return prim;
            }, a$1)));

eq_at_i("File \"js_json_test.ml\", line 205, characters 10-17", json$8, 0, /* Number */1, Caml_array.caml_array_get(a$1, 0));

eq_at_i("File \"js_json_test.ml\", line 206, characters 10-17", json$8, 1, /* Number */1, Caml_array.caml_array_get(a$1, 1));

eq_at_i("File \"js_json_test.ml\", line 207, characters 10-17", json$8, 2, /* Number */1, Caml_array.caml_array_get(a$1, 2));

var a$2 = /* int array */[
  /* true */1,
  /* false */0,
  /* true */1
];

var json$9 = JSON.parse(JSON.stringify($$Array.map(Js_boolean.to_js_boolean, a$2)));

var b$1 = Caml_array.caml_array_get(a$2, 0);

eq_at_i("File \"js_json_test.ml\", line 220, characters 10-17", json$9, 0, /* Boolean */4, b$1 ? true : false);

var b$2 = Caml_array.caml_array_get(a$2, 1);

eq_at_i("File \"js_json_test.ml\", line 221, characters 10-17", json$9, 1, /* Boolean */4, b$2 ? true : false);

var b$3 = Caml_array.caml_array_get(a$2, 2);

eq_at_i("File \"js_json_test.ml\", line 222, characters 10-17", json$9, 2, /* Boolean */4, b$3 ? true : false);

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
  add_test("File \"js_json_test.ml\", line 254, characters 16-23", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
} else {
  var match$8 = Js_json.reifyType(Caml_array.caml_array_get(match$7[1], 1));
  if (match$8[0] !== 2) {
    add_test("File \"js_json_test.ml\", line 252, characters 18-25", function () {
          return /* Ok */Block.__(4, [/* false */0]);
        });
  } else {
    var match$9 = Js_json.reifyType(option_get(Js_primitive.undefined_to_opt(match$8[1]["a"])));
    if (match$9[0] !== 0) {
      add_test("File \"js_json_test.ml\", line 250, characters 20-27", function () {
            return /* Ok */Block.__(4, [/* false */0]);
          });
    } else {
      eq("File \"js_json_test.ml\", line 249, characters 29-36", match$9[1], "bbb");
    }
  }
}

try {
  JSON.parse("{{ A}");
  add_test("File \"js_json_test.ml\", line 260, characters 11-18", function () {
        return /* Ok */Block.__(4, [/* false */0]);
      });
}
catch (exn){
  add_test("File \"js_json_test.ml\", line 263, characters 10-17", function () {
        return /* Ok */Block.__(4, [/* true */1]);
      });
}

eq("File \"js_json_test.ml\", line 267, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(/* int array */[
              1,
              2,
              3
            ])), /* Some */["[1,2,3]"]);

eq("File \"js_json_test.ml\", line 271, characters 2-9", Js_primitive.undefined_to_opt(JSON.stringify({
              foo: 1,
              bar: "hello",
              baz: {
                baaz: 10
              }
            })), /* Some */["{\"foo\":1,\"bar\":\"hello\",\"baz\":{\"baaz\":10}}"]);

eq("File \"js_json_test.ml\", line 275, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(null)), /* Some */["null"]);

eq("File \"js_json_test.ml\", line 277, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(undefined)), /* None */0);

eq("File \"js_json_test.ml\", line 280, characters 5-12", Js_json.decodeString("test"), /* Some */["test"]);

eq("File \"js_json_test.ml\", line 282, characters 5-12", Js_json.decodeString(true), /* None */0);

eq("File \"js_json_test.ml\", line 284, characters 5-12", Js_json.decodeString(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 286, characters 5-12", Js_json.decodeString(null), /* None */0);

eq("File \"js_json_test.ml\", line 288, characters 5-12", Js_json.decodeString({ }), /* None */0);

eq("File \"js_json_test.ml\", line 290, characters 5-12", Js_json.decodeString(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 294, characters 5-12", Js_json.decodeNumber("test"), /* None */0);

eq("File \"js_json_test.ml\", line 296, characters 5-12", Js_json.decodeNumber(true), /* None */0);

eq("File \"js_json_test.ml\", line 298, characters 5-12", Js_json.decodeNumber(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 300, characters 5-12", Js_json.decodeNumber(null), /* None */0);

eq("File \"js_json_test.ml\", line 302, characters 5-12", Js_json.decodeNumber({ }), /* None */0);

eq("File \"js_json_test.ml\", line 304, characters 5-12", Js_json.decodeNumber(1.23), /* Some */[1.23]);

eq("File \"js_json_test.ml\", line 308, characters 5-12", Js_json.decodeObject("test"), /* None */0);

eq("File \"js_json_test.ml\", line 310, characters 5-12", Js_json.decodeObject(true), /* None */0);

eq("File \"js_json_test.ml\", line 312, characters 5-12", Js_json.decodeObject(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 314, characters 5-12", Js_json.decodeObject(null), /* None */0);

eq("File \"js_json_test.ml\", line 316, characters 5-12", Js_json.decodeObject({ }), /* Some */[{ }]);

eq("File \"js_json_test.ml\", line 319, characters 5-12", Js_json.decodeObject(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 323, characters 5-12", Js_json.decodeArray("test"), /* None */0);

eq("File \"js_json_test.ml\", line 325, characters 5-12", Js_json.decodeArray(true), /* None */0);

eq("File \"js_json_test.ml\", line 327, characters 5-12", Js_json.decodeArray(/* array */[]), /* Some */[/* array */[]]);

eq("File \"js_json_test.ml\", line 329, characters 5-12", Js_json.decodeArray(null), /* None */0);

eq("File \"js_json_test.ml\", line 331, characters 5-12", Js_json.decodeArray({ }), /* None */0);

eq("File \"js_json_test.ml\", line 333, characters 5-12", Js_json.decodeArray(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 337, characters 5-12", Js_json.decodeBoolean("test"), /* None */0);

eq("File \"js_json_test.ml\", line 339, characters 5-12", Js_json.decodeBoolean(true), /* Some */[true]);

eq("File \"js_json_test.ml\", line 341, characters 5-12", Js_json.decodeBoolean(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 343, characters 5-12", Js_json.decodeBoolean(null), /* None */0);

eq("File \"js_json_test.ml\", line 345, characters 5-12", Js_json.decodeBoolean({ }), /* None */0);

eq("File \"js_json_test.ml\", line 347, characters 5-12", Js_json.decodeBoolean(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 351, characters 5-12", Js_json.decodeNull("test"), /* None */0);

eq("File \"js_json_test.ml\", line 353, characters 5-12", Js_json.decodeNull(true), /* None */0);

eq("File \"js_json_test.ml\", line 355, characters 5-12", Js_json.decodeNull(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 357, characters 5-12", Js_json.decodeNull(null), /* Some */[null]);

eq("File \"js_json_test.ml\", line 359, characters 5-12", Js_json.decodeNull({ }), /* None */0);

eq("File \"js_json_test.ml\", line 361, characters 5-12", Js_json.decodeNull(1.23), /* None */0);

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites     = suites;
exports.add_test   = add_test;
exports.eq         = eq;
exports.false_     = false_;
exports.true_      = true_;
exports.option_get = option_get;
exports.eq_at_i    = eq_at_i;
/* v Not a pure module */
