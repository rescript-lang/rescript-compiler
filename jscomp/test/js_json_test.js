'use strict';

var Mt                      = require("./mt.js");
var $$Array                 = require("../../lib/js/array.js");
var Block                   = require("../../lib/js/block.js");
var Js_json                 = require("../../lib/js/js_json.js");
var Caml_obj                = require("../../lib/js/caml_obj.js");
var Caml_array              = require("../../lib/js/caml_array.js");
var Js_boolean              = require("../../lib/js/js_boolean.js");
var Js_primitive            = require("../../lib/js/js_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

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
  return add_test(loc, (function () {
                return /* Eq */Block.__(0, [
                          x,
                          y
                        ]);
              }));
}

function false_(loc) {
  return add_test(loc, (function () {
                return /* Ok */Block.__(4, [/* false */0]);
              }));
}

function true_(loc) {
  return add_test(loc, (function () {
                return /* Ok */Block.__(4, [/* true */1]);
              }));
}

var v = JSON.parse(" { \"x\" : [1, 2, 3 ] } ");

add_test("File \"js_json_test.ml\", line 23, characters 11-18", (function () {
        var match = Js_json.classify(v);
        if (typeof match === "number") {
          return /* Ok */Block.__(4, [/* false */0]);
        } else if (match.tag === 2) {
          var match$1 = match[0]["x"];
          if (match$1 !== undefined) {
            var match$2 = Js_json.classify(match$1);
            if (typeof match$2 === "number") {
              return /* Ok */Block.__(4, [/* false */0]);
            } else if (match$2.tag === 3) {
              match$2[0].forEach((function (x) {
                      var match = Js_json.classify(x);
                      if (typeof match === "number") {
                        throw [
                              Caml_builtin_exceptions.assert_failure,
                              [
                                "js_json_test.ml",
                                33,
                                21
                              ]
                            ];
                      } else if (match.tag === 1) {
                        return /* () */0;
                      } else {
                        throw [
                              Caml_builtin_exceptions.assert_failure,
                              [
                                "js_json_test.ml",
                                33,
                                21
                              ]
                            ];
                      }
                    }));
              return /* Ok */Block.__(4, [/* true */1]);
            } else {
              return /* Ok */Block.__(4, [/* false */0]);
            }
          } else {
            return /* Ok */Block.__(4, [/* false */0]);
          }
        } else {
          return /* Ok */Block.__(4, [/* false */0]);
        }
      }));

eq("File \"js_json_test.ml\", line 44, characters 5-12", Js_json.test(v, /* Object */2), /* true */1);

var json = JSON.parse(JSON.stringify(null));

var x = Js_json.classify(json);

if (typeof x === "number") {
  if (x >= 2) {
    add_test("File \"js_json_test.ml\", line 49, characters 22-29", (function () {
            return /* Ok */Block.__(4, [/* true */1]);
          }));
  } else {
    console.log(x);
    add_test("File \"js_json_test.ml\", line 50, characters 26-33", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  }
} else {
  console.log(x);
  add_test("File \"js_json_test.ml\", line 50, characters 26-33", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var match = Js_json.classify(json$1);

if (typeof match === "number") {
  add_test("File \"js_json_test.ml\", line 59, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (match.tag) {
  add_test("File \"js_json_test.ml\", line 59, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else {
  eq("File \"js_json_test.ml\", line 58, characters 23-30", match[0], "test string");
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var match$1 = Js_json.classify(json$2);

var exit = 0;

if (typeof match$1 === "number" || match$1.tag !== 1) {
  exit = 1;
} else {
  eq("File \"js_json_test.ml\", line 67, characters 23-30", match$1[0], 1.23456789);
}

if (exit === 1) {
  add_test("File \"js_json_test.ml\", line 68, characters 18-25", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

var json$3 = JSON.parse(JSON.stringify(-1347440721));

var match$2 = Js_json.classify(json$3);

var exit$1 = 0;

if (typeof match$2 === "number" || match$2.tag !== 1) {
  exit$1 = 1;
} else {
  eq("File \"js_json_test.ml\", line 76, characters 23-30", match$2[0] | 0, -1347440721);
}

if (exit$1 === 1) {
  add_test("File \"js_json_test.ml\", line 77, characters 18-25", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v));
  var match = Js_json.classify(json);
  if (typeof match === "number") {
    switch (match) {
      case 0 : 
          if (Caml_obj.caml_equal(v, false)) {
            return add_test("File \"js_json_test.ml\", line 86, characters 44-51", (function () {
                          return /* Ok */Block.__(4, [/* true */1]);
                        }));
          } else {
            return add_test("File \"js_json_test.ml\", line 87, characters 18-25", (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
      case 1 : 
          if (Caml_obj.caml_equal(v, true)) {
            return add_test("File \"js_json_test.ml\", line 85, characters 42-49", (function () {
                          return /* Ok */Block.__(4, [/* true */1]);
                        }));
          } else {
            return add_test("File \"js_json_test.ml\", line 87, characters 18-25", (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
      case 2 : 
          return add_test("File \"js_json_test.ml\", line 87, characters 18-25", (function () {
                        return /* Ok */Block.__(4, [/* false */0]);
                      }));
      
    }
  } else {
    return add_test("File \"js_json_test.ml\", line 87, characters 18-25", (function () {
                  return /* Ok */Block.__(4, [/* false */0]);
                }));
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
            93,
            36
          ]
        ];
  }
}

var dict = { };

dict["a"] = "test string";

dict["b"] = 123.0;

var json$4 = JSON.parse(JSON.stringify(dict));

var match$3 = Js_json.classify(json$4);

if (typeof match$3 === "number") {
  add_test("File \"js_json_test.ml\", line 122, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (match$3.tag === 2) {
  var x$1 = match$3[0];
  var match$4 = Js_json.classify(option_get(Js_primitive.undefined_to_opt(x$1["a"])));
  if (typeof match$4 === "number") {
    add_test("File \"js_json_test.ml\", line 120, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (match$4.tag) {
    add_test("File \"js_json_test.ml\", line 120, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (match$4[0] !== "test string") {
    add_test("File \"js_json_test.ml\", line 112, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else {
    var match$5 = Js_json.classify(option_get(Js_primitive.undefined_to_opt(x$1["b"])));
    if (typeof match$5 === "number") {
      add_test("File \"js_json_test.ml\", line 118, characters 22-29", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else if (match$5.tag === 1) {
      var b = match$5[0];
      add_test("File \"js_json_test.ml\", line 117, characters 19-26", (function () {
              return /* Approx */Block.__(5, [
                        123.0,
                        b
                      ]);
            }));
    } else {
      add_test("File \"js_json_test.ml\", line 118, characters 22-29", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    }
  }
} else {
  add_test("File \"js_json_test.ml\", line 122, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

function eq_at_i(loc, json, i, expected) {
  var match = Js_json.classify(json);
  if (typeof match === "number") {
    return add_test(loc, (function () {
                  return /* Ok */Block.__(4, [/* false */0]);
                }));
  } else if (match.tag === 3) {
    return eq(loc, Js_json.classify(Caml_array.caml_array_get(match[0], i)), expected);
  } else {
    return add_test(loc, (function () {
                  return /* Ok */Block.__(4, [/* false */0]);
                }));
  }
}

var json$5 = JSON.parse(JSON.stringify($$Array.map((function (prim) {
                return prim;
              }), /* array */[
              "string 0",
              "string 1",
              "string 2"
            ])));

eq_at_i("File \"js_json_test.ml\", line 145, characters 10-17", json$5, 0, /* JSONString */Block.__(0, ["string 0"]));

eq_at_i("File \"js_json_test.ml\", line 146, characters 10-17", json$5, 1, /* JSONString */Block.__(0, ["string 1"]));

eq_at_i("File \"js_json_test.ml\", line 147, characters 10-17", json$5, 2, /* JSONString */Block.__(0, ["string 2"]));

var json$6 = JSON.parse(JSON.stringify(/* array */[
          "string 0",
          "string 1",
          "string 2"
        ]));

eq_at_i("File \"js_json_test.ml\", line 157, characters 10-17", json$6, 0, /* JSONString */Block.__(0, ["string 0"]));

eq_at_i("File \"js_json_test.ml\", line 158, characters 10-17", json$6, 1, /* JSONString */Block.__(0, ["string 1"]));

eq_at_i("File \"js_json_test.ml\", line 159, characters 10-17", json$6, 2, /* JSONString */Block.__(0, ["string 2"]));

var a = /* float array */[
  1.0000001,
  10000000000.1,
  123.0
];

var json$7 = JSON.parse(JSON.stringify(a));

eq_at_i("File \"js_json_test.ml\", line 171, characters 10-17", json$7, 0, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a, 0)]));

eq_at_i("File \"js_json_test.ml\", line 172, characters 10-17", json$7, 1, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a, 1)]));

eq_at_i("File \"js_json_test.ml\", line 173, characters 10-17", json$7, 2, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a, 2)]));

var a$1 = /* int array */[
  0,
  -1347440721,
  -268391749
];

var json$8 = JSON.parse(JSON.stringify($$Array.map((function (prim) {
                return prim;
              }), a$1)));

eq_at_i("File \"js_json_test.ml\", line 186, characters 10-17", json$8, 0, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a$1, 0)]));

eq_at_i("File \"js_json_test.ml\", line 187, characters 10-17", json$8, 1, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a$1, 1)]));

eq_at_i("File \"js_json_test.ml\", line 188, characters 10-17", json$8, 2, /* JSONNumber */Block.__(1, [Caml_array.caml_array_get(a$1, 2)]));

var a$2 = /* int array */[
  /* true */1,
  /* false */0,
  /* true */1
];

var json$9 = JSON.parse(JSON.stringify($$Array.map(Js_boolean.to_js_boolean, a$2)));

var a$prime = a$2.map((function (x) {
        if (x) {
          return /* JSONTrue */1;
        } else {
          return /* JSONFalse */0;
        }
      }));

eq_at_i("File \"js_json_test.ml\", line 202, characters 10-17", json$9, 0, Caml_array.caml_array_get(a$prime, 0));

eq_at_i("File \"js_json_test.ml\", line 203, characters 10-17", json$9, 1, Caml_array.caml_array_get(a$prime, 1));

eq_at_i("File \"js_json_test.ml\", line 204, characters 10-17", json$9, 2, Caml_array.caml_array_get(a$prime, 2));

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

var match$6 = Js_json.classify(json$10);

if (typeof match$6 === "number") {
  add_test("File \"js_json_test.ml\", line 233, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (match$6.tag === 3) {
  var match$7 = Js_json.classify(Caml_array.caml_array_get(match$6[0], 1));
  if (typeof match$7 === "number") {
    add_test("File \"js_json_test.ml\", line 231, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (match$7.tag === 2) {
    var match$8 = Js_json.classify(option_get(Js_primitive.undefined_to_opt(match$7[0]["a"])));
    if (typeof match$8 === "number") {
      add_test("File \"js_json_test.ml\", line 229, characters 20-27", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else if (match$8.tag) {
      add_test("File \"js_json_test.ml\", line 229, characters 20-27", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else {
      eq("File \"js_json_test.ml\", line 228, characters 32-39", match$8[0], "bbb");
    }
  } else {
    add_test("File \"js_json_test.ml\", line 231, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  }
} else {
  add_test("File \"js_json_test.ml\", line 233, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

try {
  JSON.parse("{{ A}");
  add_test("File \"js_json_test.ml\", line 239, characters 11-18", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}
catch (exn){
  add_test("File \"js_json_test.ml\", line 242, characters 10-17", (function () {
          return /* Ok */Block.__(4, [/* true */1]);
        }));
}

eq("File \"js_json_test.ml\", line 246, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(/* int array */[
              1,
              2,
              3
            ])), /* Some */["[1,2,3]"]);

eq("File \"js_json_test.ml\", line 250, characters 2-9", Js_primitive.undefined_to_opt(JSON.stringify({
              foo: 1,
              bar: "hello",
              baz: {
                baaz: 10
              }
            })), /* Some */["{\"foo\":1,\"bar\":\"hello\",\"baz\":{\"baaz\":10}}"]);

eq("File \"js_json_test.ml\", line 254, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(null)), /* Some */["null"]);

eq("File \"js_json_test.ml\", line 256, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(undefined)), /* None */0);

eq("File \"js_json_test.ml\", line 259, characters 5-12", Js_json.decodeString("test"), /* Some */["test"]);

eq("File \"js_json_test.ml\", line 261, characters 5-12", Js_json.decodeString(true), /* None */0);

eq("File \"js_json_test.ml\", line 263, characters 5-12", Js_json.decodeString(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 265, characters 5-12", Js_json.decodeString(null), /* None */0);

eq("File \"js_json_test.ml\", line 267, characters 5-12", Js_json.decodeString({ }), /* None */0);

eq("File \"js_json_test.ml\", line 269, characters 5-12", Js_json.decodeString(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 273, characters 5-12", Js_json.decodeNumber("test"), /* None */0);

eq("File \"js_json_test.ml\", line 275, characters 5-12", Js_json.decodeNumber(true), /* None */0);

eq("File \"js_json_test.ml\", line 277, characters 5-12", Js_json.decodeNumber(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 279, characters 5-12", Js_json.decodeNumber(null), /* None */0);

eq("File \"js_json_test.ml\", line 281, characters 5-12", Js_json.decodeNumber({ }), /* None */0);

eq("File \"js_json_test.ml\", line 283, characters 5-12", Js_json.decodeNumber(1.23), /* Some */[1.23]);

eq("File \"js_json_test.ml\", line 287, characters 5-12", Js_json.decodeObject("test"), /* None */0);

eq("File \"js_json_test.ml\", line 289, characters 5-12", Js_json.decodeObject(true), /* None */0);

eq("File \"js_json_test.ml\", line 291, characters 5-12", Js_json.decodeObject(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 293, characters 5-12", Js_json.decodeObject(null), /* None */0);

eq("File \"js_json_test.ml\", line 295, characters 5-12", Js_json.decodeObject({ }), /* Some */[{ }]);

eq("File \"js_json_test.ml\", line 298, characters 5-12", Js_json.decodeObject(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 302, characters 5-12", Js_json.decodeArray("test"), /* None */0);

eq("File \"js_json_test.ml\", line 304, characters 5-12", Js_json.decodeArray(true), /* None */0);

eq("File \"js_json_test.ml\", line 306, characters 5-12", Js_json.decodeArray(/* array */[]), /* Some */[/* array */[]]);

eq("File \"js_json_test.ml\", line 308, characters 5-12", Js_json.decodeArray(null), /* None */0);

eq("File \"js_json_test.ml\", line 310, characters 5-12", Js_json.decodeArray({ }), /* None */0);

eq("File \"js_json_test.ml\", line 312, characters 5-12", Js_json.decodeArray(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 316, characters 5-12", Js_json.decodeBoolean("test"), /* None */0);

eq("File \"js_json_test.ml\", line 318, characters 5-12", Js_json.decodeBoolean(true), /* Some */[true]);

eq("File \"js_json_test.ml\", line 320, characters 5-12", Js_json.decodeBoolean(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 322, characters 5-12", Js_json.decodeBoolean(null), /* None */0);

eq("File \"js_json_test.ml\", line 324, characters 5-12", Js_json.decodeBoolean({ }), /* None */0);

eq("File \"js_json_test.ml\", line 326, characters 5-12", Js_json.decodeBoolean(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 330, characters 5-12", Js_json.decodeNull("test"), /* None */0);

eq("File \"js_json_test.ml\", line 332, characters 5-12", Js_json.decodeNull(true), /* None */0);

eq("File \"js_json_test.ml\", line 334, characters 5-12", Js_json.decodeNull(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 336, characters 5-12", Js_json.decodeNull(null), /* Some */[null]);

eq("File \"js_json_test.ml\", line 338, characters 5-12", Js_json.decodeNull({ }), /* None */0);

eq("File \"js_json_test.ml\", line 340, characters 5-12", Js_json.decodeNull(1.23), /* None */0);

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites     = suites;
exports.add_test   = add_test;
exports.eq         = eq;
exports.false_     = false_;
exports.true_      = true_;
exports.option_get = option_get;
exports.eq_at_i    = eq_at_i;
/* v Not a pure module */
