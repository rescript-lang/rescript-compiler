'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Js_json = require("../../lib/js/js_json.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Js_boolean = require("../../lib/js/js_boolean.js");
var Js_primitive = require("../../lib/js/js_primitive.js");
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
        var ty = Js_json.classify(v);
        if (typeof ty === "number") {
          return /* Ok */Block.__(4, [/* false */0]);
        } else if (ty.tag === 2) {
          var match = ty[0]["x"];
          if (match !== undefined) {
            var ty2 = Js_json.classify(match);
            if (typeof ty2 === "number") {
              return /* Ok */Block.__(4, [/* false */0]);
            } else if (ty2.tag === 3) {
              ty2[0].forEach((function (x) {
                      var ty3 = Js_json.classify(x);
                      if (typeof ty3 === "number") {
                        throw [
                              Caml_builtin_exceptions.assert_failure,
                              [
                                "js_json_test.ml",
                                37,
                                21
                              ]
                            ];
                      } else if (ty3.tag === 1) {
                        return /* () */0;
                      } else {
                        throw [
                              Caml_builtin_exceptions.assert_failure,
                              [
                                "js_json_test.ml",
                                37,
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

eq("File \"js_json_test.ml\", line 48, characters 5-12", Js_json.test(v, /* Object */2), /* true */1);

var json = JSON.parse(JSON.stringify(null));

var ty = Js_json.classify(json);

if (typeof ty === "number") {
  if (ty >= 2) {
    add_test("File \"js_json_test.ml\", line 54, characters 30-37", (function () {
            return /* Ok */Block.__(4, [/* true */1]);
          }));
  } else {
    console.log(ty);
    add_test("File \"js_json_test.ml\", line 55, characters 27-34", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  }
} else {
  console.log(ty);
  add_test("File \"js_json_test.ml\", line 55, characters 27-34", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var ty$1 = Js_json.classify(json$1);

if (typeof ty$1 === "number") {
  add_test("File \"js_json_test.ml\", line 65, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (ty$1.tag) {
  add_test("File \"js_json_test.ml\", line 65, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else {
  eq("File \"js_json_test.ml\", line 64, characters 31-38", ty$1[0], "test string");
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var ty$2 = Js_json.classify(json$2);

var exit = 0;

if (typeof ty$2 === "number" || ty$2.tag !== 1) {
  exit = 1;
} else {
  eq("File \"js_json_test.ml\", line 74, characters 31-38", ty$2[0], 1.23456789);
}

if (exit === 1) {
  add_test("File \"js_json_test.ml\", line 75, characters 18-25", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

var json$3 = JSON.parse(JSON.stringify(-1347440721));

var ty$3 = Js_json.classify(json$3);

var exit$1 = 0;

if (typeof ty$3 === "number" || ty$3.tag !== 1) {
  exit$1 = 1;
} else {
  eq("File \"js_json_test.ml\", line 84, characters 31-38", ty$3[0] | 0, -1347440721);
}

if (exit$1 === 1) {
  add_test("File \"js_json_test.ml\", line 85, characters 18-25", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v));
  var ty = Js_json.classify(json);
  if (typeof ty === "number") {
    switch (ty) {
      case 0 : 
          return eq("File \"js_json_test.ml\", line 95, characters 31-38", false, v);
      case 1 : 
          return eq("File \"js_json_test.ml\", line 94, characters 30-37", true, v);
      case 2 : 
          return add_test("File \"js_json_test.ml\", line 96, characters 18-25", (function () {
                        return /* Ok */Block.__(4, [/* false */0]);
                      }));
      
    }
  } else {
    return add_test("File \"js_json_test.ml\", line 96, characters 18-25", (function () {
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
            102,
            36
          ]
        ];
  }
}

var dict = { };

dict["a"] = "test string";

dict["b"] = 123.0;

var json$4 = JSON.parse(JSON.stringify(dict));

var ty$4 = Js_json.classify(json$4);

if (typeof ty$4 === "number") {
  add_test("File \"js_json_test.ml\", line 134, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (ty$4.tag === 2) {
  var x = ty$4[0];
  var ta = Js_json.classify(option_get(Js_primitive.undefined_to_opt(x["a"])));
  if (typeof ta === "number") {
    add_test("File \"js_json_test.ml\", line 132, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (ta.tag) {
    add_test("File \"js_json_test.ml\", line 132, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (ta[0] !== "test string") {
    add_test("File \"js_json_test.ml\", line 123, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else {
    var ty$5 = Js_json.classify(option_get(Js_primitive.undefined_to_opt(x["b"])));
    if (typeof ty$5 === "number") {
      add_test("File \"js_json_test.ml\", line 130, characters 22-29", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else if (ty$5.tag === 1) {
      var b = ty$5[0];
      add_test("File \"js_json_test.ml\", line 129, characters 19-26", (function () {
              return /* Approx */Block.__(5, [
                        123.0,
                        b
                      ]);
            }));
    } else {
      add_test("File \"js_json_test.ml\", line 130, characters 22-29", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    }
  }
} else {
  add_test("File \"js_json_test.ml\", line 134, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

function eq_at_i(loc, json, i, kind, expected) {
  var ty = Js_json.classify(json);
  if (typeof ty === "number") {
    return add_test(loc, (function () {
                  return /* Ok */Block.__(4, [/* false */0]);
                }));
  } else if (ty.tag === 3) {
    var ty$1 = Js_json.classify(Caml_array.caml_array_get(ty[0], i));
    switch (kind) {
      case 0 : 
          if (typeof ty$1 === "number") {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          } else if (ty$1.tag) {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          } else {
            return eq(loc, ty$1[0], expected);
          }
          break;
      case 1 : 
          if (typeof ty$1 === "number") {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          } else if (ty$1.tag === 1) {
            return eq(loc, ty$1[0], expected);
          } else {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
          break;
      case 2 : 
          if (typeof ty$1 === "number") {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          } else if (ty$1.tag === 2) {
            return eq(loc, ty$1[0], expected);
          } else {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
          break;
      case 3 : 
          if (typeof ty$1 === "number") {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          } else if (ty$1.tag === 3) {
            return eq(loc, ty$1[0], expected);
          } else {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
          break;
      case 4 : 
          if (typeof ty$1 === "number") {
            switch (ty$1) {
              case 0 : 
                  return eq(loc, false, expected);
              case 1 : 
                  return eq(loc, true, expected);
              case 2 : 
                  return add_test(loc, (function () {
                                return /* Ok */Block.__(4, [/* false */0]);
                              }));
              
            }
          } else {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
          break;
      case 5 : 
          if (typeof ty$1 === "number") {
            if (ty$1 >= 2) {
              return add_test(loc, (function () {
                            return /* Ok */Block.__(4, [/* true */1]);
                          }));
            } else {
              return add_test(loc, (function () {
                            return /* Ok */Block.__(4, [/* false */0]);
                          }));
            }
          } else {
            return add_test(loc, (function () {
                          return /* Ok */Block.__(4, [/* false */0]);
                        }));
          }
      
    }
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

eq_at_i("File \"js_json_test.ml\", line 193, characters 10-17", json$5, 0, /* String */0, "string 0");

eq_at_i("File \"js_json_test.ml\", line 194, characters 10-17", json$5, 1, /* String */0, "string 1");

eq_at_i("File \"js_json_test.ml\", line 195, characters 10-17", json$5, 2, /* String */0, "string 2");

var json$6 = JSON.parse(JSON.stringify(/* array */[
          "string 0",
          "string 1",
          "string 2"
        ]));

eq_at_i("File \"js_json_test.ml\", line 205, characters 10-17", json$6, 0, /* String */0, "string 0");

eq_at_i("File \"js_json_test.ml\", line 206, characters 10-17", json$6, 1, /* String */0, "string 1");

eq_at_i("File \"js_json_test.ml\", line 207, characters 10-17", json$6, 2, /* String */0, "string 2");

var a = /* float array */[
  1.0000001,
  10000000000.1,
  123.0
];

var json$7 = JSON.parse(JSON.stringify(a));

eq_at_i("File \"js_json_test.ml\", line 219, characters 10-17", json$7, 0, /* Number */1, Caml_array.caml_array_get(a, 0));

eq_at_i("File \"js_json_test.ml\", line 220, characters 10-17", json$7, 1, /* Number */1, Caml_array.caml_array_get(a, 1));

eq_at_i("File \"js_json_test.ml\", line 221, characters 10-17", json$7, 2, /* Number */1, Caml_array.caml_array_get(a, 2));

var a$1 = /* int array */[
  0,
  -1347440721,
  -268391749
];

var json$8 = JSON.parse(JSON.stringify($$Array.map((function (prim) {
                return prim;
              }), a$1)));

eq_at_i("File \"js_json_test.ml\", line 234, characters 10-17", json$8, 0, /* Number */1, Caml_array.caml_array_get(a$1, 0));

eq_at_i("File \"js_json_test.ml\", line 235, characters 10-17", json$8, 1, /* Number */1, Caml_array.caml_array_get(a$1, 1));

eq_at_i("File \"js_json_test.ml\", line 236, characters 10-17", json$8, 2, /* Number */1, Caml_array.caml_array_get(a$1, 2));

var a$2 = /* int array */[
  /* true */1,
  /* false */0,
  /* true */1
];

var json$9 = JSON.parse(JSON.stringify($$Array.map(Js_boolean.to_js_boolean, a$2)));

var b$1 = Caml_array.caml_array_get(a$2, 0);

eq_at_i("File \"js_json_test.ml\", line 249, characters 10-17", json$9, 0, /* Boolean */4, b$1 ? true : false);

var b$2 = Caml_array.caml_array_get(a$2, 1);

eq_at_i("File \"js_json_test.ml\", line 250, characters 10-17", json$9, 1, /* Boolean */4, b$2 ? true : false);

var b$3 = Caml_array.caml_array_get(a$2, 2);

eq_at_i("File \"js_json_test.ml\", line 251, characters 10-17", json$9, 2, /* Boolean */4, b$3 ? true : false);

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

var ty$6 = Js_json.classify(json$10);

if (typeof ty$6 === "number") {
  add_test("File \"js_json_test.ml\", line 283, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
} else if (ty$6.tag === 3) {
  var ty$7 = Js_json.classify(Caml_array.caml_array_get(ty$6[0], 1));
  if (typeof ty$7 === "number") {
    add_test("File \"js_json_test.ml\", line 281, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  } else if (ty$7.tag === 2) {
    var ty$8 = Js_json.classify(option_get(Js_primitive.undefined_to_opt(ty$7[0]["a"])));
    if (typeof ty$8 === "number") {
      add_test("File \"js_json_test.ml\", line 279, characters 20-27", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else if (ty$8.tag) {
      add_test("File \"js_json_test.ml\", line 279, characters 20-27", (function () {
              return /* Ok */Block.__(4, [/* false */0]);
            }));
    } else {
      eq("File \"js_json_test.ml\", line 278, characters 40-47", ty$8[0], "bbb");
    }
  } else {
    add_test("File \"js_json_test.ml\", line 281, characters 18-25", (function () {
            return /* Ok */Block.__(4, [/* false */0]);
          }));
  }
} else {
  add_test("File \"js_json_test.ml\", line 283, characters 16-23", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}

try {
  JSON.parse("{{ A}");
  add_test("File \"js_json_test.ml\", line 289, characters 11-18", (function () {
          return /* Ok */Block.__(4, [/* false */0]);
        }));
}
catch (exn){
  add_test("File \"js_json_test.ml\", line 292, characters 10-17", (function () {
          return /* Ok */Block.__(4, [/* true */1]);
        }));
}

eq("File \"js_json_test.ml\", line 296, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(/* int array */[
              1,
              2,
              3
            ])), /* Some */["[1,2,3]"]);

eq("File \"js_json_test.ml\", line 300, characters 2-9", Js_primitive.undefined_to_opt(JSON.stringify({
              foo: 1,
              bar: "hello",
              baz: {
                baaz: 10
              }
            })), /* Some */["{\"foo\":1,\"bar\":\"hello\",\"baz\":{\"baaz\":10}}"]);

eq("File \"js_json_test.ml\", line 304, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(null)), /* Some */["null"]);

eq("File \"js_json_test.ml\", line 306, characters 12-19", Js_primitive.undefined_to_opt(JSON.stringify(undefined)), /* None */0);

eq("File \"js_json_test.ml\", line 309, characters 5-12", Js_json.decodeString("test"), /* Some */["test"]);

eq("File \"js_json_test.ml\", line 311, characters 5-12", Js_json.decodeString(true), /* None */0);

eq("File \"js_json_test.ml\", line 313, characters 5-12", Js_json.decodeString(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 315, characters 5-12", Js_json.decodeString(null), /* None */0);

eq("File \"js_json_test.ml\", line 317, characters 5-12", Js_json.decodeString({ }), /* None */0);

eq("File \"js_json_test.ml\", line 319, characters 5-12", Js_json.decodeString(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 323, characters 5-12", Js_json.decodeNumber("test"), /* None */0);

eq("File \"js_json_test.ml\", line 325, characters 5-12", Js_json.decodeNumber(true), /* None */0);

eq("File \"js_json_test.ml\", line 327, characters 5-12", Js_json.decodeNumber(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 329, characters 5-12", Js_json.decodeNumber(null), /* None */0);

eq("File \"js_json_test.ml\", line 331, characters 5-12", Js_json.decodeNumber({ }), /* None */0);

eq("File \"js_json_test.ml\", line 333, characters 5-12", Js_json.decodeNumber(1.23), /* Some */[1.23]);

eq("File \"js_json_test.ml\", line 337, characters 5-12", Js_json.decodeObject("test"), /* None */0);

eq("File \"js_json_test.ml\", line 339, characters 5-12", Js_json.decodeObject(true), /* None */0);

eq("File \"js_json_test.ml\", line 341, characters 5-12", Js_json.decodeObject(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 343, characters 5-12", Js_json.decodeObject(null), /* None */0);

eq("File \"js_json_test.ml\", line 345, characters 5-12", Js_json.decodeObject({ }), /* Some */[{ }]);

eq("File \"js_json_test.ml\", line 348, characters 5-12", Js_json.decodeObject(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 352, characters 5-12", Js_json.decodeArray("test"), /* None */0);

eq("File \"js_json_test.ml\", line 354, characters 5-12", Js_json.decodeArray(true), /* None */0);

eq("File \"js_json_test.ml\", line 356, characters 5-12", Js_json.decodeArray(/* array */[]), /* Some */[/* array */[]]);

eq("File \"js_json_test.ml\", line 358, characters 5-12", Js_json.decodeArray(null), /* None */0);

eq("File \"js_json_test.ml\", line 360, characters 5-12", Js_json.decodeArray({ }), /* None */0);

eq("File \"js_json_test.ml\", line 362, characters 5-12", Js_json.decodeArray(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 366, characters 5-12", Js_json.decodeBoolean("test"), /* None */0);

eq("File \"js_json_test.ml\", line 368, characters 5-12", Js_json.decodeBoolean(true), /* Some */[true]);

eq("File \"js_json_test.ml\", line 370, characters 5-12", Js_json.decodeBoolean(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 372, characters 5-12", Js_json.decodeBoolean(null), /* None */0);

eq("File \"js_json_test.ml\", line 374, characters 5-12", Js_json.decodeBoolean({ }), /* None */0);

eq("File \"js_json_test.ml\", line 376, characters 5-12", Js_json.decodeBoolean(1.23), /* None */0);

eq("File \"js_json_test.ml\", line 380, characters 5-12", Js_json.decodeNull("test"), /* None */0);

eq("File \"js_json_test.ml\", line 382, characters 5-12", Js_json.decodeNull(true), /* None */0);

eq("File \"js_json_test.ml\", line 384, characters 5-12", Js_json.decodeNull(/* array */[]), /* None */0);

eq("File \"js_json_test.ml\", line 386, characters 5-12", Js_json.decodeNull(null), /* Some */[null]);

eq("File \"js_json_test.ml\", line 388, characters 5-12", Js_json.decodeNull({ }), /* None */0);

eq("File \"js_json_test.ml\", line 390, characters 5-12", Js_json.decodeNull(1.23), /* None */0);

Mt.from_pair_suites("js_json_test.ml", suites[0]);

exports.suites = suites;
exports.add_test = add_test;
exports.eq = eq;
exports.false_ = false_;
exports.true_ = true_;
exports.option_get = option_get;
exports.eq_at_i = eq_at_i;
/* v Not a pure module */
