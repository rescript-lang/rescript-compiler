'use strict';

var Mt = require("./mt.js");
var $$Array = require("../../lib/js/array.js");
var Js_dict = require("../../lib/js/js_dict.js");
var Js_json = require("../../lib/js/js_json.js");
var Belt_List = require("../../lib/js/belt_List.js");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var counter = {
  contents: 0
};

function add_test(loc, test) {
  counter.contents = counter.contents + 1 | 0;
  var id = loc + (" id " + String(counter.contents));
  suites.contents = {
    hd: [
      id,
      test
    ],
    tl: suites.contents
  };
}

function eq(loc, x, y) {
  add_test(loc, (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        }));
}

function false_(loc) {
  add_test(loc, (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

function true_(loc) {
  add_test(loc, (function (param) {
          return {
                  TAG: "Ok",
                  _0: true
                };
        }));
}

var v = JSON.parse(" { \"x\" : [1, 2, 3 ] } ");

add_test("File \"js_json_test.res\", line 22, characters 11-18", (function (param) {
        var ty = Js_json.classify(v);
        if (typeof ty !== "object") {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }
        if (ty.TAG !== "JSONObject") {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }
        var v$1 = Js_dict.get(ty._0, "x");
        if (v$1 === undefined) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }
        var ty2 = Js_json.classify(Caml_option.valFromOption(v$1));
        if (typeof ty2 !== "object") {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }
        if (ty2.TAG !== "JSONArray") {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }
        ty2._0.forEach(function (x) {
              var ty3 = Js_json.classify(x);
              if (typeof ty3 !== "object") {
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "js_json_test.res",
                        37,
                        19
                      ],
                      Error: new Error()
                    };
              }
              if (ty3.TAG === "JSONNumber") {
                return ;
              }
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "js_json_test.res",
                      37,
                      19
                    ],
                    Error: new Error()
                  };
            });
        return {
                TAG: "Ok",
                _0: true
              };
      }));

eq("File \"js_json_test.res\", line 48, characters 5-12", Js_json.test(v, "Object"), true);

var json = JSON.parse(JSON.stringify(null));

var ty = Js_json.classify(json);

if (typeof ty !== "object") {
  if (ty === "JSONNull") {
    add_test("File \"js_json_test.res\", line 55, characters 24-31", (function (param) {
            return {
                    TAG: "Ok",
                    _0: true
                  };
          }));
  } else {
    console.log(ty);
    add_test("File \"js_json_test.res\", line 58, characters 11-18", (function (param) {
            return {
                    TAG: "Ok",
                    _0: false
                  };
          }));
  }
} else {
  console.log(ty);
  add_test("File \"js_json_test.res\", line 58, characters 11-18", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

var json$1 = JSON.parse(JSON.stringify("test string"));

var ty$1 = Js_json.classify(json$1);

if (typeof ty$1 !== "object") {
  add_test("File \"js_json_test.res\", line 68, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
} else if (ty$1.TAG === "JSONString") {
  eq("File \"js_json_test.res\", line 67, characters 26-33", ty$1._0, "test string");
} else {
  add_test("File \"js_json_test.res\", line 68, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

var json$2 = JSON.parse(JSON.stringify(1.23456789));

var ty$2 = Js_json.classify(json$2);

var exit = 0;

if (typeof ty$2 !== "object" || ty$2.TAG !== "JSONNumber") {
  exit = 1;
} else {
  eq("File \"js_json_test.res\", line 77, characters 26-33", ty$2._0, 1.23456789);
}

if (exit === 1) {
  add_test("File \"js_json_test.res\", line 78, characters 18-25", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

var json$3 = JSON.parse(JSON.stringify(-1347440721));

var ty$3 = Js_json.classify(json$3);

var exit$1 = 0;

if (typeof ty$3 !== "object" || ty$3.TAG !== "JSONNumber") {
  exit$1 = 1;
} else {
  eq("File \"js_json_test.res\", line 87, characters 26-33", ty$3._0 | 0, -1347440721);
}

if (exit$1 === 1) {
  add_test("File \"js_json_test.res\", line 88, characters 18-25", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

function test(v) {
  var json = JSON.parse(JSON.stringify(v));
  var ty = Js_json.classify(json);
  if (typeof ty === "object") {
    return add_test("File \"js_json_test.res\", line 100, characters 18-25", (function (param) {
                  return {
                          TAG: "Ok",
                          _0: false
                        };
                }));
  }
  switch (ty) {
    case "JSONFalse" :
        return eq("File \"js_json_test.res\", line 99, characters 24-31", false, v);
    case "JSONTrue" :
        return eq("File \"js_json_test.res\", line 98, characters 23-30", true, v);
    default:
      return add_test("File \"js_json_test.res\", line 100, characters 18-25", (function (param) {
                    return {
                            TAG: "Ok",
                            _0: false
                          };
                  }));
  }
}

test(true);

test(false);

function option_get(x) {
  if (x !== undefined) {
    return Caml_option.valFromOption(x);
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "js_json_test.res",
          111,
          12
        ],
        Error: new Error()
      };
}

var dict = {};

dict["a"] = "test string";

dict["b"] = 123.0;

var json$4 = JSON.parse(JSON.stringify(dict));

var ty$4 = Js_json.classify(json$4);

if (typeof ty$4 !== "object") {
  add_test("File \"js_json_test.res\", line 142, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
} else if (ty$4.TAG === "JSONObject") {
  var x = ty$4._0;
  var ta = Js_json.classify(option_get(Js_dict.get(x, "a")));
  if (typeof ta !== "object") {
    add_test("File \"js_json_test.res\", line 140, characters 18-25", (function (param) {
            return {
                    TAG: "Ok",
                    _0: false
                  };
          }));
  } else if (ta.TAG === "JSONString") {
    if (ta._0 !== "test string") {
      add_test("File \"js_json_test.res\", line 131, characters 15-22", (function (param) {
              return {
                      TAG: "Ok",
                      _0: false
                    };
            }));
    } else {
      var ty$5 = Js_json.classify(option_get(Js_dict.get(x, "b")));
      if (typeof ty$5 !== "object") {
        add_test("File \"js_json_test.res\", line 137, characters 22-29", (function (param) {
                return {
                        TAG: "Ok",
                        _0: false
                      };
              }));
      } else if (ty$5.TAG === "JSONNumber") {
        var b = ty$5._0;
        add_test("File \"js_json_test.res\", line 136, characters 38-45", (function (param) {
                return {
                        TAG: "Approx",
                        _0: 123.0,
                        _1: b
                      };
              }));
      } else {
        add_test("File \"js_json_test.res\", line 137, characters 22-29", (function (param) {
                return {
                        TAG: "Ok",
                        _0: false
                      };
              }));
      }
    }
  } else {
    add_test("File \"js_json_test.res\", line 140, characters 18-25", (function (param) {
            return {
                    TAG: "Ok",
                    _0: false
                  };
          }));
  }
} else {
  add_test("File \"js_json_test.res\", line 142, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

function eq_at_i(loc, json, i, kind, expected) {
  var ty = Js_json.classify(json);
  if (typeof ty !== "object") {
    return add_test(loc, (function (param) {
                  return {
                          TAG: "Ok",
                          _0: false
                        };
                }));
  }
  if (ty.TAG !== "JSONArray") {
    return add_test(loc, (function (param) {
                  return {
                          TAG: "Ok",
                          _0: false
                        };
                }));
  }
  var ty$1 = Js_json.classify(Caml_array.get(ty._0, i));
  switch (kind) {
    case "String" :
        if (typeof ty$1 !== "object") {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        } else if (ty$1.TAG === "JSONString") {
          return eq(loc, ty$1._0, expected);
        } else {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
    case "Number" :
        if (typeof ty$1 !== "object") {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        } else if (ty$1.TAG === "JSONNumber") {
          return eq(loc, ty$1._0, expected);
        } else {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
    case "Object" :
        if (typeof ty$1 !== "object") {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        } else if (ty$1.TAG === "JSONObject") {
          return eq(loc, ty$1._0, expected);
        } else {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
    case "Array" :
        if (typeof ty$1 !== "object") {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        } else if (ty$1.TAG === "JSONArray") {
          return eq(loc, ty$1._0, expected);
        } else {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
    case "Boolean" :
        if (typeof ty$1 === "object") {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
        switch (ty$1) {
          case "JSONFalse" :
              return eq(loc, false, expected);
          case "JSONTrue" :
              return eq(loc, true, expected);
          default:
            return add_test(loc, (function (param) {
                          return {
                                  TAG: "Ok",
                                  _0: false
                                };
                        }));
        }
    case "Null" :
        if (typeof ty$1 !== "object") {
          if (ty$1 === "JSONNull") {
            return add_test(loc, (function (param) {
                          return {
                                  TAG: "Ok",
                                  _0: true
                                };
                        }));
          } else {
            return add_test(loc, (function (param) {
                          return {
                                  TAG: "Ok",
                                  _0: false
                                };
                        }));
          }
        } else {
          return add_test(loc, (function (param) {
                        return {
                                TAG: "Ok",
                                _0: false
                              };
                      }));
        }
    
  }
}

var json$5 = JSON.parse(JSON.stringify($$Array.map((function (prim) {
                return prim;
              }), [
              "string 0",
              "string 1",
              "string 2"
            ])));

eq_at_i("File \"js_json_test.res\", line 199, characters 10-17", json$5, 0, "String", "string 0");

eq_at_i("File \"js_json_test.res\", line 200, characters 10-17", json$5, 1, "String", "string 1");

eq_at_i("File \"js_json_test.res\", line 201, characters 10-17", json$5, 2, "String", "string 2");

var json$6 = JSON.parse(JSON.stringify([
          "string 0",
          "string 1",
          "string 2"
        ]));

eq_at_i("File \"js_json_test.res\", line 208, characters 10-17", json$6, 0, "String", "string 0");

eq_at_i("File \"js_json_test.res\", line 209, characters 10-17", json$6, 1, "String", "string 1");

eq_at_i("File \"js_json_test.res\", line 210, characters 10-17", json$6, 2, "String", "string 2");

var a = [
  1.0000001,
  10000000000.1,
  123.0
];

var json$7 = JSON.parse(JSON.stringify(a));

eq_at_i("File \"js_json_test.res\", line 219, characters 10-17", json$7, 0, "Number", Caml_array.get(a, 0));

eq_at_i("File \"js_json_test.res\", line 220, characters 10-17", json$7, 1, "Number", Caml_array.get(a, 1));

eq_at_i("File \"js_json_test.res\", line 221, characters 10-17", json$7, 2, "Number", Caml_array.get(a, 2));

var a$1 = [
  0,
  -1347440721,
  -268391749
];

var json$8 = JSON.parse(JSON.stringify($$Array.map((function (prim) {
                return prim;
              }), a$1)));

eq_at_i("File \"js_json_test.res\", line 230, characters 10-17", json$8, 0, "Number", Caml_array.get(a$1, 0));

eq_at_i("File \"js_json_test.res\", line 231, characters 10-17", json$8, 1, "Number", Caml_array.get(a$1, 1));

eq_at_i("File \"js_json_test.res\", line 232, characters 10-17", json$8, 2, "Number", Caml_array.get(a$1, 2));

var a$2 = [
  true,
  false,
  true
];

var json$9 = JSON.parse(JSON.stringify(a$2));

eq_at_i("File \"js_json_test.res\", line 241, characters 10-17", json$9, 0, "Boolean", Caml_array.get(a$2, 0));

eq_at_i("File \"js_json_test.res\", line 242, characters 10-17", json$9, 1, "Boolean", Caml_array.get(a$2, 1));

eq_at_i("File \"js_json_test.res\", line 243, characters 10-17", json$9, 2, "Boolean", Caml_array.get(a$2, 2));

function make_d(s, i) {
  var d = {};
  d["a"] = s;
  d["b"] = i;
  return d;
}

var a$3 = [
  make_d("aaa", 123),
  make_d("bbb", 456)
];

var json$10 = JSON.parse(JSON.stringify(a$3));

var ty$6 = Js_json.classify(json$10);

if (typeof ty$6 !== "object") {
  add_test("File \"js_json_test.res\", line 271, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
} else if (ty$6.TAG === "JSONArray") {
  var ty$7 = Js_json.classify(Caml_array.get(ty$6._0, 1));
  if (typeof ty$7 !== "object") {
    add_test("File \"js_json_test.res\", line 269, characters 18-25", (function (param) {
            return {
                    TAG: "Ok",
                    _0: false
                  };
          }));
  } else if (ty$7.TAG === "JSONObject") {
    var ty$8 = Js_json.classify(option_get(Js_dict.get(ty$7._0, "a")));
    if (typeof ty$8 !== "object") {
      add_test("File \"js_json_test.res\", line 267, characters 20-27", (function (param) {
              return {
                      TAG: "Ok",
                      _0: false
                    };
            }));
    } else if (ty$8.TAG === "JSONString") {
      eq("File \"js_json_test.res\", line 266, characters 35-42", ty$8._0, "bbb");
    } else {
      add_test("File \"js_json_test.res\", line 267, characters 20-27", (function (param) {
              return {
                      TAG: "Ok",
                      _0: false
                    };
            }));
    }
  } else {
    add_test("File \"js_json_test.res\", line 269, characters 18-25", (function (param) {
            return {
                    TAG: "Ok",
                    _0: false
                  };
          }));
  }
} else {
  add_test("File \"js_json_test.res\", line 271, characters 16-23", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}

try {
  JSON.parse("{{ A}");
  add_test("File \"js_json_test.res\", line 279, characters 11-18", (function (param) {
          return {
                  TAG: "Ok",
                  _0: false
                };
        }));
}
catch (exn){
  add_test("File \"js_json_test.res\", line 281, characters 17-24", (function (param) {
          return {
                  TAG: "Ok",
                  _0: true
                };
        }));
}

eq("File \"js_json_test.res\", line 287, characters 12-19", JSON.stringify([
          1,
          2,
          3
        ]), "[1,2,3]");

eq("File \"js_json_test.res\", line 290, characters 2-9", JSON.stringify({
          foo: 1,
          bar: "hello",
          baz: {
            baaz: 10
          }
        }), "{\"foo\":1,\"bar\":\"hello\",\"baz\":{\"baaz\":10}}");

eq("File \"js_json_test.res\", line 295, characters 12-19", JSON.stringify(null), "null");

eq("File \"js_json_test.res\", line 297, characters 12-19", JSON.stringify(undefined), undefined);

eq("File \"js_json_test.res\", line 300, characters 5-12", Js_json.decodeString("test"), "test");

eq("File \"js_json_test.res\", line 301, characters 5-12", Js_json.decodeString(true), undefined);

eq("File \"js_json_test.res\", line 302, characters 5-12", Js_json.decodeString([]), undefined);

eq("File \"js_json_test.res\", line 303, characters 5-12", Js_json.decodeString(null), undefined);

eq("File \"js_json_test.res\", line 304, characters 5-12", Js_json.decodeString({}), undefined);

eq("File \"js_json_test.res\", line 305, characters 5-12", Js_json.decodeString(1.23), undefined);

eq("File \"js_json_test.res\", line 309, characters 5-12", Js_json.decodeNumber("test"), undefined);

eq("File \"js_json_test.res\", line 310, characters 5-12", Js_json.decodeNumber(true), undefined);

eq("File \"js_json_test.res\", line 311, characters 5-12", Js_json.decodeNumber([]), undefined);

eq("File \"js_json_test.res\", line 312, characters 5-12", Js_json.decodeNumber(null), undefined);

eq("File \"js_json_test.res\", line 313, characters 5-12", Js_json.decodeNumber({}), undefined);

eq("File \"js_json_test.res\", line 314, characters 5-12", Js_json.decodeNumber(1.23), 1.23);

eq("File \"js_json_test.res\", line 318, characters 5-12", Js_json.decodeObject("test"), undefined);

eq("File \"js_json_test.res\", line 319, characters 5-12", Js_json.decodeObject(true), undefined);

eq("File \"js_json_test.res\", line 320, characters 5-12", Js_json.decodeObject([]), undefined);

eq("File \"js_json_test.res\", line 321, characters 5-12", Js_json.decodeObject(null), undefined);

eq("File \"js_json_test.res\", line 322, characters 5-12", Js_json.decodeObject({}), {});

eq("File \"js_json_test.res\", line 323, characters 5-12", Js_json.decodeObject(1.23), undefined);

eq("File \"js_json_test.res\", line 327, characters 5-12", Js_json.decodeArray("test"), undefined);

eq("File \"js_json_test.res\", line 328, characters 5-12", Js_json.decodeArray(true), undefined);

eq("File \"js_json_test.res\", line 329, characters 5-12", Js_json.decodeArray([]), []);

eq("File \"js_json_test.res\", line 330, characters 5-12", Js_json.decodeArray(null), undefined);

eq("File \"js_json_test.res\", line 331, characters 5-12", Js_json.decodeArray({}), undefined);

eq("File \"js_json_test.res\", line 332, characters 5-12", Js_json.decodeArray(1.23), undefined);

eq("File \"js_json_test.res\", line 336, characters 5-12", Js_json.decodeBoolean("test"), undefined);

eq("File \"js_json_test.res\", line 337, characters 5-12", Js_json.decodeBoolean(true), true);

eq("File \"js_json_test.res\", line 338, characters 5-12", Js_json.decodeBoolean([]), undefined);

eq("File \"js_json_test.res\", line 339, characters 5-12", Js_json.decodeBoolean(null), undefined);

eq("File \"js_json_test.res\", line 340, characters 5-12", Js_json.decodeBoolean({}), undefined);

eq("File \"js_json_test.res\", line 341, characters 5-12", Js_json.decodeBoolean(1.23), undefined);

eq("File \"js_json_test.res\", line 345, characters 5-12", Js_json.decodeNull("test"), undefined);

eq("File \"js_json_test.res\", line 346, characters 5-12", Js_json.decodeNull(true), undefined);

eq("File \"js_json_test.res\", line 347, characters 5-12", Js_json.decodeNull([]), undefined);

eq("File \"js_json_test.res\", line 348, characters 5-12", Js_json.decodeNull(null), null);

eq("File \"js_json_test.res\", line 349, characters 5-12", Js_json.decodeNull({}), undefined);

eq("File \"js_json_test.res\", line 350, characters 5-12", Js_json.decodeNull(1.23), undefined);

function id(obj) {
  return Js_json.deserializeUnsafe(Js_json.serializeExn(obj));
}

function idtest(obj) {
  eq("File \"js_json_test.res\", line 355, characters 23-30", obj, Js_json.deserializeUnsafe(Js_json.serializeExn(obj)));
}

idtest(undefined);

idtest({
      hd: [
        undefined,
        undefined,
        undefined
      ],
      tl: /* [] */0
    });

idtest(Belt_List.makeBy(500, (function (i) {
            if (i % 2 === 0) {
              return ;
            } else {
              return 1;
            }
          })));

idtest(Belt_Array.makeBy(500, (function (i) {
            if (i % 2 === 0) {
              return ;
            } else {
              return 1;
            }
          })));

Mt.from_pair_suites("Js_json_test", suites.contents);

var J;

exports.suites = suites;
exports.J = J;
exports.add_test = add_test;
exports.eq = eq;
exports.false_ = false_;
exports.true_ = true_;
exports.option_get = option_get;
exports.eq_at_i = eq_at_i;
exports.id = id;
exports.idtest = idtest;
/* v Not a pure module */
